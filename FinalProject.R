library(ggplot2) #ggplotting
library(Amelia) #for missmap
library(mlbench) #machine learning bench for fit and predict
library(stats) # for glm 
library(rpart) # need to install adabag
library(adabag) # for boosted tree
library(MLmetrics) # for log-loss
library(tidyverse) #tidyverse for tidying
library(forcats)#for factoring data
library(plyr)#for condensing data
library(randomForest) #random forest
library(earth) # for Mars
library(xgboost) # for xgboost
library(caret) # for EDA
library(tuneRanger) # for tune random forest
library(mlr) # for Performance measure to evaluate/optimize. It works with tuneRanger
library(keras)# for neural networks
library(neuralnet)# for neural networks
library(gains) # gain and lift
library(wmtsa) # D stats
#library(psych)
#library(rpart)
#library(party)
#library(partykit)
library(corrgram) # for correlogram
library(gridExtra) # for grid.arrange
library(pacman)# for loading for function
library(sfsmisc)#for boxplot matrix

trainer = read.csv("ElTrain.csv",header=TRUE) # load train data
trainer$GCTrain = as.factor(trainer$GCTrain)#making actual y factor
target = as.factor(trainer$GCTrain)
predict = trainer %>% select(-c(AzTrain, rNThrTrain))
predict2 = trainer %>% select(-c(AzTrain, rNThrTrain,GCTrain))

## tune using tools that come with the algorithm. Can be used later
trainer%>% select(-c(trainer$GCTrain)) -> Predictors1
Predictors = as.matrix(Predictors1)
trainer2rf = as.data.frame(trainer)

# bestmtry <- tuneRF(Predictors, trainer2$readmitted, stepFactor=1.5, improve=1e-5, ntree=2500)
# print(bestmtry)

readmitted.task = makeClassifTask(data = predict, target = "GCTrain") 
# Estimate runtime
estimateTimeTuneRanger(readmitted.task)
# Approximated time for tuning: 7M 56S
#
      # Tuning
      res = tuneRanger(readmitted.task, measure = list(logloss), num.trees = 1950, iters = 100, iters.warmup = 30) # default tune.parameters = c("mtry", "min.node.size", "sample.fraction"), multiclass.brier instead of logloss
      # Mean of best 5 % of the results
      res

# Model with the new tuned hyperparameters
res$model
###
# Prediction
#
# with tuned hyperparameters ("mtry", "min.node.size", "sample.fraction") using tuneRanger: 
# for confusionMatrix
predict_randomForest = predict(res$model, newdata = predict, type = "prob")
predReadmit = predict_randomForest$data$prob.1
class_prediction <- as.factor(ifelse(predReadmit>0.12,
                                     1,
                                     0))

caret::confusionMatrix(class_prediction,as.factor(trainer$GCTrain))
#for D-stat
predD <-D.statistic(as.numeric(class_prediction))
#checking logloss
LogLoss(as.numeric(class_prediction), as.numeric(trainer$GCTrain))


# plot correlogram
  
correlogram_trainer <- corrgram::corrgram(predict, order=NULL, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

# Return high correlations (1 > |ij| > 0.001)
  hi_corr <- data.frame()
  m <- sqrt(length(correlogram_trainer))
  for (i in 1:m){
    for (j in 1:m){
      if ( !is.na(correlogram_trainer[i,j])){
        if ( abs(correlogram_trainer[i,j]) > 0.001 && correlogram_trainer[i,j] < 1) {
          hi_corr[i,1] <- c(paste(rownames(correlogram_trainer)[i], "-" , colnames(correlogram_trainer)[j]))
          hi_corr[i,2] <- c(paste(correlogram_trainer[i,j]))
        }
      }
    }
  }
  hi_corr <- na.omit(hi_corr) # Omit NAs
  dupes <- duplicated(x=hi_corr$V2) # Select Duplicates
  hi_corr <- data.frame(hi_corr$V1[dupes == FALSE], hi_corr$V2[dupes == FALSE]) #Remove duplicates
  colnames(hi_corr) <- c("Predictors", "Correlation")
  #correlation values
  hi_corr
####
train_pairs = trainer %>% select(-c( AzTrain, rNThrTrain))
#scatter plot of the variables
train_pairs %>% pairs()
#histogram of the variables
g2= ggplot(data=train_pairs, mapping = aes(train_pairs$PhiDPTrain))+geom_histogram()+labs(x="PhiDPTrain")
g3= ggplot(data=train_pairs, mapping = aes(train_pairs$SpectrumWidthTrain))+geom_histogram()+labs(x="SpectrumWidthTrain")
g4= ggplot(data=train_pairs, mapping = aes(train_pairs$RhoHVTrain))+geom_histogram()+labs(x="RhoHVTrain")
g5= ggplot(data=train_pairs, mapping = aes(train_pairs$ZdrTrain))+geom_histogram()+labs(x="ZdrTrain")
g6 = ggplot(data=train_pairs, mapping = aes(train_pairs$ZhTrain))+geom_histogram()+labs(x="ZhTrain")


grid.arrange(g2,g3,g4,g5,g6,  bottom = "Original distributions of PhiDPTrain, SpectrumWidthTrain, RhoHVTrain, ZdrTrain, and ZhTrain")


#function for giving a lot of different statistics/ summary of results
thisisafunction <- function(model, data,predictedvalues,actualvalues){
  par(mfrow=c(2,2))
  #ROC curve
  pacman::p_load(caret, ROCR, DescTools,wmtsa,MLmetrics,ggplot2,gains)
  pred<- prediction(predictedvalues,actualvalues)
  perf<- performance(pred,"tpr","fpr")
  plot(perf,colorize=TRUE, print.cutoffs.at = c(0.5),main="ROC curve")
  abline(0,1,col="red")
  cdp <- ConDisPairs(cbind.data.frame(ifelse(predictedvalues>0.5, 1, 0), ifelse(actualvalues=="1", 1, 0)))[c("C","D")]
  paste("Concordant Pairs", cdp)
  predD <-D.statistic(predictedvalues)
  paste("predicted D-stat", predD)
  actD <- D.statistic(as.numeric(actualvalues))
  paste("actual D-stat", actD)
  ll <- LogLoss(predictedvalues, as.numeric(actualvalues))
  paste("Log Loss: ", ll)
  ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
  plot(perf,main=paste0(' KS =',round(ks*100,1),'%'))
  lines(x = c(0,1),y=c(0,1))
  paste("K-S Statistic:", ks);
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  paste("AUC: ",auc)
  plot(0,0,type="n", xlim= c(0,1), ylim=c(0,400),     
       xlab="Prediction", ylab="Density",  
       main="Distribution of predicted TP and TN probs.")
  
  for (runi in 1:length(pred@predictions)) {
    lines(density(pred@predictions[[runi]][pred@labels[[runi]]==1]), col= "blue")
    lines(density(pred@predictions[[runi]][pred@labels[[runi]]==0]), col="green")
  }
  # Cumulative Gains Chart
  gain = performance(pred, "tpr", "rpp")
  plot(gain, col="orange", lwd=2, main="Cumulative Gains Chart")
  concordant_pairs <- cdp
  predicted_dstat <- predD
  actual_dstat <- actD
  log_loss <- ll
  k_statistic <- ks
  area_under_curve <- auc
  output <- data.frame(concordant_pairs, predicted_dstat, actual_dstat, log_loss, k_statistic, area_under_curve)
  return(output)
}


    thisisafunction(res$model, predict,predReadmit,as.factor(trainer$GCTrain))
LogLoss(predict_randomForest$data$prob.1,as.numeric(trainer$GCTrain))
boxer = na.omit(predict)
pairs(boxer)
matboxer <- as.matrix(boxer$PhiDPTrain)
#Boxplots of data 
par(mfrow = c(2,3))
boxplot(boxer$PhiDPTrain, main="PhiDPTrain")write.csv(submission, "RF.csv",row.names = FALSE)
boxplot(boxer$SpectrumWidthTrain, main="SpectrumWidthTrain")
boxplot(boxer$RhoHVTrain, main="RhoHVTrain")
boxplot(boxer$ZdrTrain, main="ZdrTrain")
boxplot(boxer$ZhTrain,main="ZHTrain")


#Starting a KS graph by decile
KS<-data.frame(Group=numeric(10),
               CumPct0=numeric(10),
               CumPct1=numeric(10),
               Dif=numeric(10))
myvals<-cut(predReadmit,seq(1,0,-.1),include.lowest=T)

xtab<-table(myvals,trainer$GCTrain)
xtab
#fill data frame with information: Group ID, 
#Cumulative % of 0's, of 1's and Difference
for (i in 1:10) {
  KS$Group[i]<-i
  KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
  KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
  KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
}

KS  

KS[KS$Dif==max(KS$Dif),]

maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]
#making KS plot
ggplot(data=KS)+
  geom_line(aes(Group,CumPct0),color="blue")+
  geom_line(aes(Group,CumPct1),color="red")+
  geom_segment(x=maxGroup,xend=maxGroup,
               y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
  labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")




#Doing a gains chart by itself
# pred<- prediction(predReadmit,as.factor(trainer$GCTrain))
# gain = performance(pred, "tpr", "rpp")
# plot(gain, col="orange", print.cutoffs.at = c(0.12,0.15,0.1), lwd=2, main="Cumulative Gains Chart")
# perf= performance(pred,"tpr","fpr")
# par(mfrow=c(1,1))
#lines(as.numeric(unlist(perf@x.values)),as.numeric(unlist(perf@y.values)),colorize=TRUE, print.cutoffs.at = c(0.12,0.15,0.1),main="ROC curve")
#doing an ROC curve by itself
#perf<- performance(pred,"tpr","fpr")
#plot(perf,colorize=TRUE, print.cutoffs.at = c(0.12,0.15,0.1),main="ROC curve")



# validation data check

tester = read.csv("ElTest.csv",header=TRUE) # load test data
valpredict = tester %>% select(-c(AzTrain, rNThrTrain))
valtarget = as.factor(tester$GCTrain)

valpredict_randomForest = predict(res$model,newdata= valpredict, type = "prob")
valpredReadmit = valpredict_randomForest$data$prob.1
valclass_prediction <- as.factor(ifelse(valpredReadmit>0.13,
                                        1,
                                        0))


caret::confusionMatrix(valclass_prediction,as.factor(tester$GCTrain))
predD <-D.statistic(as.numeric(valclass_prediction))
LogLoss(as.numeric(valclass_prediction), as.numeric(tester$GCTrain))

#Making a 50-50 split in examples

TrainerCor1<- trainer[trainer$GCTrain ==1,]
TrainerCor0<- trainer[trainer$GCTrain ==0,]
TesterCor1<- tester[tester$GCTrain==1,]
TesterCor0<- tester[tester$GCTrain==0,]


sample0 <- sample_n(TrainerCor0,113)
test0 <- sample_n(TesterCor0,62)
halfdata<- rbind(sample0,TrainerCor1)
halfdata2<- rbind(test0,TesterCor1)

readmitted.task = makeClassifTask(data = halfdata, target = "GCTrain") 
# Estimate runtime
estimateTimeTuneRanger(readmitted.task)
# Approximated time for tuning: 7M 56S
#
# Tuning
res = tuneRanger(readmitted.task, measure = list(logloss), num.trees = 2000, iters = 100, iters.warmup = 30) # default tune.parameters = c("mtry", "min.node.size", "sample.fraction"), multiclass.brier instead of logloss

valpredict_randomForest = predict(res$model,newdata= halfdata, type = "prob")
valpredReadmit = valpredict_randomForest$data$prob.1
valclass_prediction <- as.factor(ifelse(valpredReadmit>0.5,
                                        1,
                                        0))


caret::confusionMatrix(valclass_prediction,as.factor(halfdata$GCTrain))


LogLoss(valpredReadmit,as.numeric(halfdata$GCTrain))
predD <-D.statistic(as.numeric(valpredReadmit))

thisisafunction(res$model, halfdata,valpredReadmit,as.factor(halfdata$GCTrain))


testpredict_randomForest = predict(res$model,newdata= tester, type = "prob")

predD <-D.statistic(as.numeric(testpredReadmit))
testpredReadmit = testpredict_randomForest$data$prob.1
LogLoss(testpredReadmit,as.numeric(tester$GCTrain))


