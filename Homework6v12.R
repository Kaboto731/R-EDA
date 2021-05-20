library(ggplot2) #ggplotting
library(mlbench) #machine learning bench for fit and predict
library(EnvStats)
#library(VIM)
#library(mice)
library(tidyverse) #tidyverse for tidying
library(backports)
library(AppliedPredictiveModeling)#for predict?
library(caret)
library(forcats)#for factoring data
library(plyr)#for condensing data
#library(reshape2)
#library(DAAG)
library(broom)# for glancing at our data
library(ggfortify)#plot for mars residuals 
library(elasticnet)#lasso
library(glmnet)#elasticnet
library(randomForest)#random forest
library(pls)#partial least squares
library(adabag)#boosted tree method
library(earth)#Mars
library(rgl)#rotating graphics library
library(MASS)#for M-estimation method of robust regression
library(prediction)#for prediction of rlm
library(e1071)#for svm
library(BAS)#for bayesian regression
library(deepboost)#for deepboost
library(brnn)#for Bayesian regularized neural networks
library(ModelMetrics)#for RMSE
library(tuneRanger) # for tune random forest
library(mlr) # for Performance measure to evaluate/optimize. It works with tuneRanger

###
# set up the program
pick_up_tester =0 # if you work only train the model pick_up_tester = 0, if you are ready to submit, pick_up_tester = 1
#
work_df = 3 # 0 # the same dataframe as was in HW5 + some new levels of collapsed factorial variables
            # 1 # the same as was for work_df == 0, but pageviews=log(pageviews+14.8)
            # 2 # the same as in HW5, but all new variables from 1 are included
            # 3 # the same as 2, but without LogPageviewsDivTimesince because it is high correlated with timesince
#
submission=6 #0 for lm 
             #1 for lasso 
             #2 for randomforest 
             #3 for PLS
             #4 for MARS 
             #5 for elastic net
             #6 for Bayes neural net 
###
if (pick_up_tester == 1) {
  #reading in test file
  trainer = read.csv('Test.csv',header=TRUE)
} else {
  #reading in the trainer file
  trainer = read.csv("Train.csv", header = TRUE)
}

###
# collapsing the variables
#
# channelGrouping 
mycol = fct_collapse(trainer$channelGrouping, chan_Gr_meanRev13  = c("Direct","Display","Social","Affiliates","(Other)","Referral"))
#
# browser
trainer %>% dplyr::mutate(browser = forcats::fct_lump(fct_explicit_na(browser), n = 1))  -> trainer
#
# operatingSystem
 trainer %>% dplyr::mutate(operatingSystem = forcats::fct_lump(fct_explicit_na(operatingSystem), n = 3))  -> trainer
#
# deviceCategory
 trainer %>% dplyr::mutate(deviceCategory = forcats::fct_lump(fct_explicit_na(deviceCategory), n = 1)) -> trainer
#
# continent
trainer %>% dplyr::mutate(continent = forcats::fct_lump(fct_explicit_na(continent), n = 1)) -> trainer
#
# country
trainer %>% dplyr::mutate(country = forcats::fct_lump(fct_explicit_na(country), n = 4))  -> trainer
#
# source
trainer %>% dplyr::mutate(source = forcats::fct_lump(fct_explicit_na(source), n = 4))  -> trainer
###

###
# Setting the data frame
#
# creating columns where we are writing collapsed variables 
#   (one column per variable)
mycol3=trainer$operatingSystem
mycol4=trainer$browser
mycol5=trainer$continent
mycol6=trainer$country
mycol7=trainer$source
#
# create data frames and put the columns there
df  <- as.data.frame(mycol)
df3 <- as.data.frame(mycol3)
df4 <- as.data.frame(mycol4)
df5 <- as.data.frame(mycol5)
df6 <- as.data.frame(mycol6)
df7 <- as.data.frame(mycol7)
#
#setting data frame with tester
#
#one hot encoding the factors: create a new matrix mymat that concicts of 
#   the columns named as the levels in factorial variables
#   numbers of rows equal to the numbers of the observation or sessions. 
#   If in session 10, for example, channelGrouping had chan_Gr_meanRev13 
#   it will put 1 in the column chan_Gr_meanRev13 and 0 in all others
mymat<-model.matrix(~ . + 0, data=df, contrasts.arg = lapply(df, contrasts, contrasts=FALSE))
mymat3<-model.matrix(~ . + 0, data=df3, contrasts.arg = lapply(df3, contrasts, contrasts=FALSE))
mymat4<-model.matrix(~ . + 0, data=df4, contrasts.arg = lapply(df4, contrasts, contrasts=FALSE))
mymat5<-model.matrix(~ . + 0, data=df5, contrasts.arg = lapply(df5, contrasts, contrasts=FALSE))
mymat6<-model.matrix(~ . + 0, data=df6, contrasts.arg = lapply(df6, contrasts, contrasts=FALSE))
mymat7<-model.matrix(~ . + 0, data=df7, contrasts.arg = lapply(df7, contrasts, contrasts=FALSE))
#
# importing mymat, mymat3, etc. as data frame
mydf<- as.data.frame(mymat)
mydf4<-as.data.frame(mymat4)
mydf5<-as.data.frame(mymat5)
mydf6<-as.data.frame(mymat6)
mydf7<-as.data.frame(mymat7)
mydf3<-as.data.frame(mymat3)
mydft<- as.data.frame(mymatt)
#

# making more columns in trainer
#Fixing NA values
trainer <- trainer
trainer[trainer==""]=NA
trainer$mycol <- mycol
trainer$onebig<-mydf$mycolchan_Gr_meanRev13 
trainer$Osearch<-mydf$`mycolOrganic Search`
trainer$Psearch<-mydf$`mycolPaid Search`
trainer$colAndroid<-mydf3$mycol3Android
trainer$colWindows<-mydf3$mycol3Windows
trainer$colOSOther<-mydf3$mycol3Other
trainer$colMac<-mydf3$mycol3Macintosh
trainer$colBother <-mydf4$mycol4Other
trainer$colChrome <-mydf4$mycol4Chrome
trainer$colAmerica<-mydf5$mycol5Americas
trainer$colUK<-mydf6$`mycol6United Kingdom`
trainer$colSgoogle<-mydf7$mycol7mall.googleplex.com
trainer$colSyoutube<-mydf7$mycol7youtube.com

if (pick_up_tester == 1) {
  # for test
  # 
  if(work_df == 0){
  # the same dataframe as was in HW5 + some new levels of collapsed factorial variables
  custidRev<-ddply(trainer,.(custId),summarize,onebig=sum(onebig,na.rm=TRUE),
                   bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                   colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                   osearch=sum(Osearch,na.rm=TRUE), timesince=mean(timeSinceLastVisit,na.rm=TRUE),
                   visitnum= sum(visitNumber,na.rm=TRUE ),
                   pageviews=sum(pageviews,na.rm=TRUE), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
                   }
  #
  if(work_df == 1){
  # the same as was for work_df == 0, but pageviews=log(pageviews+14.8)
  custidRev<-ddply(trainer,.(custId),summarize,onebig=sum(onebig,na.rm=TRUE),
                   bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                   colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                   osearch=sum(Osearch,na.rm=TRUE), timesince=mean(timeSinceLastVisit,na.rm=TRUE),
                   visitnum= sum(visitNumber,na.rm=TRUE ),
                   pageviews=log(sum(pageviews,na.rm=TRUE)+14.8), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE))     
  }
  #
  if(work_df == 2){
    # the same as in HW5, but all new variables from 1 are included
    custidRev <-ddply(trainer,.(custId), summarize, onebig=sum(onebig,na.rm=TRUE),
                       bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                       colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                       osearchDivVisitnum=sum(Osearch,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE ), 
                       LogPageviewsDivTimesince=log(sum(pageviews,na.rm=TRUE)/(mean(timeSinceLastVisit,na.rm=TRUE)+10^(-300))+0.5),
                       timesince=log(mean(timeSinceLastVisit,na.rm=TRUE)+0.5), 
                       LogPageviewsDivvisitNumber=log(sum(pageviews,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE) + 0.005),
                       pageviews=log(sum(pageviews,na.rm=TRUE)+14.8), isMobile=median(isMobile,na.rm=TRUE),
                       isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
  }
  #
  if(work_df == 3){
    # the same as 2, but without LogPageviewsDivTimesince because it is high correlated with timesince  
    custidRev <-ddply(trainer,.(custId), summarize, onebig=sum(onebig,na.rm=TRUE),
                       bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                       colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                       osearchDivVisitnum=sum(Osearch,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE ), 
                       timesince=log(mean(timeSinceLastVisit,na.rm=TRUE)+0.5), 
                       LogPageviewsDivvisitNumber=log(sum(pageviews,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE) + 0.005),
                       pageviews=log(sum(pageviews,na.rm=TRUE)+14.8), isMobile=median(isMobile,na.rm=TRUE),
                       isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
  }
  
  
  #storing the custID as its own row
  custId<-custidRev$custId
  
  
  if(submission==0){
    
    #prediction with the test data
    predRevenuelm<-predict(fit,custidRev)
    predRevenue<-predict(fit,custidRev)
    #removing all negative values with 0
    predRevenue[predRevenue<0]<-0
    predRevenue<-as.data.frame(predRevenue)
    sum(is.na(predRevenue))
    #storing in data frame
    mydata <- data.frame(custId, predRevenue)
    
    colnames(mydata) <- c("custId", "predRevenue")
    #saving the file to a CSV
    write.csv(mydata,file="PandaOLS.csv",row.names = FALSE)
    
  }
  if(submission==1){
    
    x<-as.matrix(custidRev[,2:14])
    predRevenuel <- predict(lasso_best, s = best_lam, newx = x)
    predRevenue<-predict(lasso_best, s = best_lam, newx = x)
    #removing all negative values with 0
    predRevenue[predRevenue<0]<-0
    predRevenue<-as.data.frame(predRevenue)
    sum(is.na(predRevenue))
    #storing in data frame
    mydata <- data.frame(custId, predRevenue)
    
    colnames(mydata) <- c("custId", "predRevenue")
    #saving the file to a CSV
    write.csv(mydata,file="Pandalasso.csv",row.names = FALSE)
    
  }
  if(submission==2){
    
    predRevenue<-predict(res$model, newdata=custidRev)
    # #removing all negative values with 0
     predRevenue[predRevenue<0]<-0
    predRevenue<-as.data.frame(predRevenue)
    sum(is.na(predRevenue))
    #storing in data frame
    mydata <- data.frame(custId, predRevenue)
    
    colnames(mydata) <- c("custId", "predRevenue")
    #saving the file to a CSV
    write.csv(mydata,file="Pandaforest.csv",row.names = FALSE)
    
  }
  if (submission==3){
    
  predRevenue<-predict(pls.fit,custidRev)
    
  predRevenue<-as.data.frame(predRevenue)
  #removing all negative values with 0
  predRevenue[predRevenue<0]<-0
  sum(is.na(predRevenue))
  #storing in data frame
  mydata <- data.frame(custId, predRevenue)
  
  colnames(mydata) <- c("custId", "predRevenue")
  #saving the file to a CSV
  write.csv(mydata,file="Pandapls.csv",row.names = FALSE)
    
  }
  
  if(submission==4){
    
    x<-as.matrix(custidRev[,2:14])
    MARSpredRevenue <- predict(mars_fit, x)
    predRevenue<-predict(mars_fit, x)
    #removing all negative values with 0
    predRevenue[predRevenue<0]<-0
    predRevenue<-as.data.frame(predRevenue)
    sum(is.na(predRevenue))
    #storing in data frame
    mydata <- data.frame(custId, predRevenue)
    
    colnames(mydata) <- c("custId", "predRevenue")
    #saving the file to a CSV
    write.csv(mydata,file="PandaMars.csv",row.names = FALSE)
    
  }
  if (submission==5){
    x<-as.matrix(custidRev[,2:14])
    predRevenueelastic <- predict(elastic_best, s = best_lam, newx = x)
    predRevenue<-predict(elastic_best, s = best_lam, newx = x)
    predRevenue<-as.data.frame(predRevenue)
    sum(is.na(predRevenue))
    #removing all negative values with 0
    predRevenue[predRevenue<0]<-0
    #storing in data frame
    mydata <- data.frame(custId, predRevenue)
    
    colnames(mydata) <- c("custId", "predRevenue")
    #saving the file to a CSV
    write.csv(mydata,file="Pandaenet.csv",row.names = FALSE)
  }

  if (submission==6){
    predRevenueBRNN <- predict(BRNNtunedpanda,custidRev)
    predRevenue<-predRevenueBRNN
    predRevenue<-as.data.frame(predRevenue)
    sum(is.na(predRevenue))
    #removing all negative values with 0
    predRevenue[predRevenue<0]<-0
    #storing in data frame
    mydata <- data.frame(custId, predRevenue)
    
    colnames(mydata) <- c("custId", "predRevenue")
    #saving the file to a CSV
    write.csv(mydata,file="PandaBRNN.csv",row.names = FALSE)
  }
  
  
}else {
  # for train 
  #
  #creating the dataframe with which all model are working 
  if(work_df == 0){
  # the same dataframe as was in HW5 + some new levels of collapsed factorial variables
  custidRev<-ddply(trainer,.(custId),revenew=log(sum(revenue,na.rm=TRUE)+1),summarize,onebig=sum(onebig,na.rm=TRUE),
                   bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                   colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                   osearch=sum(Osearch,na.rm=TRUE), timesince=mean(timeSinceLastVisit,na.rm=TRUE),
                   visitnum= sum(visitNumber,na.rm=TRUE ),
                   pageviews=sum(pageviews,na.rm=TRUE), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
                   }
  if(work_df == 1){
  # the same as was for work_df == 0, but pageviews=log(pageviews+14.8)
  custidRev<-ddply(trainer,.(custId),revenew=log(sum(revenue,na.rm=TRUE)+1), summarize,onebig=sum(onebig,na.rm=TRUE),
                   bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                   colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                   osearch=sum(Osearch,na.rm=TRUE), timesince=mean(timeSinceLastVisit,na.rm=TRUE),
                   visitnum= sum(visitNumber,na.rm=TRUE ),
                   pageviews=log(sum(pageviews,na.rm=TRUE)+14.8), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE))     
                  }
  if(work_df == 2){
  # the same as in HW5, but all new variables from 1 are included
  custidRev1 <-ddply(trainer,.(custId), revenew=log(sum(revenue,na.rm=TRUE)+1), summarize, onebig=sum(onebig,na.rm=TRUE),
                   bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                   colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                   osearchDivVisitnum=sum(Osearch,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE ), 
                   LogPageviewsDivTimesince=log(sum(pageviews,na.rm=TRUE)/(mean(timeSinceLastVisit,na.rm=TRUE)+10^(-300))+0.5),
                   timesince=log(mean(timeSinceLastVisit,na.rm=TRUE)+0.5), 
                   LogPageviewsDivvisitNumber=log(sum(pageviews,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE) + 0.005),
                   pageviews=log(sum(pageviews,na.rm=TRUE)+14.8), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
  # variables for modeling
  Target = as.numeric(custidRev1[,2])
  Predictors = as.matrix(custidRev1[,3:16])
  # dataframe without custId
  custidRev = as.data.frame(custidRev1[,2:16]) # revenew is a target
  #
  # check correlation
  cor(custidRev) # LogPageviewsDivTimesince vs  Timesince  -0.97028558; rev vs Log = -0.46131499; rev vs Timesince = 0.48154563
  }
  #
  if(work_df == 3){
    # the same as 2, but without LogPageviewsDivTimesince because it is high correlated with timesince  
    custidRev1 <-ddply(trainer,.(custId), revenew=log(sum(revenue,na.rm=TRUE)+1), summarize, onebig=sum(onebig,na.rm=TRUE),
                       bother=sum(colBother,na.rm=TRUE), colandroid=sum(colAndroid,na.rm=TRUE), colamericas=sum(colAmerica,na.rm=TRUE),
                       colunited=sum(colUK,na.rm=TRUE), youtube=sum(colSyoutube,na.rm=TRUE), google=sum(colSgoogle,na.rm=TRUE),
                       osearchDivVisitnum=sum(Osearch,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE ), 
                       timesince=log(mean(timeSinceLastVisit,na.rm=TRUE)+0.5), 
                       LogPageviewsDivvisitNumber=log(sum(pageviews,na.rm=TRUE)/sum(visitNumber,na.rm=TRUE) + 0.005),
                       pageviews=log(sum(pageviews,na.rm=TRUE)+14.8), isMobile=median(isMobile,na.rm=TRUE),
                       isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
    # variables for modeling
    Target = as.numeric(custidRev1[,2])
    Predictors = as.matrix(custidRev1[,3:15])
    # dataframe without custId
    custidRev = as.data.frame(custidRev1[,2:15]) # revenew is a target
  }

            
  #fitting the data
  if(submission==0){
    # lm
    fit<-lm(data=custidRev,revenew ~.) # 
    #predicting with the fit 
    mypredictor<-predict(fit,custidRev)
    #checking the NA's
    sum(is.na(mypredictor))
    # root mean square error
    RMSE(mypredictor,custidRev$revenew)
    #checking with cross validation
    train.control <- trainControl(method = "cv", number = 10)
    #looking at our coefficients
    glance(fit)
    tidy(fit)
    # summary of some p-values: 
    # 
    #  work_df = 2: the same as in HW5, but all new variables from 1 are included
    # LogPageviewsDivTimesince  1.08e-  3 
    # timesince                 9.22e-  3
    
    
    #creating our cross validations hyperparameters
    model <- caret::train(revenew~., data = custidRev, method = "lm",
                 trControl = train.control)
    #printing our CV's results
    print(model)
  }
  if (submission==1){
  #working with the lasso
  y<-Target
  x<-Predictors
  
#  model.lasso<-lars(x,y,type='lasso')
#  plot(model.lasso)
#  model.lasso$lambda
#  summary(model.lasso)
#  lambda.lasso<-c(model.lasso$lambda,0)
#  beta <-coef(model.lasso)
  
#  str(model.lasso)
  
#  colors <- rainbow(13)
  
 # matplot(lambda.lasso,beta,type="o",pch=20,xlab = expression(lambda),ylab=expression(hat(beta)))
#  text(rep(-0,9),beta[9,],colnames(x),pos=4)
#  abline(v=lambda.lasso[4],lty=2)
#  abline(h=0, lty=2)
  
#  beta.lasso<-beta[4,]
#  resid.lasso<-custidRev$revenew-predict(model.lasso,as.matrix(custidRev[,3:15]),s=4,type="fit")$fit
#  rss.lasso<-sum(resid.lasso^2)/(67-4)
  
#  cvlas<-cv.lars(x,y,type="lasso",mode="fraction")
 # cvlas
 # opt.frac <- min(cvlas$cv) + sd(cvlas$cv) 
#  opt.frac <- cvlas$index[which(cvlas$cv < opt.frac)[1]]
#  lasso.path <- lars(x, y, type = "lasso")
#  summary(lasso.path)
#  lasso.fit <- predict.lars(lasso.path, type = "coefficients", mode = "fraction", s = opt.frac)
#  coef(lasso.fit)
 #  library(glmnet)
#TUNING LAMBDA
  lambda_seq <- 10^seq(2, -2, by = -.1)
  cv_output <- cv.glmnet(x, y, 
                         alpha = 1, lambda = lambda_seq)
  
  plot(cv_output)
  cv_output
  #RMSE of our problem
  sqrt(cv_output$cvm[cv_output$lambda == cv_output$lambda.min])
  #R^2 of our problem
  r2 <- cv_output$glmnet.fit$dev.ratio[which(cv_output$glmnet.fit$lambda == cv_output$lambda.min)]
  best_lam<-cv_output$lambda.min
  # running model with tuned lambda
  lasso_best <- glmnet(x, y, alpha = 1, lambda = best_lam)
 
  
  }
  if (submission==2){
    #random forest
    #
    ###
    # tune rf 
    # 
    # generate seed for random forest
    seed=123
    # set as our seed for randomization of random forest.
    set.seed(seed)
    #
    # ## tune using tools that come with the algorithm. Can be used later
    # bestmtry <- tuneRF(Predictors, Target, stepFactor=1.5, improve=1e-5, ntree=1500)
    # print(bestmtry)
    ##
    #
    ## tune with tuneRanger
    # A mlr task has to be created in order to use the package
 #   Revenew.task = makeRegrTask(data = custidRev, target = "revenew")  # revenew is a target
    #
    # Estimate runtime
#    estimateTimeTuneRanger(Revenew.task)
    # Approximated time for tuning: 1H 21M 22S
    #
    # Tuning
#    res = tuneRanger(Revenew.task, measure = list(rmse), num.trees = 2500,
 #                    num.threads = 2, iters = 70, iters.warmup = 30) # default tune.parameters = c("mtry", "min.node.size", "sample.fraction")
    # Mean of best 5 % of the results
 #   res
    # Model with the new tuned hyperparameters
#    res$model
    ###
    # Prediction
    #
    # with tuned hyperparameters ("mtry", "min.node.size", "sample.fraction") using tuneRanger: 
 #   predict_randomForest = predict(res$model, newdata = custidRev)
    #Random forest cross validation
    rf.cv <- rfcv(Predictors,Target,cv.fold = 10, trees=1500)
    #plot(rf_random)
    #dotplot(results)
  rf.cv$error.cv
  rf.cv$n.var
  rf.cv$predicted

  }
  if (submission==3){
    #looking at best values with cross val
    ctrl = trainControl(method="repeatedcv",number=10,repeats=10)
    pls.fit= caret::train(revenew~.,data=custidRev,method="pls",trControl=ctrl,tuneLength=20)
    plot(pls.fit)
    #taking best tune
    pls.fit$bestTune
    PartLeastSqr<-plsr(revenew~.,data=custidRev,method="oscorespls",validation="CV")
    summary(PartLeastSqr)
    pls.fit<- caret::train(revenew~., data=custidRev,method="pls",trControl=ctrl,tuneGrid=expand.grid(ncomp=pls.fit$bestTune))
    pls.fit$modelInfo
    plsme<-pls.fit$finalModel
    #pls analysis residuals
    qqplot(custidRev$revenew,myprediction)
    plot(plsme$residuals)
    summary(pls.fit)
    hist(plsme$residuals)
    myprediction<-predict(pls.fit, custidRev)
    #checking in on our rsme
    rmse( custidRev$revenew,myprediction)
    #0.7960927
    plot(importance)
    #checking R^2
    cor(custidRev$revenew,myprediction)^2
    #0.304417
    
    
  }
  if(submission==4){
    #fitting our model with the Target's trying to predict Predictor
    mars_fit<-earth(Target~Predictors,data=custidRev)
    #cross validation
    train.control <- trainControl(method = "cv", number = 10)
    marsmodel<-caret::train(revenew~.,data=custidRev,method='earth',trControl=train.control)
    #selecting the final model
    bestmars<-marsmodel$finalModel
    
    marspred<-predict(bestmars,custidRev)
    #viewing the data
    plotmo(bestmars)
    #mars residuals
    fivenum(bestmars$residuals)
    #plotting mars residuals
    plot(bestmars$residuals)
    #checking in on leverage
    plot(hatvalues(bestmars))
    #creating our leverage line
    abline(abline(h=2*(15)/4729),col="blue")
    hatvalues(bestmars)[hatvalues(bestmars)>0.2]
    #summary of mars data
    summary(bestmars)
    #histogram of the residuals
    hist(mars_fit$residuals)
    #checking in on our rsme
    rmse( custidRev$revenew,marspred)
    
    #RMSE = 0.7439428
    #RMSE with work_df=3 0.74674
    #checking our tuning parameters
    marsmodel$bestTune
    #plotting what variables were used
    importance<-varImp(marsmodel,scale=F)
    plot(importance)
    #plotting the residual of our best fitted data
    plot(bestmars)
    cor(custidRev$revenew,marspred)^2
    #0.6772734
    #R^2 with work_df=3 0.6748408
    qqplot(custidRev$revenew,marspred)
  }

  if (submission==5){
    #creating a seuqence of lambdas to test
    
    lambda_seq <- 10^seq(2, -2, by = -.1)
    #cross validation
    cv_output <- cv.glmnet(Predictors, Target, 
                           alpha = 0.5, lambda = lambda_seq)
    #viewing the cross validation
    plot(cv_output)
    cv_output
    #choosing our minimum lambda as optimal
    best_lam<-cv_output$lambda.min
    #fitting our model with our lambda
    elastic_best <- glmnet(Predictors, Target, alpha = 0.5, lambda = best_lam)
    
    elastic_best
    
    #RMSE of our problem (elasticnet)
    sqrt(cv_output$cvm[cv_output$lambda == cv_output$lambda.min])
    #R^2 of our problem (elasticnet)
    r2 <- cv_output$glmnet.fit$dev.ratio[which(cv_output$glmnet.fit$lambda == cv_output$lambda.min)]
  }
  if (submission==6){
    #tuning on neurons, 16 did best
  #for (nneurons in c(2, 4, 8, 16,32)) {
  #  BRNNpanda<-brnn(revenew~., data=custidRev,neurons=nneurons)
  #  BRNNpredict<-predict(BRNNpanda,custidRev)
   
  #  BRNNModelRMSE[nneurons] <- rmse( custidRev$revenew,BRNNpredict)
     #0.7160505
  #  rsq[nneurons]<-cor(custidRev$revenew,BRNNpredict)^2
    #0.701022
   # }
    
   
    
    BRNNtunedpanda<-brnn(revenew~., data=custidRev,neurons=16)
    
    
    train.control <- trainControl(method = "cv", number = 10)
    
    model <-caret::train(revenew~.,data=custidRev, method="brnn",trControl=train.control)
    
    
    BNNtunedpanda<- model$finalModel
    BRNNpredict<-predict(BRNNtunedpanda,custidRev)
    BRNNrmse<-rmse( custidRev$revenew,BRNNpredict)
    #RMSE = 0.6870601
    rsq<-cor(custidRev$revenew,BRNNpredict)^2
    #Rsqr = 0.7247387
    
  }
}