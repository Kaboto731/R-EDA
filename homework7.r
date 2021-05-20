# Packages
library(VIM)            # for missing data exploration
library(tidyverse)      # for data transformations
library(caret)          # for processes such as confusion matrix, train and non zero variance functions
library(randomForest)
library(MLmetrics)
library(car)
# read and combine the Train and Test data with the identifier column df_source
trainData <- read.csv("Train.csv", stringsAsFactors=TRUE)
testData <- read.csv("Test.csv", stringsAsFactors=TRUE)

dfAll <- data.frame(bind_rows(data.frame(mutate(trainData, df_source = "train")), 
                              mutate(testData, readmitted = NA, df_source = "test")))


# look at the missingness in the data
aggr(dfAll[,1:44], col=c('darkgreen','orange'), numbers=TRUE, sortVars=TRUE, 
     labels=names(data), ylab=c("Histogram of missing data","Pattern"))
dfAll$diagnosis %>% is.na() %>% sum()

# get rid of NA's
dfAll$payer_code[is.na(dfAll$payer_code)] = "Missing"
dfAll$race[is.na(dfAll$race)] = "Missing"
dfAll$medical_specialty[is.na(dfAll$medical_specialty)] = "Missing"
dfAll$diagnosis[is.na(dfAll$diagnosis)] = "Missing"
dfAll$diagnosis %>% is.na() %>% sum()

# look at the data
summary(dfAll)
glimpse(dfAll)
head(dfAll)
dfAll[,1:44] %>% select_if(is.double) %>% glimpse()



# transform categoric variables that are stored as numeric variables to characters
dfAll<- dfAll %>% mutate_at(vars(patientID, 
                                 admission_type, 
                                 discharge_disposition, 
                                 admission_source), as.character)

# change characters to factors
dfAll<- dfAll %>% mutate_if(is.character, as.factor)

# remove the variables with near zero variance
dfAll <- dfAll %>% dplyr::select(-c(nearZeroVar(dfAll)))

# level lumping for medical_specialty
levels(dfAll$medical_specialty)
dfAll$medical_specialty %>% table
ggplot() + geom_bar(aes(dfAll$medical_specialty)) + coord_flip()
dfAll$medical_specialty<- fct_lump(dfAll$medical_specialty, prop=0.01)  
ggplot() + geom_bar(aes(dfAll$medical_specialty)) + coord_flip()

# Backup data 
dfAll_before_diagnosis <- dfAll
dfAll <- dfAll_before_diagnosis

# grouping for diagnosis according to Diseases and Injuries Tabular Index
dfAll <- dfAll %>% mutate(diagnosis=
                            ifelse(startsWith(as.character(diagnosis), "V"), "V",
                                   ifelse(startsWith(as.character(diagnosis), "E"), "E",
                                          ifelse(between(as.numeric(as.character(diagnosis)), 1, 139), "1",
                                                 ifelse(between(as.numeric(as.character(diagnosis)), 140, 239), "2",
                                                        ifelse(between(as.numeric(as.character(diagnosis)), 240, 279), "3",
                                                               ifelse(between(as.numeric(as.character(diagnosis)), 280, 289), "4",
                                                                      ifelse(between(as.numeric(as.character(diagnosis)), 290, 319), "5",
                                                                             ifelse(between(as.numeric(as.character(diagnosis)), 320, 389), "6",
                                                                                    ifelse(between(as.numeric(as.character(diagnosis)), 390, 459), "7",
                                                                                           ifelse(between(as.numeric(as.character(diagnosis)), 460, 519), "8",
                                                                                                  ifelse(between(as.numeric(as.character(diagnosis)), 520, 579), "9", 
                                                                                                         ifelse(between(as.numeric(as.character(diagnosis)), 580, 629), "10",
                                                                                                                ifelse(between(as.numeric(as.character(diagnosis)), 630, 679), "11",
                                                                                                                       ifelse(between(as.numeric(as.character(diagnosis)), 680, 709), "12",
                                                                                                                              ifelse(between(as.numeric(as.character(diagnosis)), 710, 739), "13",
                                                                                                                                     ifelse(between(as.numeric(as.character(diagnosis)), 740, 759), "14",
                                                                                                                                            ifelse(between(as.numeric(as.character(diagnosis)), 760, 779), "15",
                                                                                                                                                   ifelse(between(as.numeric(as.character(diagnosis)), 780, 799), "16",
                                                                                                                                                          ifelse(between(as.numeric(as.character(diagnosis)), 800, 999), "17",
                                                                                                                                                                 "NotMapped"))))))))))))))))))))
ggplot() + geom_bar(aes(dfAll$diagnosis)) + coord_flip()



dfAll$payer_code<-as.numeric(dfAll$payer_code)
dfAll$payer_code[is.na(dfAll$payer_code)] <- 0

# seperate transformed Train and Test data
dfTrain <- dfAll %>% filter(df_source == "train") %>% dplyr::select(-c(df_source))
dfTest <- dfAll %>% filter(df_source == "test") %>% dplyr::select(-c(df_source, readmitted))


dfAll$diagnosis %>% is.na() %>% sum()
dfTrain$readmitted<- as.factor(dfTrain$readmitted)
Predictors$payer_code<-as.numeric(Predictors$payer_code)
Predictors$payer_code[is.na(Predictors$payer_code)] <- 0
Predictors$payer_code<-as.factor(Predictors$payer_code)
Target = dfTrain$readmitted
Predictors = dfTrain[c(3,9,15,16,18,27)]#2,3
Predictors <- na.omit(Predictors)
Target<- na.omit(Target)
Predictortest = dfTest
Predictors$readmitted <- as.factor(Predictors$readmitted)
# dmy <- dummyVars(" ~ .", data = Predictors)
# testPredictors = Predictors
# testPredictors$readmitted<-NULL
# dmy2 <- dummyVars(" ~ .", data = testPredictors)
# onehottrainer <- data.frame(predict(dmy, newdata = dfTrain))
# onehottrainer[is.na(onehottrainer)]<-0
# onehottester <- data.frame(predict(dmy2, newdata = dfTest))
# x<-as.matrix(onehottrainer)
# y<-as.numeric(Target)

Trainer =  dfTrain[c(3,9,15,16,18)]
PredTest = dfTest[c(3,9,15,16,18)]#2,3

df  <- as.data.frame(Trainer[c(1,2)])
mymat<-model.matrix(~ . + 0, data=df, contrasts.arg = lapply(df, contrasts, contrasts=FALSE))

mydf<- as.data.frame(mymat)

view(Trainer)
mydf$readmitted <- Trainer$e
#MARS
mars_fit<-earth(Target~x,data=dfTrain)

marspred<-predict(mars_fit,dfTrain)
marspred = marspred-1
?LogLoss
LogLoss(marspred,dfTrain$readmitted)

x<- as.matrix(dfTest)
testpred <-predict(mars_fit,x)

#Random Forest
myforest<-randomForest(readmitted~.,data=Predictors,importance=TRUE)
myforest$predicted
mypredtest<-predict(myforest, newdata=Predictors, type = "prob")

view(mypredtest)
view(PredTest)
mypredtest<-as.data.frame(mypredtest)


#0 submission 9.37188
myforest$predicted<- as.numeric(myforest$predicted)
Predictors$readmitted <- as.numeric(Predictors$readmitted)
LogLoss(myforest$predicted, Predictors$readmitted)

mydata <- data.frame(dfTest$patientID,mypredtest$`0` )

colnames(mydata) <- c("patientID", "predReadmit")
#saving the file to a CSV
write.csv(mydata,file="Panda7Forest.csv",row.names = FALSE)

colnames(mydata) <- c("custId", "predRevenue")


symbox(trainer$time_in_hospital)
symbox(trainer$num_lab_procedures)
symbox(trainer$num_procedures)
symbox(trainer$num_medications)
symbox(trainer$number_outpatient)
symbox(trainer$number_emergency)
symbox(trainer$number_inpatient)
symbox(trainer$number_diagnoses)