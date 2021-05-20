library(ggplot2) #ggplotting
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


### reading data
#reading in test file
tester = read.csv('Test.csv',header=TRUE)
#
#reading in the trainer file
trainer = read.csv("Train.csv",header=TRUE) # load data

### wragling data
## take out 
# variables with zero variance
trainer %>% dplyr::select(-c(nearZeroVar(trainer))) -> trainer1
tester %>% dplyr::select(-c(nearZeroVar(trainer))) -> tester1
#
## find out if there is any duplicate row. Remove it
trainer1[duplicated(trainer1),]
trainer1 = trainer1[!duplicated(trainer1, fromLast = TRUE),] # To remove duplicate row and resave data frame as trainer1
trainer1[duplicated(trainer1),]
tester1[duplicated(tester1),]
tester1 = tester1[!duplicated(tester1, fromLast = TRUE),] # To remove duplicate row and resave data frame as tester1
tester1[duplicated(tester1),]
#
## replacing missing values with "miss"
# in payer_code
# trainer
# Get levels and add "miss"
levels_payer_code <- levels(trainer1$payer_code)
levels_payer_code[length(levels_payer_code) + 1] <- "miss"
# refactor payer_code to include "miss" as a factor level
#    and replace NA with "miss"
trainer1$payer_code <- factor(trainer1$payer_code, levels = levels_payer_code)
trainer1$payer_code[is.na(trainer1$payer_code)] <- "miss"
#table(trainer1$payer_code)
# tester
# Get levels and add "miss"
levelsTester_payer_code <- levels(tester1$payer_code)
levelsTester_payer_code[length(levelsTester_payer_code) + 1] <- "miss"
# refactor payer_code to include "miss" as a factor level
#    and replace NA with "miss"
tester1$payer_code <- factor(tester1$payer_code, levels = levelsTester_payer_code)
tester1$payer_code[is.na(tester1$payer_code)] <- "miss"
#table(tester1$payer_code)
#
# in race
# trainer
# Get levels and add "miss"
levels_race <- levels(trainer1$race)
levels_race[length(levels_race) + 1] <- "miss"
# refactor race to include "miss" as a factor level
#    and replace NA with "miss"
trainer1$race <- factor(trainer1$race, levels = levels_race)
trainer1$race[is.na(trainer1$race)] <- "miss"
# tester
# Get levels and add "miss"
levelsTester_race <- levels(tester1$race)
levelsTester_race[length(levelsTester_race) + 1] <- "miss"
# refactor race to include "miss" as a factor level
#    and replace NA with "miss"
tester1$race <- factor(tester1$race, levels = levelsTester_race)
tester1$race[is.na(tester1$race)] <- "miss"
#
# in diagnosis
# trainer
# Get levels and add "0"
levels_diagnosis <- levels(trainer1$diagnosis)
levels_diagnosis[length(levels_diagnosis) + 1] <- "0"
# refactor diagnosis to include "0" as a factor level
#    and replace NA with "0"
trainer1$diagnosis <- factor(trainer1$diagnosis, levels = levels_diagnosis)
trainer1$diagnosis[is.na(trainer1$diagnosis)] <- "0"
# tester
# Get levels and add "0"
levelsTester_diagnosis <- levels(tester1$diagnosis)
levelsTester_diagnosis[length(levelsTester_diagnosis) + 1] <- "0"
# refactor diagnosis to include "0" as a factor level
#    and replace NA with "0"
tester1$diagnosis <- factor(tester1$diagnosis, levels = levelsTester_diagnosis)
tester1$diagnosis[is.na(tester1$diagnosis)] <- "0"
#
# in medical_specialty
# trainer
# Get levels and add "miss"
levels_ms <- levels(trainer1$medical_specialty)
levels_ms[length(levels_ms) + 1] <- "miss"
# refactor medical_specialty to include "miss" as a factor level
#    and replace NA with "miss"
trainer1$medical_specialty <- factor(trainer1$medical_specialty, levels = levels_ms)
trainer1$medical_specialty[is.na(trainer1$medical_specialty)] <- "miss"
# tester
# Get levels and add "miss"
levelsTester_ms <- levels(tester1$medical_specialty)
levelsTester_ms[length(levelsTester_ms) + 1] <- "miss"
# refactor medical_specialty to include "miss" as a factor level
#    and replace NA with "miss"
tester1$medical_specialty <- factor(tester1$medical_specialty, levels = levelsTester_ms)
tester1$medical_specialty[is.na(tester1$medical_specialty)] <- "miss"
####
## check if all nominal variables are factors, if not, reformat them as factors
trainer1$patientID = as.factor(trainer1$patientID)
trainer1$admission_type = as.factor(trainer1$admission_type)
trainer1$discharge_disposition = as.factor(trainer1$discharge_disposition)
trainer1$admission_source = as.factor(trainer1$admission_source)
tester1$patientID = as.factor(tester1$patientID)
tester1$admission_type = as.factor(tester1$admission_type)
tester1$discharge_disposition = as.factor(tester1$discharge_disposition)
tester1$admission_source = as.factor(tester1$admission_source)
####
# ## collapsing the variables
# #
# # in age 
# # trainer
# trainer1$age = fct_collapse(trainer1$age, age70_90  = c("[80-90)", "[70-80)"), age20_60_no30_40  = c("[20-30)", "[40-50)", "[50-60)" )) #
# # tester
# tester1$age = fct_collapse(tester1$age, age70_90  = c("[80-90)", "[70-80)"), age20_60_no30_40  = c("[20-30)", "[40-50)", "[50-60)" ))
# ####
#
## create new feature
nrowTrainer = nrow(trainer1)
nrowTester = nrow(tester1)
#
# base on race 
#    variable with 1 when race is Caucasian
raceCaucasian <- rep (0, nrowTrainer)
raceCaucasian [trainer1$race == "Caucasian"] <- 1
table(raceCaucasian)
trainer1 <- data.frame(trainer1 ,raceCaucasian)
#str(trainer1)
# tester
raceCaucasian <- rep (0, nrowTester)
raceCaucasian [tester1$race == "Caucasian"] <- 1
table(raceCaucasian)
tester1 <- data.frame(tester1 ,raceCaucasian)
#str(tester1)
#
# base on age 
#    variable with 1 when age is [70-80)
age70_80 <- rep (0, nrowTrainer)
age70_80 [trainer1$age == "[70-80)"] <- 1
table(age70_80)
trainer1 <- data.frame(trainer1,age70_80)
#str(trainer1)
# tester
age70_80 <- rep (0, nrowTester)
age70_80 [tester1$age == "[70-80)"] <- 1
table(age70_80)
tester1 <- data.frame(tester1, age70_80)
#str(tester1)
#
# base on admission_type 
#    variable with 1 when admission_type is 6
admission_type6 <- rep (0, nrowTrainer)
admission_type6 [trainer1$admission_type == "6"] <- 1
table(admission_type6)
trainer1 <- data.frame(trainer1, admission_type6)
#str(trainer1)
# tester
admission_type6 <- rep (0, nrowTester)
admission_type6 [tester1$admission_type == "6"] <- 1
table(admission_type6)
tester1 <- data.frame(tester1, admission_type6)
#str(tester1)
#
# base on discharge_disposition 
#    variable with 1 when discharge_disposition is 6
discharge_disposition6 <- rep (0, nrowTrainer)
discharge_disposition6 [trainer1$discharge_disposition == "6"] <- 1
table(discharge_disposition6)
trainer1 <- data.frame(trainer1, discharge_disposition6)
#str(trainer1)
# tester
discharge_disposition6 <- rep (0, nrowTester)
discharge_disposition6 [tester1$discharge_disposition == "6"] <- 1
table(discharge_disposition6)
tester1 <- data.frame(tester1, discharge_disposition6)
#str(tester1)
#
# base on admission_source 
#    variable with 1 when admission_source is 1
admission_source1 <- rep (0, nrowTrainer)
admission_source1 [trainer1$admission_source == "1"] <- 1
table(admission_source1)
trainer1 <- data.frame(trainer1, admission_source1)
#str(trainer1)
# tester
admission_source1 <- rep (0, nrowTester)
admission_source1 [tester1$admission_source == "1"] <- 1
table(admission_source1)
tester1 <- data.frame(tester1, admission_source1)
#str(tester1)
#
# base on medical_specialty 
#    variable with 1 when medical_specialty is miss
medical_specialtymiss <- rep (0, nrowTrainer)
medical_specialtymiss [trainer1$medical_specialty == "miss"] <- 1
table(medical_specialtymiss)
trainer1 <- data.frame(trainer1, medical_specialtymiss)
#str(trainer1)
# tester
medical_specialtymiss <- rep (0, nrowTester)
medical_specialtymiss [tester1$medical_specialty == "miss"] <- 1
table(medical_specialtymiss)
tester1 <- data.frame(tester1, medical_specialtymiss)
#str(tester1)
#
# base on metformin 
#    variable with 1 when metformin is No
metforminNo <- rep (0, nrowTrainer)
metforminNo [trainer1$metformin == "No"] <- 1
table(metforminNo)
trainer1 <- data.frame(trainer1, metforminNo)
#str(trainer1)
# tester
metforminNo <- rep (0, nrowTester)
metforminNo [tester1$metformin == "No"] <- 1
table(metforminNo)
tester1 <- data.frame(tester1, metforminNo)
#str(tester1)
#
# base on insulin 
#    variable with 1 when insulin is Down
insulinDown <- rep (0, nrowTrainer)
insulinDown [trainer1$insulin == "Down"] <- 1
table(insulinDown)
trainer1 <- data.frame(trainer1, insulinDown)
#str(trainer1)
# tester
insulinDown <- rep (0, nrowTester)
insulinDown [tester1$insulin == "Down"] <- 1
table(insulinDown)
tester1 <- data.frame(tester1, insulinDown)
#str(tester1)
#
#    variable with 1 when insulin is Up
insulinUp <- rep (0, nrowTrainer)
insulinUp [trainer1$insulin == "Up"] <- 1
table(insulinUp)
trainer1 <- data.frame(trainer1, insulinUp)
#str(trainer1)
# tester
insulinUp <- rep (0, nrowTester)
insulinUp [tester1$insulin == "Up"] <- 1
table(insulinUp)
tester1 <- data.frame(tester1, insulinUp)
#str(tester1)
#
# base on diagnose 
#    variable with 1 when diagnose has code 428
diagnose428 <- rep (0, nrowTrainer)
diagnose428 [trainer1$diagnosis == "428"] <- 1
table(diagnose428)
trainer1 <- data.frame(trainer1 ,diagnose428)
#str(trainer1)
# tester
diagnose428 <- rep (0, nrowTester)
diagnose428 [tester1$diagnosis == "428"] <- 1
table(diagnose428)
tester1 <- data.frame(tester1 ,diagnose428)
#str(tester1)
#
# base on payer_code 
#    variable with 1 when payer_code has medicare
payer_code3 <- rep (0, nrowTrainer)
payer_code3 [trainer1$payer_code == "medicare"] <- 1
table(payer_code3)
trainer1 <- data.frame(trainer1 ,payer_code3)
#str(trainer1)
# tester
payer_code3 <- rep (0, nrowTester)
payer_code3 [tester1$payer_code == "medicare"] <- 1
table(payer_code3)
tester1 <- data.frame(tester1 ,payer_code3)
#str(tester1)
#
#    variable with 1 when payer_code has insurance
payer_code2 <- rep (0, nrowTrainer)
payer_code2 [trainer1$payer_code == "insurance"] <- 1
table(payer_code2)
trainer1 <- data.frame(trainer1 ,payer_code2)
#str(trainer1)
# tester
payer_code2 <- rep (0, nrowTester)
payer_code2 [tester1$payer_code == "insurance"] <- 1
table(payer_code2)
tester1 <- data.frame(tester1 ,payer_code2)
#str(tester1)
#
# base on diabetesMed 
#    variable with 1 when diabetesMed has Yes
diabetesMedNum <- rep (0, nrowTrainer)
diabetesMedNum [trainer1$diabetesMed == "Yes"] <- 1
table(diabetesMedNum)
trainer1 <- data.frame(trainer1 ,diabetesMedNum)
#str(trainer1)
# tester
diabetesMedNum <- rep (0, nrowTester)
diabetesMedNum [tester1$diabetesMed == "Yes"] <- 1
table(diabetesMedNum)
tester1 <- data.frame(tester1,diabetesMedNum)
#str(tester1)
#
####
#
# model data frame
# tester
tester1 %>% select(c(number_inpatient, number_diagnoses, number_emergency,
                     number_outpatient, time_in_hospital, 
                     num_procedures, 
                     diagnose428,  
                     raceCaucasian,  
                     age70_80, 
                     admission_type6,
                     discharge_disposition6, 
                     admission_source1,
                     payer_code2, payer_code3, 
                     medical_specialtymiss, 
                     metforminNo, 
                     insulinDown, insulinUp,  
                     diabetesMedNum)) -> tester2 # medical_specialty
# trainer
trainer1 %>% select(c(number_inpatient, number_diagnoses, number_emergency,
                      number_outpatient, time_in_hospital, 
                      num_procedures,     
                      diagnose428, 
                      raceCaucasian,   
                      age70_80,
                      admission_type6,
                      discharge_disposition6, 
                      admission_source1,
                      medical_specialtymiss, 
                      metforminNo, 
                      insulinDown, insulinUp,  
                      diabetesMedNum, readmitted)) -> trainer2

trainer2 %>% dplyr::select(c(nearZeroVar(trainer2))) -> trainer22
str(trainer22)
# trainer2 %>% dplyr::select(-c(nearZeroVar(trainer2))) -> trainer2
# tester2 %>% dplyr::select(-c(nearZeroVar(tester2))) -> tester2

## data transformation
#trainer2$number_inpatient = log(trainer1$number_inpatient+1)
trainer2$number_diagnoses = log(trainer1$number_diagnoses)
#trainer2$number_emergency = log(trainer1$number_emergency+1)
#trainer2$number_outpatient = log(trainer1$number_outpatient+1)
trainer2$time_in_hospital = log(trainer1$time_in_hospital)
tester2$number_diagnoses = log(tester2$number_diagnoses)
tester2$time_in_hospital = log(tester2$time_in_hospital)
#
# z scaling
trainer2$number_inpatient = (trainer2$number_inpatient - mean(trainer2$number_inpatient))/sd(trainer2$number_inpatient)
trainer2$number_emergency = (trainer2$number_emergency - mean(trainer2$number_emergency))/sd(trainer2$number_emergency)
trainer2$number_outpatient = (trainer2$number_outpatient - mean(trainer2$number_outpatient))/sd(trainer2$number_outpatient)
trainer2$num_procedures = (trainer2$num_procedures - mean(trainer2$num_procedures))/sd(trainer2$num_procedures)
# tester
tester2$number_inpatient = (tester2$number_inpatient - mean(tester2$number_inpatient))/sd(tester2$number_inpatient)
tester2$number_emergency = (tester2$number_emergency - mean(tester2$number_emergency))/sd(tester2$number_emergency)
tester2$number_outpatient = (tester2$number_outpatient - mean(tester2$number_outpatient))/sd(tester2$number_outpatient)
tester2$num_procedures = (tester2$num_procedures - mean(tester2$num_procedures))/sd(tester2$num_procedures)
####
#
# set the TARGET as factor in order to avoid some problems in models that can be also regressions
trainer2$readmitted <- as.factor(trainer2$readmitted)

#
# overwrite original values of target to 1 if they are "Y", and to 0 otherwise
#   we do not need to do it for this data set though
Target_actual_fix  <- ifelse(trainer2$readmitted=="Y", 1, 0) 
# 
# asign patientID from the TESTER!!!!
patientID <- tester$patientID # be sure that it is tester patient ID because 
# tester and trainer have diferent # of observations


### MARS  
#
fitter <- earth(readmitted ~ ., data=trainer2, degree=3, nprune = 11,glm=list(family=binomial))
# model summary
#summary(fitter)
#

train.control <- trainControl(method = "cv", number = 10)
marsmodel<-caret::train(readmitted~.,data=trainer2,method='earth',trControl=train.control)
#selecting the final model
bestmars<-marsmodel$finalModel

marspred<-predict(bestmars,trainer2)
class_prediction <-as.factor(
  ifelse(marspred > 0.50,
         1,
         0
  ))
readmittednum = as.factor(trainer2$readmitted)
marsprednum = as.factor(marspred)
xtab = table(marsprednum,readmittednum)
confusionMatrix(class_prediction,trainer2$readmitted)
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


# predicted Target
Target_predicted = fitter$fitted.values
# 
### evaluate the performance
# LogLoss(Target_predicted, Target_actual)
LogLoss(Target_predicted, Target_actual_fix)
#
# importent variables
varImp(fitter)  
  ### 
  #
  ### prediction
  predReadmit = predict(fitter, tester2, type = "response")

  #
  submission <- data.frame(patientID, predReadmit)
  write.csv(submission, "MARS.csv",row.names = FALSE)
