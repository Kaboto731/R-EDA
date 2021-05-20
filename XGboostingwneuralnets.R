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
library(keras)# for neural networks
library(corrplot)#for correlation plot
library(MASS)# for LDA
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
#
## collapsing the variables
# 
## collapsing the variables
#
# in age 
# trainer
trainer1$age = fct_collapse(trainer1$age, age70_90  = c("[80-90)", "[70-80)"), age20_60_no30_40  = c("[20-30)", "[40-50)", "[50-60)" )) #
# tester
tester1$age = fct_collapse(tester1$age, age70_90  = c("[80-90)", "[70-80)"), age20_60_no30_40  = c("[20-30)", "[40-50)", "[50-60)" ))
####


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
#    variable with 1 when race is AfricanAmerican
raceAfricanAmerican <- rep (0, nrowTrainer)
raceAfricanAmerican [trainer1$race == "AfricanAmerican"] <- 1
table(raceAfricanAmerican)
trainer1 <- data.frame(trainer1 ,raceAfricanAmerican)
#str(trainer1)
# tester
raceAfricanAmerican <- rep (0, nrowTester)
raceAfricanAmerican [tester1$race == "AfricanAmerican"] <- 1
table(raceAfricanAmerican)
tester1 <- data.frame(tester1 ,raceAfricanAmerican)
#str(tester1)
#


# base on age 
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
#    variable with 1 when diagnose has code 427
diagnose427 <- rep (0, nrowTrainer)
diagnose427 [trainer1$diagnosis == "427"] <- 1
table(diagnose427)
trainer1 <- data.frame(trainer1 ,diagnose427)
#str(trainer1)
# tester
diagnose427 <- rep (0, nrowTester)
diagnose427 [tester1$diagnosis == "427"] <- 1
table(diagnose427)
tester1 <- data.frame(tester1 ,diagnose427)
#str(tester1)
#
#    variable with 1 when diagnose has code 786
diagnose786 <- rep (0, nrowTrainer)
diagnose786 [trainer1$diagnosis == "786"] <- 1
table(diagnose786)
trainer1 <- data.frame(trainer1 ,diagnose786)
#str(trainer1)
# tester
diagnose786 <- rep (0, nrowTester)
diagnose786 [tester1$diagnosis == "786"] <- 1
table(diagnose786)
tester1 <- data.frame(tester1 ,diagnose786)
#str(tester1)
#
# base on payer_code 
#    variable with 1 when payer_code has miss
payer_code4 <- rep (0, nrowTrainer)
payer_code4 [trainer1$payer_code == "miss"] <- 1
table(payer_code4)
trainer1 <- data.frame(trainer1 ,payer_code4)
#str(trainer1)
# tester
payer_code4 <- rep (0, nrowTester)
payer_code4 [tester1$payer_code == "miss"] <- 1
table(payer_code4)
tester1 <- data.frame(tester1 ,payer_code4)
#str(tester1)
#
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
#    variable with 1 when payer_code has selfpay
payer_code1 <- rep (0, nrowTrainer)
payer_code1 [trainer1$payer_code == "selfpay"] <- 1
table(payer_code1)
trainer1 <- data.frame(trainer1 ,payer_code1)
#str(trainer1)
# tester
payer_code1 <- rep (0, nrowTester)
payer_code1 [tester1$payer_code == "selfpay"] <- 1
table(payer_code1)
tester1 <- data.frame(tester1 ,payer_code1)
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
tester1 <- data.frame(tester1 ,diabetesMedNum)
#str(tester1)
#

# check if all nominal variables are factors, if not, reformate them as factors
trainer1$patientID = as.factor(trainer1$patientID)
trainer1$admission_type = as.factor(trainer1$admission_type)
trainer1$discharge_disposition = as.factor(trainer1$discharge_disposition)
trainer1$admission_source = as.factor(trainer1$admission_source)
tester1$patientID = as.factor(tester1$patientID)
tester1$admission_type = as.factor(tester1$admission_type)
tester1$discharge_disposition = as.factor(tester1$discharge_disposition)
tester1$admission_source = as.factor(tester1$admission_source)

# model data frame
# tester
tester1 %>% select(c(number_inpatient, number_diagnoses, number_emergency,
                     number_outpatient, diagnose428, diagnose427, diagnose786, 
                     time_in_hospital, 
                     raceCaucasian, raceAfricanAmerican, age, discharge_disposition,
                     payer_code2, payer_code3, diabetesMedNum,    
                     admission_source)) -> tester2 # medical_specialty
# trainer
trainer1 %>% select(c(number_inpatient, number_diagnoses, number_emergency,
                      number_outpatient, diagnose428, diagnose427, diagnose786, 
                      time_in_hospital, 
                      raceCaucasian, raceAfricanAmerican, age, discharge_disposition, readmitted,
                      payer_code2, payer_code3, diabetesMedNum,
                      admission_source)) -> trainer2

trainer2 %>% dplyr::select(c(nearZeroVar(trainer2))) -> trainer22
str(trainer22)
trainer2 %>% dplyr::select(-c(nearZeroVar(trainer2))) -> trainer2
tester2 %>% dplyr::select(-c(nearZeroVar(tester2))) -> tester2

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


### xgboost  
#
# tester
tester2 %>% select_if(is.numeric) -> tester3 # medical_specialty
# trainer
trainer2 %>% select_if(is.numeric) -> trainer3
str(trainer3)
# add

#
# fit the model
fitter <- xgboost(data = as.matrix(trainer3), label = trainer$readmitted, 
                  max.depth = 2, eta = 1, nthread = 2, nrounds = 2, 
                  objective = "binary:logistic")
#
# predicted Target
Target_predicted <- predict(fitter, as.matrix(trainer3))
# 
### evaluate the performance
# LogLoss(Target_predicted, Target_actual)
LogLoss(Target_predicted, Target_actual_fix)
#
  ### 
  #
  ### prediction
  predReadmit = predict(fitter, as.matrix(tester3))
  #
  submission <- data.frame(patientID, predReadmit)
  write.csv(submission, "xgboost.csv",row.names = FALSE)
  
  
  # to EDA
  ggplot() + geom_bar(aes(trainer1$medical_specialty)) + coord_flip()
  ggplot() + geom_bar(aes(fct_lump(trainer1$medical_specialty, n=7))) + coord_flip()
  
  ggplot() + geom_bar(aes(trainer1$diagnosis)) + coord_flip()
  ggplot() + geom_bar(aes(fct_lump(trainer1$diagnosis, n=600))) + coord_flip()

  
  
  # does not work
  
  # dtrain <- xgb.DMatrix(data = as.matrix(trainer3), label=trainer$readmitted)
  # dtest <- xgb.DMatrix(data = as.matrix(tester3))
  # watchlist <- list(train=dtrain, test=dtest)  
  # fitter <-  xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")
  categorical_labels <- to_categorical(trainer$readmitted, num_classes =2)
  categorical_labels2 <- to_categorical(trainer3, num_classes = NULL)
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')
  model %>% compile(
    optimizer = 'adam', 
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
  model %>% fit(as.matrix(trainer3), categorical_labels, epochs = 300,
                validation_split = 0.2) 
  predictions <- model %>% predict(as.matrix(trainer3))
  predictions<- as.data.frame(predictions) #0.6394
  
  LogLoss(predictions$V1, trainer$readmitted)
  predictions <- model %>% predict(as.matrix(tester3))
  predictions<- as.data.frame(predictions) 
  predReadmit<- predictions$V2
  submission <- data.frame(patientID, predReadmit)
  write.csv(submission, "neuralnets.csv",row.names = FALSE)
  
  trainer3$readmitted = as.factor(trainer$readmitted)
  rd_fit <- randomForest(readmitted ~ ., data = trainer3, importance = T, ntrees=1500, mtry=3)
  predRF = predict(rd_fit, newdata=trainer3, type = "prob")
  predRF_factors = factor(predRF, levels = levels(trainer3$Default))
  readmitted_probs <- predRF[,2]
  LogLoss(readmitted_probs,trainer$readmitted)
  
  trainer3$readmitted = as.integer(trainer$readmitted)
  mycor <-cor(trainer3)
  corrplot(mycor,method="square")
  
  r <- lda(readmitted~.,data=trainer3, CV=TRUE)
  plot(lda(readmitted~.,data=trainer3))
  
  