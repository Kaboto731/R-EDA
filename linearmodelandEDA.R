# show all installed packages
library(tidyverse) # for gglot2 
library(ggplot2) # to install ggbiplot
library(ggbiplot) # for biplot
library(dplyr) # for data transformation
library(VIM) # for aggr
library(mice) # for md.pairs
library(Rcpp) # for Loading Amelia
library(Amelia) # for missmap
library(gridExtra) # for grid.arrange
library(caret) #train control
library(car) # for symbox
library(randomForest) # for randomForest
library(haven) # for reading the names of the countries
library(corrgram) # for correlogram
library(forcats) # for fct_lump

Train = read_csv("Train.csv") # load data
#?Train
#View(Train)
#
# tranform all characters in factors
Train = Train %>% mutate_if(is.character, as.factor)

# rescale timeSinceLastVisit from the second to the weeks
Train$timeSinceLastVisit = Train$timeSinceLastVisit/604800

# summarize the percentage missing for each column
Train %>% mutate_all(is.na) %>% summarise_all(mean) %>% glimpse()
#
# extract some data for easier further analysis
revenue = Train$revenue
PVs = Train$pageviews # PVs
TLV = Train$timeSinceLastVisit # in week units: 1 = 1 week
VN = Train$visitNumber

miss_values_pr = function(x) mean(is.na(x))
#
miss_values = apply(Train,2,miss_values_pr)
miss_values_index =  which(miss_values > 0.96);
#
# variables with missingnes greater than 90% delete
Train_96 = Train[,-miss_values_index]

missingness_rev = aggr(Train_96,delimiter = NULL, plot = TRUE)
# # summary of missing values
summary(missingness_rev)

Train_96 %>% select(c(country, metro, region, city)) %>% md.pattern(plot = T, rotate.names = T)

# take out sessionId, custId, newVisits, bounces and select only numeric variables
Train_96_cor = Train_96 %>% select(-c(sessionId, custId, newVisits, bounces)) %>% select_if(is.numeric) %>% na.omit()
# plot correlogram
correlogram_Train_96_cor <- corrgram(Train_96_cor, order=NULL, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)


# Return high correlations (1 > |ij| > 0.001)
hi_corr <- data.frame()
m <- sqrt(length(correlogram_Train_96_cor))
for (i in 1:m){
  for (j in 1:m){
    if ( !is.na(correlogram_Train_96_cor[i,j])){
      if ( abs(correlogram_Train_96_cor[i,j]) > 0.001 && correlogram_Train_96_cor[i,j] < 1) {
        hi_corr[i,1] <- c(paste(rownames(correlogram_Train_96_cor)[i], "-" , colnames(correlogram_Train_96_cor)[j]))
        hi_corr[i,2] <- c(paste(correlogram_Train_96_cor[i,j]))
      }
    }
  }
}
hi_corr <- na.omit(hi_corr) # Omit NAs
dupes <- duplicated(x=hi_corr$V2) # Select Duplicates
hi_corr <- data.frame(hi_corr$V1[dupes == FALSE], hi_corr$V2[dupes == FALSE]) #Remove duplicates
colnames(hi_corr) <- c("Predictors", "Correlation")
hi_corr
# # excluding custId
Train_pairs = Train_96_cor%>% select(-c(visitStartTime))
#plot pairs
#windows()
Train_pairs %>% pairs()

# plot the distributions 
g2 = ggplot(data = Train, mapping = aes(PVs)) + geom_histogram() + labs(x = "PVs")
g3 =  ggplot(data = Train, mapping = aes(TLV)) + geom_histogram() + labs(x = "TLV")
g4 =  ggplot(data = Train, mapping = aes(VN)) + geom_histogram() + labs(x = "VN")
g5 =  ggplot(data = Train, mapping = aes(PVs/VN)) + geom_histogram() + labs(x = "PVs/VN")
g6 =  ggplot(data = Train, mapping = aes((TLV/PVs)^(-1))) + geom_histogram() + labs(x = "PVs/TLV")

grid.arrange(g2, g3, g4, g5, g6, ncol = 3,
             bottom = "Fig. 5. Original distributions of PVs, TLV, VN, PVs/VN, and PVs/TLV.")

par(mfrow = c(2,3)) # arranging the plots on the page
# plot symbox
symbox(PVs, powers=c(0,-0.5,-1, -2))
symbox(TLV, powers=c(0,-0.5,-1,-2,-3)) # 0,-0.5,-1,-2,
symbox(VN, powers=c(0,-0.5,-1,-2,-3)) # 0,-0.5,
symbox(PVs/VN, powers=c(0,-0.5))
symbox(PVs/TLV, powers=c(1,0,-0.5, -1))

g1 = ggplot(mapping = aes(log(PVs))) + geom_histogram() + labs(x = "log(PVs)")+ coord_cartesian(ylim = c(0, 20000))
g2 = ggplot(mapping = aes(log(TLV))) + geom_histogram() + labs(x = "log(TLV)")+ coord_cartesian(ylim = c(0, 2700))
g3 = ggplot(mapping = aes(-1/VN)) + geom_histogram() + labs(x = "-1/VN")
g4 = ggplot(mapping = aes(log(PVs/VN))) + geom_histogram() + labs(x = "log(PVs/VN)") + coord_cartesian(ylim = c(0, 10000))
g5 = ggplot(mapping = aes(log(PVs/TLV))) + geom_histogram() + labs(x = "log(PVs/TLV)")+ coord_cartesian(ylim = c(0, 2700))
grid.arrange(g1, g2, g3, g4, g5, nrow = 2, ncol = 3,
             bottom = "Fig. 7. Transformed Distributions of PVs, TLV, VN, PVs/VN, 
            and PVs/TLV using the symbox function.")
Train_96 %>% 
  mutate(channelGrouping = fct_lump(channelGrouping, n=8)) %>%   # keep all levels for channelGrouping because it has only 8 levels.
  group_by(channelGrouping) %>% 
  dplyr::summarize(n = n(),   #some basic summary stats         
                   mean(revenue),   # sum
                   stdev = sd(revenue)) %>% 
  arrange(desc(n))        #and order t

Train_96 %>% 
  mutate(channelGrouping = fct_collapse(Train$channelGrouping, chan_Gr_meanRev13 = c("Direct","Display","Social","Affiliates","(Other)","Referral")) )%>%   
  group_by(channelGrouping) %>% 
  dplyr::summarize(n = n(),   #some basic summary stats         
                   mean(revenue),   # could be to sum
                   stdev = sd(revenue)) %>% 
  arrange(desc(n))        #and order t

Train_96 %>% 
  mutate(referralPath = fct_lump(fct_explicit_na(referralPath), n = 4)) %>%   #use of fct_explicit_na() to make "NA" a factor level too,  and then fct_lump it as well., keep top 5 most frequent countries; lump everything else into "other" category
  group_by(referralPath) %>% 
  dplyr::summarize(n = n(),   #some basic summary stats         
                   mean(revenue),   # mean, we can use sum here to. If sum(revenue) is considerably greater than sum (revenue) of other levels, we will delete this variable
                   stdev = sd(revenue)) %>% 
  arrange(desc(n))        #and order them

# create a new data frame Train_anal that includes everyting from Train_96 except
# "date", "region", "metro", ... (s. below)
Train_anal = Train_96 %>% select(-c("date","region", "metro", "city", "networkDomain", "topLevelDomain", "medium", "referralPath", "subContinent", "visitStartTime", "newVisits", "bounces"))

# run this piece line by line otherwives it does not work corretly and I do  not know why?
# collapse variables in channelGrouping and writing it in a new data frame Train_anal_col
Train_anal %>% dplyr::mutate(channelGrouping = forcats::fct_collapse(Train_anal$channelGrouping, chan_Gr_meanRev13 = c("Direct", "Display", "Social", "Affiliates", "(Other)", "Referral")) ) -> Train_anal_col 
# table(Train_anal_col$channelGrouping)  # view collapced channelGrouping

# collapse variables in browser
Train_anal_col = Train_anal %>% dplyr::mutate(browser = forcats::fct_lump(fct_explicit_na(browser), n = 1)) 
# table(Train_anal_col$browser)  # view collapced browser

# collapse variables in operatingSystem
Train_anal_col = Train_anal %>% dplyr::mutate(operatingSystem = forcats::fct_lump(fct_explicit_na(operatingSystem), n = 3))

# collapse variables in deviceCategory
Train_anal_col = Train_anal %>% dplyr::mutate(deviceCategory = forcats::fct_lump(fct_explicit_na(deviceCategory), n = 1))

# collapse variables in continent
Train_anal_col = Train_anal %>% dplyr::mutate(continent = forcats::fct_lump(fct_explicit_na(continent), n = 1))

# collapse variables in country
Train_anal_col = Train_anal %>% dplyr::mutate(country = forcats::fct_lump(fct_explicit_na(country), n = 4)) 

# collapse variables in source
Train_anal_col = Train_anal %>% dplyr::mutate(source = forcats::fct_lump(fct_explicit_na(source), n = 4)) 


pick_up_tester = 0 # if you work only train the model pick_up_tester = 0, if you are ready to submit, pick_up_tester = 1

if (pick_up_tester == 1) {
  #reading in test file
  tester = read.csv('Test.csv',header=TRUE)
} else {
  #reading in the trainer file
  trainer = read.csv("Train.csv", header = TRUE)
}

#
mycol = fct_collapse(trainer$channelGrouping,chan_Gr_p_big = c("Direct","Display","Social","Affiliates","(Other)","Referral"))
mycol2 = fct_collapse(trainer$operatingSystem, OS_p_big = c("iOS","Windows Phone", "Xbox", "FreeBSD", "BlackBerry", "Nintendo Wii","Nintendo WiiU","Nintendo 3DS","Firefox OS","Samsung","Linux"))
mycolt = fct_collapse(tester$channelGrouping,chan_Gr_p_big = c("Direct","Display","Social","Affiliates","(Other)","Referral"))
mycol2t = fct_collapse(tester$operatingSystem, OS_p_big = c("iOS","Windows Phone", "Xbox", "FreeBSD", "BlackBerry", "Nintendo Wii","Nintendo WiiU","Nintendo 3DS","Firefox OS","Samsung","Linux"))
# Setting the data frame
df2 <- as.data.frame(mycol2)
df<-as.data.frame(mycol)
#setting data frame with tester
df2t <- as.data.frame(mycol2t)
dft<-as.data.frame(mycolt)
#one hot encoding the factors
mymat<-model.matrix(~ . + 0, data=df, contrasts.arg = lapply(df, contrasts, contrasts=FALSE))
mymat2<-model.matrix(~ . + 0, data=df2, contrasts.arg = lapply(df2, contrasts, contrasts=FALSE))
#for there testing
mymatt<-model.matrix(~ . + 0, data=df, contrasts.arg = lapply(df, contrasts, contrasts=FALSE))
mymat2t<-model.matrix(~ . + 0, data=df2, contrasts.arg = lapply(df2, contrasts, contrasts=FALSE))
#importing as data frame
mydf<- as.data.frame(mymat)
mydf2<- as.data.frame(mymat2)
mydft<- as.data.frame(mymatt)
mydf2t<- as.data.frame(mymat2t)
# making more columns in trainer
#Fixing NA values
trainer <- trainer
trainer[trainer==""]=NA
trainer$mycol <- mycol
trainer$onebig<-mydf$mycolchan_Gr_p_big
trainer$Osearch<-mydf$`mycolOrganic Search`
trainer$Psearch<-mydf$`mycolPaid Search`
trainer$mwindows<-mydf2$mycol2Windows
trainer$mandroid<-mydf2$mycol2Android
trainer$mbig<-mydf2$mycol2OS_p_big
trainer$mchrome<-mydf2$`mycol2Chrome OS`
trainer$mmac<-mydf2$mycol2Macintosh 

tester <- tester
tester[tester==""]=NA
tester$mycol <- mycol
tester$onebig<-mydf$mycolchan_Gr_p_big
tester$Osearch<-mydf$`mycolOrganic Search`
tester$Psearch<-mydf$`mycolPaid Search`
tester$mwindows<-mydf2$mycol2Windows
tester$mandroid<-mydf2$mycol2Android
tester$mbig<-mydf2$mycol2OS_p_big
tester$mchrome<-mydf2$`mycol2Chrome OS`
tester$mmac<-mydf2$mycol2Macintosh 


if (pick_up_tester == 1) {
  # for test
  custidRev<-ddply(tester,.(custId),summarize,onebig=sum(onebig,na.rm=TRUE),
                   osearch=sum(Osearch,na.rm=TRUE),mwindows=sum(mwindows,na.rm=TRUE),mandroid=sum(mandroid,na.rm=TRUE),
                   mbig=sum(mbig,na.rm=TRUE),mchrome=sum(mchrome),mmac=sum(mmac,na.rm=TRUE),psearch=sum(Psearch,na.rm=TRUE),
                   timesince=mean(timeSinceLastVisit,na.rm=TRUE),visitnum= sum(visitNumber,na.rm=TRUE ),
                   pageviews=sum(pageviews,na.rm=TRUE), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE))
  #prediction with the test data
  predRevenue<-predict(fit,custidRev)
  #storing the custID as its own row
  custId<-custidRev$custId
  #storing the data frame
  mydata <- data.frame(custId, predRevenue)
  #saving the file to a CSV
  write.csv(mydata,file="ManuelPred2.csv",row.names = FALSE)
  
  
  
}else {
  # for train
  #creating the data 
  custidRev<-ddply(trainer,.(custId),revenew=log(sum(revenue,na.rm=TRUE)+1),summarize,onebig=sum(onebig,na.rm=TRUE),
                   osearch=sum(Osearch,na.rm=TRUE),mwindows=sum(mwindows,na.rm=TRUE),mandroid=sum(mandroid,na.rm=TRUE),
                   mbig=sum(mbig,na.rm=TRUE),mchrome=sum(mchrome),mmac=sum(mmac,na.rm=TRUE),psearch=sum(Psearch,na.rm=TRUE),
                   timesince=mean(timeSinceLastVisit,na.rm=TRUE),visitnum= sum(visitNumber,na.rm=TRUE ),
                   pageviews=sum(pageviews,na.rm=TRUE), isMobile=median(isMobile,na.rm=TRUE),
                   isTrueDirect=median(isTrueDirect,na.rm=TRUE)) 
  #0.7921273 submitted with 0.77712
  cor(custidRev)
  #creating the correlation matrix
  #fitting the data
  fit<-lm(data=custidRev,revenew ~log(custidRev$pageviews+22.86)+custidRev$isMobile+custidRev$isTrueDirect+
            custidRev$onebig+ custidRev$osearch/custidRev$visitnum + log(custidRev$pageviews/custidRev$visitnum + 0.005)+
            log(custidRev$pageviews/(custidRev$timesince+10^(-300))+0.5)+
            log(custidRev$timesince+0.5)) # try to delete isMobile+isTrueDirect
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
  #creating our cross validations hyperparameters
  model <- train(revenew~., data = custidRev, method = "lm",
                 trControl = train.control)
  #printing our CV's results
  print(model)
}
