#Manuel Lopez - Santillana
#IDA- Homework 4
#Tuesday October 1

library(ggplot2)
library(mlbench)
library(car)
library(EnvStats)
library(VIM)
library(mice)
library(tidyverse)
library(backports)
?Glass
#Grouping graphs together in 3's
par(mfrow=c(1,3)) 
#1
hist(Glass$Fe)#Large Skew
hist(Glass$Mg)#Large skew
hist(Glass$Ca)#Skew

#1a
symbox(Glass$Fe,powers=c(-10,-2,-0.5,0,0.5,2))
symbox(Glass$Mg,powers=c(-2,-0.75,-0.5,0,0.5,2))
symbox(Glass$Ca,powers=c(-8,-2,-0.75,-0.5,0,0.5,2))
#Shifting the values so that we can evaluate all values with boxcox
NGlassMg<-Glass$Mg+0.0001
NGlassFe<-Glass$Fe+0.0001
#1b
boxcox(NGlassFe, optimize = TRUE)
boxcox(NGlassMg,optimize = TRUE)
boxcox(Glass$Ca,optimize=TRUE)
#Checking for lambda's true optimal value, since it was at max
boxcox(NGlassMg,optimize = TRUE,lambda=c(-2,10))

#normalized histograms
hist((Glass$Ca**0.8593591-1)/0.8593591) 
hist((NGlassMg**5.060733-1)/5.060733)
hist((NGlassFe**0.5953855-1)/0.5953855)


#2a 
#checking ratio of missing data
msleep %>% select_if(is.numeric) %>% mutate_all(is.na) %>% summarise_all(mean)
#grab numeric
numasleep <- msleep %>% select_if(is.numeric)
aggr_plot <- aggr(numasleep, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(nonasleep), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
md.pattern(numasleep)
#2 c i
#Since we are choosing linear regression we use norm.nob 
imput<-mice(numasleep,m=6,method='norm.nob')
#Looking at overall structure
str(imput)
#Checking means and variances
imput$chainMean
imput$chainVar
imput$predictorMatrix
summary(imput)
#plotting
#ii
#step 3
fiter<-with(imput, lm(log(sleep_rem/sleep_total)~log(sleep_cycle)+log(brainwt)))
#Step 4
ester<-pool(fiter)
summary(ester)
##2 c iii) Using a complete set:
#omitting data
nonasleep = na.omit(numasleep)
#making the linear model
fulldata <- lm(data=nonasleep,log(sleep_rem/sleep_total)~log(sleep_cycle)+log(brainwt))
#providing a summary of the data with p values
summary(fulldata)


#2 c iv) Using our different models
#imputing with the method midastouch
imput2<-mice(nonasleep,m=6,method='midastouch')
#looking at predictor matrix to see what is used for imputed values
imput2$predictorMatrix
#fitting the data
fiters2<-with(imput2,lm(log(sleep_rem/sleep_total)~log(sleep_cycle)+log(brainwt)))
#pooling it
est2<-pool(fiters2)
#providing a summary of the values and p values
summary(est2)
#imputing with the method cart
imput3<-mice(numasleep,m=6,method='cart')
#looking at predictor matrix to see what is used for imputed values
imput3$predictorMatrix
#fitting the data
fiters3<-with(imput3,lm(log(sleep_rem/sleep_total)~log(sleep_cycle)+log(brainwt)))
#pooling it
est3<-pool(fiters3)
#providing a summary of the values and p values
summary(est3)