#Manuel Lopez - Santillana
#Homework 2
#IDA
#Installing tidyverse
install.packages("tidyverse")

#Used libraries:
library(tidyverse)
library(VIM)
library(mice)
library(Amelia)
library(naniar)
library(UpSetR)
library(plyr)
#Beginning of Assignment---------------------------------------
#Problem 1 a

ggplot2::mpg

?mpg

#3.2.4 Exercise 4 plot of Hwy vs Cyl---------------------------------------
ggplot(data = mpg) + geom_point(mapping = aes(x=hwy, y=cyl))+xlab('Highway miles per gallon')+ylab("Number of cylinders")

#3.2.4 Exercise 5 Making a scatterplot of Class vs Drv---------------------------------------
ggplot(data = mpg) + geom_point(mapping = aes(x=class, y=drv))
#This is not useful because certain car classes can have multiple drvs, this does not 
#Tell much information other than these combinations exist
View(mpg)
#3.3.1 Exercise 3---------------------------------------------
#Map a continuous variable to color, size, and shape.
#how do these aesthetics 
#behave differently for categorical vs. continuous variables?
#Coloring for displ
ggplot(data = mpg)+geom_point(mapping = aes(x = hwy,y = cty, color = displ))+xlab('Highway miles per gallon')+ylab("city miles per gallon")+ggtitle("Miles Per Gallon vs Engine Displacement in Litres")
#Size for displ
ggplot(data = mpg)+geom_point(mapping = aes(x = hwy,y = cty, size = displ))+xlab('Highway miles per gallon')+ylab("city miles per gallon")+ggtitle("Miles Per Gallon vs Engine Displacement in Litres")
#Shape for displ
ggplot(data = mpg)+geom_point(mapping = aes(x = hwy,y = cty, shape = displ))+xlab('Highway miles per gallon')+ylab("city miles per gallon")+ggtitle("Miles Per Gallon vs Engine Displacement in Litres")
#A continuous variable cannot be mapped to shape
#3.3.1 Exercise 4
#What happens when you map a variable to multiple aesthetics?
#Creation of plot
ggplot(data = mpg)+geom_point(mapping = aes(x = hwy,y = cty, color = displ, size=displ))+xlab('Highway miles per gallon')+ylab("city miles per gallon")+ggtitle("Miles Per Gallon vs Engine Displacement in Litres")
#3.3.1 Exercises 6
#Creation of plot
ggplot(data = mpg)+geom_point(mapping = aes(x = hwy,y = cty, color = displ<5, size=displ))+xlab('Highway miles per gallon')+ylab("city miles per gallon")+ggtitle("Miles Per Gallon vs Engine Displacement in Litres")
#3.5.1 Exercises #4
#Creation of plot
ggplot(data = mpg)+ geom_point(mapping = aes(x=displ, y=hwy))+ facet_wrap(~class, nrow =2)+ylab('Highway miles per gallon')+xlab("Engine Displacement")
#Problem 1 part b
ggplot(data = mpg,mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(x=displ, y=hwy), position = 'jitter', alpha=I(0.20))+
  facet_wrap(~drv, nrow =1)+xlab("Displacement")+ylab("Highway MPG")+
  geom_smooth(mapping = aes(x=displ, y=hwy), method="lm",color="black",se=F)+
  geom_smooth(mapping = aes(x=displ, y=hwy), method= "loess")

#Problem 2 a
#The four different distributions of 500:normal, geomteric,uniform and exponential
a<- rnorm(n=500,mean=0,sd=1)
b<- rgeom(n=500,prob=0.5)
c<- runif(n=500, min=0,max=5)
d<- rexp(n=500,rate=1)
df <- data.frame(a,b,c,d)
View(df)
#making df2 and being cool using ddplyr (awhh yeah...) 
df2<-gather(data=df,key = "groupVar",value = "value")
View(df2)
#Problem 2 b
ggplot(df2,aes(x=value,fill=groupVar))+geom_density(alpha=I(0.25))
#Problem 3
#Reading in housing data and viewing the format
Housing = read.csv("housingData.csv", header=TRUE)
View(Housing)
#Boxplot of Overall Condition based on Building
ggplot(Housing,aes(x=BldgType,y=OverallCond))+geom_boxplot()+xlab("Building Type")+ylab("Overall Condition")+ggtitle("Overall Condition based on Building Type")
#Facet Plotting Housing by year sold,building type, and external qualities
ggplot(Housing, aes(YrSold))+ geom_bar(aes(fill=BldgType), width = 0.5)+facet_wrap(~ExterQual)+xlab("Year Sold")+ggtitle("Sold Houses by External Quality")
#Scatter Plotting Condition and Quality of houses based on year built
ggplot(Housing,aes(x=OverallCond,y=OverallQual))+geom_point(position='jitter',aes(color=YearBuilt))
#Scatter Plotting External Quality and Garage Quality along with year built
ggplot(Housing,aes(x=ExterQual,y=GarageQual))+geom_point(position='jitter',aes(color=YearBuilt))+xlab("External Quality")+ylab("Garage Quality")+ggtitle("The Effects of Building Materials on Garages")
#Scatter Plotting the Built Year, Year Sold and what type of building was sold
ggplot(Housing,aes(x=YrSold,y=YearBuilt))+geom_point(position='jitter',aes(color=BldgType))+xlab("Year House Was Sold")+ylab("Year the House was Built")+ggtitle("Housing Construction Market")
#Scatter Plotting Building Materials with Quality
ggplot(Housing, aes(x=Exterior1st,y=Exterior2nd))+geom_point(position='jitter',aes(color=ExterQual))+xlab("Exterior Covering on House")+ylab("2nd Exterior Covering on House")+ggtitle("Exterior Building Materials Vs Quality")
#Plotting BasementQuality with Foundation and Finished Area
ggplot(Housing, aes(x=BsmtExposure,y=BsmtQual))+geom_point(position='jitter', aes(color=Foundation))+xlab("Basement Exposure")+ylab("Basement Quality")+ggtitle("Basement Quality, Foundation, and Exposure")
#Plotting trends with Year built year sold and basement quality
ggplot(Housing,aes(x=YearBuilt, y=YrSold))+
  geom_point(position = 'jitter', alpha=I(0.20))+
  facet_wrap(~BsmtQual, nrow =1)+xlab("Year Built")+ylab("Year Sold")+
  ggtitle("Housing Market based on Basement Quality")+
  geom_smooth( method="lm",color="black",se=F)+
  geom_smooth(method= "loess")
ggplot(Housing,aes(x=SalePrice,fill=Foundation))+geom_density(alpha=I(0.25))+xlab("Sale Price")+ggtitle("Sale Price for Different Foundations")

#Problem 4 a
?freetrade
data("freetrade", package="Amelia")
df3 <-freetrade
#showing the missing data
missmap(df3,xlab="Categories",ylab="indices")

#Problem 4 b 
TariffandCountry<-df3[,c("tariff","country")]
NATariff<- ddply(TariffandCountry,c("country"),summarise, cNA=sum(is.na(tariff)))
#t-test 
t.test(NATariff$cNA, mu=0)
#chisquared test
chisq.test(NATariff$cNA)
#Creating data without nepal
NONepal<-NATariff[(NATariff$country!="Nepal"),]

#t-test
t.test(NONepal$cNA, mu=0)
#chisquared test
chisq.test(NONepal$cNA)
View(NATariff)
#Creating data without the phillippines
NOPhil<-NATariff[(NATariff$country!="Philippines"),]
#t-test
t.test(NOPhil$cNA, mu=0)
#chisquared test
chisq.test(NOPhil$cNA)




