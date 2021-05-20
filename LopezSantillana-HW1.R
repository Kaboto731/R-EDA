#Manuel Lopez Santillana
#IDA - Homework 1
#Library for skewness and kurtosis
library(moments)
library(plyr)
library(datasets)
#Problem 1 a------------------------------------------------
x = c(3,12,6,-5,0,8,15,1,-10,7)
#Problem 1 b------------------------------------------------
y=seq(min(x),max(x),length.out=10)

#Problem 1 c------------------------------------------------
# X
sum(x)
mean(x)
quantile(x)
sd(x)
var(x)
mad(x)
quantile(x,prob= seq(0,1,length =6))

# Y
sum(y)
mean(y)
quantile(y)
sd(y)
var(y)
mad(y)
quantile(y,prob= seq(0,1,length =6))
#Problem 1 d------------------------------------------------
sample(x,7,replace = TRUE)

#Problem 1 e------------------------------------------------
skewness(x)
moments(x)

#Problem 1 f------------------------------------------------
t.test(x-y)

#Problem 1 g------------------------------------------------
sort(x)
t.test(x,y,paired=TRUE)
#Problem 1 h------------------------------------------------
x2=x>=0
#Problem 1 i------------------------------------------------
x<-x[x2]

#Problem 2 a------------------------------------------------
college <- read.csv(file="/home/manuel/Desktop/Homework/IDA/college.csv",header=TRUE, sep=",")
#Problem 2 b------------------------------------------------
rownames(college) <- college [,1]
View (college)
college <- college[,-1]
#Problem 2 c i----------------------------------------------
summary(college)
#Problem 2 c ii---------------------------------------------
pairs(college[,1:10])
#Problem 2c iii---------------------------------------------
plot(data=college,Outstate~Private, main="Out of State Tuitions for Public and Private University",
     xlab="Private University", ylab="Out of State Tuition")
#Problem 2c iv-----------------------------------------------
#Creates a vector of size of rows of college and fills them with No
Elite <-rep ("No",nrow(college))
#If the top 10 percent in college is greater than 50, then we replace it with yes
Elite[college$Top10perc >50]<-"Yes"
#We then convert Elite into a factor
Elite <- as.factor(Elite)
#adds the Elite component to the data frame college
college <- data.frame(college,Elite)
#Problem 2c v------------------------------------------------
summary(Elite)
#There are 78 elite colleges
#Problem 2c vi-----------------------------------------------
plot(data=college,Outstate~Elite, main="Out of State for Colleges", xlab="Elite Colleges",ylab="Out of State College")
op <- par(mfrow=c(2,2))
hist(college$Top10perc,breaks = 30,main="Histogram of Top 10% of Colleges", xlab="Top 10 percent Tuition")
hist(college$F.Undergrad,breaks=15,main="Histogram of College Undergrad Tuition", xlab="Undergraduate College Tuition")
hist(college$Top25perc,breaks=20, main="Histogram of Top 25% of Colleges",xlab="Top 25 Pecent Tuition")
hist(college$Personal,breaks=5,main="Histogram of Personal Expense in College" ,xlab="Personal College Expenses")
#Problem 3 a-------------------------------------------------
?baseball
#Problem 3 b-------------------------------------------------
baseball$sf[baseball$year<1954]<-0

baseball$hbp[is.na(baseball$hbp)]<- 0
baseball<- subset(baseball,baseball$ab>=50)
#Problem 3 c-------------------------------------------------
obp<- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf))
#Problem 3 d-------------------------------------------------

baseball<- data.frame(baseball,obp)
#ordering the on base percentages
baseball<- baseball[order(obp),]
#checking the first 5 
head(baseball[,c("id","year","obp")],n=5)

#Problem 4 a-------------------------------------------------
#Loading in the quakes from the datasets
data(quakes,package="datasets")
str(quakes)
#Changing the output to display one graph
par(mfrow=c(1,1))
#Problem 4 b-------------------------------------------------
#plotting the magnitude vs depth
plot(quakes$depth,quakes$mag,xlab="Quake Depth", ylab="Quake Magnitude", main="Quake depth vs magnitude")
#Problem 4 c-------------------------------------------------
#quakeAvgDepth = aggregate(list("Average Depth"=quakes$depth),list("Magnitude"=quakes$mag),mean)
quakeAvgDepth= aggregate(quakes$depth, list(quakes$mag),mean)
#Problem 4 d-------------------------------------------------
#Changing the labels
names(quakeAvgDepth)<- c("Magnitude","Average Depth")
#Porblem 4 e-------------------------------------------------
#Plotting the graph
plot(quakeAvgDepth$Magnitude ~ quakeAvgDepth$Average.Depth,xlab="Average Depth",ylab="Magnitude",main="Average Depth vs Magnitude")
#Problem 4 f------------------------------------------------
#We see as the average depth increases the earthquake gets weaker
