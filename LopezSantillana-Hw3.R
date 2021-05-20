# Manuel Lopez Santillana
#  
# IDA 

library(mlbench)
library(ggplot2)
library(ggbiplot)
library(MASS)
library(Rtsne)
library(caret)

?Glass

 data("Glass")
 sapply(Glass, class)
 #Problem 1 a i
 #Since Type is a factor it is removed from the data set
 mydata <- Glass[,c(1,2,3,4,5,6,7,8,9)]
 #Correlation matrix
 Cormat <- cor(mydata)
 
 #Problem 1 a ii
 #Now for eigenvalues and eigenvectors
 
 Cormateigen <- eigen(Cormat)
 
 #Problem 1 a iii 
  PrinCompGlass <- prcomp(mydata, scale=TRUE)
  
  #Problem iv
  #They are the same just the sign is flipped, however the sign is 
  #arbitrary in this case, since it is just a signifier, the only difference
  # is one algorithm chose (+) as its signifier and the other chose (-)
  
  #Problem 1 a v
  
 ma =  t(PrinCompGlass$rotation[,2]) %*% PrinCompGlass$rotation[,1]
  
  #  ma is not 0 due to machine epsion
 
 #Part 1 b i
 #visualizations of Prcomp
 ggbiplot(PrinCompGlass, groups = Glass$Type)
 #info of ggplot and lda
 ??ggbiplot
 ??lda
 Typer = Glass[,c(10)]
 #Performing LDA by type
 mylda = lda(Type ~ . , data = Glass)
 preproc.paramGlass <- Glass %>% preProcess(method = c("center", "scale")) 
 
 # Transform the data using the estimated parameters 
 transformedGlass <- preproc.paramGlass %>% predict(Glass)
 #Creating predictions based on LDA
 predictions = predict(mylda,Glass)
 #Changing margins
   par(mar=c(2,2,2,2))
   #First discriminant plotting
  ldahist(data=predictions$x[,1],g=Glass$Type)
  #Second discriminant plotting
ldahist(data = predictions$x[,2], g= Glass$Type)


#Part 2
#reading in the data
Facebook = read.csv("FB-metrics.csv")
#Using all of the 11 features given
mydata2 <- Facebook[,c(8,9,10,11,12,13,14,15,16,17,18)]
#Breaking it down to pairs, depending on the graph you want to 
#choose just use run to select it
mydata3 <- Facebook[,c(13,14)]
mydata3 <- Facebook[,c(8,14)]
mydata3 <- Facebook[,c(9,10)]
mydata3 <- Facebook[,c(13,14)]
#Performing PCA
prcFace = prcomp(mydata3,scale=TRUE)
#creating a few plots to see what works
ggbiplot(prcFace, groups=Facebook$Type)
ggbiplot(prcFace, groups=Facebook$Category)
ggbiplot(prcFace, groups=Facebook$Page.total.likes)
ggbiplot(prcFace, groups=Facebook$Paid)
ggbiplot(prcFace, groups=Facebook$Post.Weekday)
#Running R's t-sne on Facebook
rtsneFace<-Rtsne(Facebook)
#Running it on pairs
rtsneFace2<-Rtsne(mydata3)
#Creating a dataframe with the data.
df<-data.frame(x=rtsneFace$Y[,1],y=rtsneFace$Y[,2], type=rtsneFace2$costs)
#plotting it
ggplot(data=df,aes(x=x,y=y,group=Facebook$Post.Month,color=Facebook$Type))+geom_point()




