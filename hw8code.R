#Manuel Lopez - Santillana
#Homework 8 IDA 
library(datasets)
library(tidyverse)
library(cluster)
library(factoextra)
library(fpc)
library(tidyverse)
library(ggplot2)
library(reshape2)
mydata= data("ChickWeight")
?ChickWeight
head(mydata)
#kmeans
ChickWeight$weight <- as.numeric(ChickWeight$weight)
ChickWeight$Time <- as.numeric(ChickWeight$Time)
ChickWeight$Chick <- as.numeric(ChickWeight$Chick)
ChickWeight$Diet <- as.numeric(ChickWeight$Diet)
chickmatrix <- as.matrix(ChickWeight[,1:4])
chickcluster<- kmeans(chickmatrix,centers=4)
chickcluster$cluster <- as.factor(chickcluster$cluster)
#plotting of kmeans
ggplot(ChickWeight, aes(Chick, weight, color=chickcluster$cluster))+geom_point()
ChickWeight$cluster<- as.factor(chickcluster$cluster)
names(ChickWeight)[5] <- "cluster"
melted<- melt(ChickWeight[c(1:4,5)],id.vars="cluster")
ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)



#dbscan

chickmatrix2 <- ChickWeight %>% select(3,1)

res <-dbscan(chickmatrix2,eps=6, MinPts = 5, showplot=1)
ChickWeight$cluster<- as.factor(res$cluster)
names(ChickWeight)[5] <- "cluster"
melted<- melt(ChickWeight[c(1:4,5)],id.vars="cluster")
ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)

#fviz_cluster(res,data=chickmatrix2,show.clust.cent=TRUE)
#mediods

pam.res<-pam(chickmatrix2,4)
print(pam.res)
pam.res$medoids
pam.res$clustering
ChickWeight$cluster<- as.factor(pam.res$clustering)
#plot of mediods
fviz_cluster(pam.res,show.clust.cent=TRUE)
names(ChickWeight)[5] <- "cluster"
melted<- melt(ChickWeight[c(1:4,5)],id.vars="cluster")
ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)

