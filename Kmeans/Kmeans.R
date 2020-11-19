setwd("D:/R/datasets")
cust_data <- read.csv("cluster.csv")
head(cust_data)
summary(cust_data)
cust_data <- cust_data[-c(1)]
head(cust_data)

#scaling is normalizing the data into same digits of numbers and distance
cust_data_f <- scale(cust_data)
head(cust_data_f)
dist.res=dist(cust_data_f,method = "euclidean")
hc <- hclust(dist.res,method = "complete")

#visualize the hclust
plot(hc,labels = FALSE,hang = -1)
rect.hclust(hc,k=3,border = 2:3)
install.packages("vegan")
install.packages("permute")
library(vegan)
library(permute)
library(lattice)
fit <- cascadeKM(scale(cust_data,center = TRUE,scale = TRUE),1,10,iter = 1000)
plot(fit,sortg = TRUE,grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters: ",calinski.best,"\n")

#also looking at the elbow chart
mydata <- cust_data

#determine the optimal cluster size based on within sum of squares 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for(i in 2:15) wss[i] <- sum(kmeans(mydata,centers = i)$withinss)

#plot the elbow chart to determine the optimal cluster
plot(1:15,wss,type = "b",xlab = "Number of Clusters",ylab = "Within group sum of squares",col="mediumseagreen",pch=12)

#from elbow chart it looks like 4 clusters although the change is low after 2.So we can either consider 2 or 4Lets stick with

##run the kmeans algorithm to generate to generate the clusters
k1 <- kmeans(cust_data_f,2)
k1

#see the clustering results
#fetch the group means for each variable
k1$centers

#fetch size/n of obs for the groups
k1$size






