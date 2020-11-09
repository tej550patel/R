setwd("D:/R")
Data <- read.csv("Regression.csv")
summary(Data)
hist(Data$Age)
sum(is.na(Data$Age))
Data$Age[is.na(Data$Age)]=38
summary(Data)

#converting categorical values to numeric
Data$Job.type_employed<-as.numeric(Data$Job.Type=="Employed")
Data$Job.type_retired<-as.numeric(Data$Job.Type=="Retired")
Data$Job.type_unemployed<-as.numeric(Data$Job.Type=="Unemployed")
Data$Married_y<-as.numeric(Data$Marital.Status=="Yes")
Data$Education_secondary<-as.numeric(Data$Education=="Secondry")
Data$Education_gra<-as.numeric(Data$Education=="Graduate")
Data$Metro_y<-as.numeric(Data$Metro.City=="Yes")

#checking the dummy variables
head(Data)

#removing the categorical colomns
final_data <- Data[-c(2,3,4,5)]

set.seed(56952)

#check our final data
head(final_data)

#we first start with Univariate Analysis,outlier detection 
par(mfrow=c(1,2))
bx = boxplot(final_data$Age)

#check the distribution of variable Age
quantile(final_data$Age,seq(0,1,0.02))

#checking the quantile values
bx$stats
summary(Data)

#Since the 98th percentile is 57, we cap the outliers with the same value
final_data$Age <- ifelse(final_data$Age>60,57,final_data$Age)

#box plot to again check the outliers
boxplot(final_data$Age)

#check the outlier for other variable sign in sice days
boxplot(final_data$Signed.in.since.Days.)

#outlier treatment for singned in since
quantile(final_data$Signed.in.since.Days.,seq(0,1,0.02))

#Thus capping the value of values less than 45 with 48(8 percentile)
final_data$Signed.in.since.Days. <- ifelse(final_data$Signed.in.since.Days.<45,48,final_data$Signed.in.since.Days.)
boxplot(final_data$Signed.in.since.Days.)

hist(final_data$Purchase.made,main = "Dependent")

install.packages("car")

#lets check do the bi-variate analysis to check the relationship between variables
library(car)
scatterplot(final_data$Age,final_data$Purchase.made)

#sice we are done with EDA lest check the corelation
cor(final_data)

#checking the multi-colinearity
d1 <- lm(Purchase.made~.,data = final_data)
vif(d1)

#Graduation was higly co-linear with the other variables,lest verify once again
step(d1)

#since all the variable are not below the threshold of 5,we need to correct the model,lets remove the
final_data2 <- lm(Purchase.made~Age + Signed.in.since.Days. + Married_y + Job.type_retired + Job.type_unemployed + Education_gra + Metro_y, data = final_data)
vif(final_data2)

final_data3 <- lm(Purchase.made~Age + Signed.in.since.Days. + Married_y + Job.type_retired + Job.type_unemployed + Education_gra + Metro_y, data = final_data)
summary(final_data3)

final_data4 <- lm(Purchase.made~ + Signed.in.since.Days. + Married_y  + Job.type_unemployed + Education_gra + Metro_y, data = final_data)





