# Assumptions of logistic regression
#1 The dependent variable should be dichotomous(binary)
#2 There should be no outliers in the data
#3 There should be no high correlations(multicollinearity) among the predictors

setwd('D:/R')
data <- read.csv("titanic.csv")
View(data)
head(data)

#check the missing values,mean,median,mode
summary(data)

#histogram to see the data is skewed
hist(data$age)
sum(is.na(data$age))
#replacing the NA values for variable 1 with 29
data$age[is.na(data$age)] = 29

#check if missing values are replaced
summary(data)

#have a look at the data
head(data)

#As seen in the data, some of the variables are categorical, which we need to create as a dummy variables first
data$female <- ifelse(data$sex=="female",1,0)
data$embarked_s <- ifelse(data$embarked=="S",1,0)
data$embarked_c <- ifelse(data$embarked=="C",1,0)

#checking the dummy variables
head(data)

#removing the categorical values
finaldata <- data[-c(3,4,9)]
head(finaldata)

#Since all the values are either continuous or binary we can now begin with the analysis process
#we first start with Univariate Analysis
bx = boxplot(finaldata$age)
bx$stats

# Getting the quantile values
quantile(finaldata$age, seq(0,1,0.02))

#Checking the quantile values
bx$stats

#Based on the boxplot outliers we are capping below 4% & above 96%
finaldata$age <- ifelse(finaldata$age>=52,52,finaldata$age)
finaldata$age<-ifelse(finaldata$age<=4,4,finaldata$age)
boxplot(finaldata$age)
boxplot(finaldata$fare)
bx$stats
quantile(finaldata$fare, seq(0,1,0.02))
finaldata$fare<-ifelse(finaldata$fare>=136,136,finaldata$fare)
boxplot(finaldata$fare)
library(car)
scatterplot(finaldata$age,finaldata$survived)
set.seed(222)
t=sample(1:nrow(finaldata),0.7*nrow(finaldata))
t_train=finaldata[t,]
t_test=finaldata[-t,]
library(car)
mod<- lm(survived ~ ., data=t_train)
t = vif(mod)
sort(t, decreasing = T)
mod1 <- glm(as.factor(survived) ~ ., family="binomial", data=t_train)
summary(mod1)
stpmod = step(mod1, direction = "both")
formula(stpmod)
summary(stpmod)
mod2 <- glm(as.factor(survived) ~ pclass + age + sibsp + female + embarked_c, family="binomial", data=t_train)
summary(mod2)
t_train$score=predict(mod2,newdata=t_train,type = "response")
head(t_train$score)
tail(t_train$score)
library(lattice)
library(ggplot2)
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
prediction<-ifelse(t_train$score>=0.6,1,0)
confusionMatrix(factor(prediction),factor(t_train$survived