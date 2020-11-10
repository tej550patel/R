setwd("D:/R/datasets")
hr_data <- read.csv("HRdataset.csv")
library(randomForest)
require(caret)
library(pROC)
library(e1071)
colnames(hr_data)
hr_data1 <- hr_data[-c(9,10)]
str(hr_data1)
hr_data1$role_code <- as.factor(hr_data1$role_code)
hr_data1$salary.code <- as.factor(hr_data1$salary.code)
str(hr_data1)
summary(hr_data1)
splitIndex <- sample(1:nrow(hr_data1),0.7*nrow(hr_data1))
trainSplit <- hr_data1[splitIndex,]
testSplit <- hr_data1[-splitIndex,]
modelrf <- randomForest(as.factor(left)~.,data = trainSplit,do.trace=T)
modelrf
importance(modelrf)
varImpPlot(modelrf)
predrf_tr <- predict(modelrf,trainSplit)
predrf_test <- predict(modelrf,testSplit)
confusionMatrix(table(predrf_tr,trainSplit$left))
confusionMatrix(table(predrf_test,testSplit$left))
