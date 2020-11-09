setwd("D:/R/datasets")
diabetes <- read.csv("diabetes.csv")
head(diabetes)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
summary(diabetes)
str(diabetes)
diabetes$Outcome <- as.factor(as.character(diabetes$Outcome))
diabetes$Outcome <- factor(diabetes$Outcome,levels = c(0,1),labels = c("False","True"))
str(diabetes)
diab <- sample(1:nrow(diabetes),0.7*nrow(diabetes))
diabetes_train <- diabetes[diab,]
diabetes_test <- diabetes[-diab,]
diabetes_model <- rpart(Outcome~.,data = diabetes_train)
diabetes_model
plot(diabetes_model,margin = 0.1)
text(diabetes_model,use.n = TRUE,pretty = TRUE,cex=0.8)
pred_diab <- predict(diabetes_model,newdata = diabetes_test,type = "class")
pred_diab
confusionMatrix(table(pred_diab,diabetes_test$Outcome))
