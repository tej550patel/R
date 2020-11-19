setwd("D:/R/datasets")
heart <- read.csv("heart.data",header = FALSE,sep = ',')
head(heart)
str(heart)
summary(heart)
intrain <- sample(1:nrow(heart),0.7*nrow(heart))
training <- heart[intrain,]
testing <- heart[-intrain,]
dim(training)
dim(testing)
anyNA(heart)
summary(training)
sum(is.na(heart))
heart[heart=="?"] <- NA
summary(heart)
training$V14 <- factor(training$V14)
library(caret)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trctrl
library(kernlab)
svm_linear <- train(V14~.,data = training,method = "svmLinear",
                    trcontrol=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_linear
test_pred <- predict(svm_linear, newdata = testing)
test_pred
confusionMatrix(table(test_pred, testing$V14))
