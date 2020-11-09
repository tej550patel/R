setwd("D:/R/datasets")
mushrooms <- read.csv("mushrooms.csv")
head(mushrooms)
summary(mushrooms)
str(mushrooms)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
mushrooms <- mushrooms[c(-17)]
table(mushrooms$class,mushrooms$odor)
number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]
par(mar=c(10,2,2,2))
str(mushrooms)
barplot(number.perfect.splits,main = "Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,color="wheat")
train <- sample(1:nrow(mushrooms),0.8*nrow(mushrooms))
mushrooms_train <- mushrooms[train,]        
mushrooms_test <- mushrooms[-train,]
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
tree <- rpart(class~.,
              data = mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")
rpart.plot(tree,nn=TRUE)
pred <- predict(object=tree,mushrooms_test[-1],type="class")
t <- table(mushrooms_test$class,pred)
confusionMatrix(t)
