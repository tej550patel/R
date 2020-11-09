setwd("D:/R/datasets")
voice <- read.csv("voice.csv")
head(voice)
str(voice)
rough <- glm(label~.,data = voice,family = "binomial")
summary(rough)
summary(voice)
voice$label <- ifelse(voice$label=="female",1,0)
str(voice)
voice$label <- factor(voice$label,levels = c(0,1))
View(voice)
library(caret)
'%ni%' <- Negate('%in%')
table(voice$label)
set.seed(100)
voice_data <- sample(1:nrow(voice),0.7*nrow(voice))
voice_train <- voice[voice_data,]
voice_test <- voice[-voice_data,]
table(voice_train$label)
down_voice <- downSample(x=voice_train[,colnames(voice_train)%ni%"label"],y=voice_train$label)
table(down_voice$Class)
up_voice <- upSample(x=voice_train[,colnames(voice_train)%ni%"label"],y=voice_train$label)
table(up_voice$Class)
log <- glm(Class~.,family = "binomial",data = down_voice)
summary(log)
stpmod = step(log,direction = "both")
formula(stpmod)
final_logistic <- glm(Class ~ Q25 + Q75 + kurt + sp.ent + sfm + meanfun + minfun + 
                        modindx,family = "binomial",data = down_voice)
prediction <- predict(final_logistic,newdata = voice_test,type = "response")
y_prediction <- ifelse(prediction > 0.5,1,0)
y_predict <- factor(y_prediction,levels = c(0,1))
y_final <- voice_test$label
mean(y_predict == y_final)



