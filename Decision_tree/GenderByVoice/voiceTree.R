setwd("D:/R/datasets")
voice <- read.csv("voice.csv")
head(voice)
str(voice)
v <- sample(1:nrow(voice),0.7*nrow(voice))
voice_train <- voice[v,]
voice_test <- voice[-v,]
voice_model <- rpart(label~meanfun,data = voice_train)
voice_model
plot(voice_model,margin = 0.1)
text(voice_model,use.n = TRUE,pretty = TRUE,cex=0.8)
pred_voice <- predict(voice_model,newdata = voice_test,type = "class")
pred_voice
confusionMatrix(table(pred_voice,voice_test$label))

