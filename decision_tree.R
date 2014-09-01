setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\bike-sharing-data-mining")
require(Metrics)
require(lubridate)
require(randomForest)
require(caret)
df <- read.csv('Data/train.csv', head=T, stringsAsFactor=F)
test <- read.csv('Data/test.csv', head=T, stringsAsFactor=F)
str(df)
df$hour <- hour(df$datetime)
df$wd <- wday(df$datetime)
# test$hour <- hour(test$datetime)
# test$wd <- wday(test$datetime)
head(df)

trainIndex <- createDataPartition(df$count, p=.8, list=F,times=1)
train <- df[trainIndex,]
test <- df[-trainIndex,]
# preProcValues <- preProcess(df, method=c('center', 'scale'))
# preProcValues
# scaledTrain <- predict(preProcValue, df)


cvCtrl <- trainControl(method='repeatedcv',repeats=3, summaryFunction=twoClassSummary, classProbs=T)
set.seed(8888)
rpartTune <- train(count~.,data=train,method='rpart', tuneLength=30, metric='ROC',trControl=cvCtrl)

plot(rpartTune, scales=list(x=list(log=10)))

rpartPred <- predict(rpartTune, test)
# confusionMatrix(rpartPred,test$Class)
rpartProbs <- predict(rpartTune, test, type='probs')

library(pROC)
rpartROC <- roc(test$Count, rpartProbs[,'PS'], levels=rev(testProbs$Class))
plot(rpartROC,type='S', print.thres=.5)
rpartROC