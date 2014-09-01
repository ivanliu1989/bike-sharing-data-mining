setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\bike-sharing-data-mining")
require(Metrics)
require(lubridate)
require(randomForest)
require(caret)
# preprocessing
train <- read.csv('Data/train.csv', head=T, stringsAsFactor=F)
test <- read.csv('Data/test.csv', head=T, stringsAsFactor=F)
train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)
train$wd <- wday(train$datetime)
test$wd <- wday(test$datetime)
test$count <- 0

# parameter tuning
fitControl <- trainControl(method='repeatedcv', # 10-fold CV
                           number = 10, 
                           repeats =10) # repeated ten times
# boosted tree model
gbmFit1 <- train(count ~ season + holiday + weather + wd+ hour + temp + atemp
                 + humidity + windspeed,
                 data=train, method='rf', trControl = fitControl, verbose=F)
gbmFit1
nrow(test)
trellis.par.set(caretTheme())
plot(gbmFit1)
predictions <- predict(gbmFit1, test)
gbmPrinted <- data.frame(test$datetime, predictions)
names(gbmPrinted)<- c('datetime', 'count')
write.table(x=gbmPrinted, file='gbm.csv', sep=',', row.names=F)
