setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\bike-sharing-data-mining")
gc()
require(lubridate)
require(caret)
# preprocessing
train <- read.csv('Data/train.csv', head=T, stringsAsFactor=F)
test <- read.csv('Data/test.csv', head=T, stringsAsFactor=F)
train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)
train$wd <- wday(train$datetime)
test$wd <- wday(test$datetime)
test$count <- 0
head(test)
head(train)
train <- train[,-1]
train <- train[,-10]
train <- train[,-9]
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
train$hour <- as.factor(train$hour)
train$wd <- as.factor(train$wd)
test$season <- as.factor(test$season)
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)
test$hour <- as.factor(test$hour)
test$wd <- as.factor(test$wd)


# parameter tuning
set.seed(1234)  
fitControl <- trainControl(method='cv', # 10-fold CV
                           10) # repeated ten times
Grid <-  expand.grid(
    n.trees = c(1000),
    interaction.depth = c(22) ,
    shrinkage = 0.2)
# boosted tree model
gbmFit1 <- train(as.factor(count) ~ ., data=train, method='gbm', trControl = fitControl, verbose=T,
                 tuneGrid = Grid)
gbmFit1
pred1 <- predict(gbmFit1, train)
confusionMatrix(pred1, as.integer(train$count))

# fit 2 poisson
fit2 <- train(as.factor(count) ~ ., data=train, method='glm', verbose=T, family='poisson')

# random forest
fit3 <- train(as.factor(count) ~ . , data=train, method='rf', verbose=T, trControl=fitControl)

# predictions
predictions <- predict(fit2, test)
gbmPrinted <- data.frame(test$datetime, predictions)
names(gbmPrinted)<- c('datetime', 'count')
gbmPrinted[which(gbmPrinted$count < 0),'count'] <- 0
write.table(x=gbmPrinted, file='gbm_cv.csv', sep=',', row.names=F)
