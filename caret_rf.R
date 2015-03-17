setwd("/Users/ivan/Work_directory/bike-sharing-data-mining/")
gc()
require(lubridate);require(caret)
# preprocessing
train <- read.csv('Data/train.csv', head=T, stringsAsFactor=F)
test <- read.csv('Data/test.csv', head=T, stringsAsFactor=F)
train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)
train$wd <- wday(train$datetime)
test$wd <- wday(test$datetime)
test$count <- 0
head(test);head(train)
train <- train[,-c(1,10,9)]
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
set.seed(888)  
fitControl <- trainControl(method = "adaptive_cv",number = 10,repeats = 5,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 12,alpha = 0.05,method = "gls",complete = TRUE))
# Grid <-  expand.grid()
# boosted tree model
g <- train(count ~ ., data=train, method='rf', trControl = fitControl, verbose=T, metric = "RMSE", tuneLength=12)
                 #tuneGrid = Grid)
g
pred1 <- predict(g, train)
confusionMatrix(pred1, as.integer(train$count))

# fit 2 poisson
eGrid <- expand.grid(.alpha = (1:10) * 0.1, .lambda = "all")
Control <- trainControl(method = "repeatedcv",repeats = 3,verboseIter =TRUE)
fit2 <- train(count~., data= train,
              method = "glmnet",
              tuneGrid = eGrid,
              trControl = Control)
# random forest
require(doMC)
registerDoMC(cores=2)
tc <- trainControl("repeatedcv", number=10, repeats=10, classProbs=TRUE, savePred=T)
tc <- trainControl("cv",10, classProbs=TRUE, savePred=T)
fit3_2 <- train(count ~ . , data=train, method='rf', trControl=tc, preProc=c("center", "scale"),verbose=T)
pred3 <- predict(fit3, train)
confusionMatrix(pred3, train$count)
    # test set preProcess

# predictions
predictions <- predict(fit3, test)
gbmPrinted <- data.frame(test$datetime, predictions)
names(gbmPrinted)<- c('datetime', 'count')
gbmPrinted[which(gbmPrinted$count < 0),'count'] <- 0
write.table(x=gbmPrinted, file='fr_cv_10.csv', sep=',', row.names=F)
