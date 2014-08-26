setwd(choose.dir())
train <- read.csv('Data/train.csv', head=T, stringsAsFactor=F)
test <- read.csv('Data/test.csv', head=T, stringsAsFactor=F)
str(train)
str(test)

require(Metrics)
require(lubridate)
require(randomForest)

train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)
head(train)

train$wd <- wday(train$datetime)
test$wd <- wday(test$datetime)

test$count <- 0

fit <- randomForest(count ~ season + holiday + weather + wd+ hour + temp + atemp
                    + humidity + windspeed , data=train, ntree = 700, importance=TRUE)
png('rf.png')
varImpPlot(fit)
dev.off()

pred <- predict(fit, test)
submit <- data.frame(datetime=test$datetime, count=pred)
write.csv(submit, file='rf.csv',row.names=F)

