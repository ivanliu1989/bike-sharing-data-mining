setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\bike-sharing-data-mining")
library(mlbench)
data(Sonar)
head(Sonar)
str(Sonar)

library(caret)
set.seed(888)
inTraining <- createDataPartition(Sonar$Class, p=0.75, list=F)
training <- Sonar[inTraining,]
testing <-Sonar[-inTraining,]

# parameter tuning
fitControl <- trainControl(method='repeatedcv', # 10-fold CV
                           number = 10, 
                           repeats =10) # repeated ten times
# boosted tree model
gbmFit1 <- train(Class ~. , data=training, method='gbm', trControl = fitControl, verbose=F)
gbmFit1

trellis.par.set(caretTheme())
plot(gbmFit1)
