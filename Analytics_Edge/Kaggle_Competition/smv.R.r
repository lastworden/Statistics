
library(rpart)

library(e1071)

TrainImp = read.csv("pollTrain_imputed.csv")
TestImp = read.csv("pollTest_imputed.csv")
TrainImp$USER_ID = NULL

str(TrainImp)

TrainImp$Party = as.factor(ifelse(TrainImp$Party=="Democrat",1,0))

table(TrainImp$Party)

library(caTools)

split = sample.split(TrainImp$Party, SplitRatio = 0.75)

Train = subset(TrainImp, split == TRUE)
Test = subset(TrainImp, split == FALSE)

table(Train$Party)
nrow(Train)

svmModel = svm(Party~., data = Train, cost=1000, gamma = 10, type = "C")

summary(svmModel)

predTest = predict(svmModel, newdata = Train)


table(predTest)





t = table(Test$Party,predTest)
t

summary(predTest)

?svm


