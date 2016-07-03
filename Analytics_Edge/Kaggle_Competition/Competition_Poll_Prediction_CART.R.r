
PollTrainImp = read.csv("pollTrain_imputed.csv")

str(PollTrainImp)

library(caTools)

t = PollTrainImp$Party
t = ifelse(PollTrainImp$Party=="Democrat", 1, 0)

PollTrainImp$Party = t

table(PollTrainImp$Party)

split = sample.split(PollTrainImp$Party, SplitRatio = 0.7)

Train = subset(PollTrainImp,split==TRUE)
Test = subset(PollTrainImp, split == FALSE)
nrow(Train)
nrow(Test)

Train$Party= as.factor(Train$Party)
Test$Party = as.factor(Test$Party)

library(randomForest)

rfModel1 = randomForest(Party~.-USER_ID, data = Train, ntree = 50, nodesize = 10)

predForestTrain = predict(rfModel1, newdata = Train)

t= table(Train$Party, predForestTrain)
t

predForestTest = predict(rfModel1, newdata = Test)

t = table(Test$Party, predForestTest)
t

sum(diag(t))/nrow(Test)

PollTestImp = read.csv("pollTest_imputed.csv")
str(PollTestImp)

predForestFin = predict(rfModel1, newdata = PollTestImp)

table(predForestFin)

PredTestLabels = as.factor(ifelse(predForestFin == 1, "Democrat", "Republican"))

table(PredTestLabels)

MySubmission = data.frame(USER_ID = PollTestImp$USER_ID, Predictions = PredTestLabels)
str(MySubmission)


write.csv(MySubmission, "Submission2.csv", row.names=FALSE)

pollTrain = read.csv("train2016.csv")

str(pollTrain)

t = pollTrain
t = ifelse(pollTrain$Party=="Democrat", 1, 0)

pollTrain$Party = t

table(pollTrain$Party)

split = sample.split(pollTrain$Party, SplitRatio = 0.7)

Train = subset(pollTrain, split==TRUE)
Test = subset(pollTrain, split==FALSE)

Train$Party = as.factor(Train$Party)
Test$Party = as.factor(Test$Party)

rfModel2 = randomForest(Party ~ . , data = Train)


