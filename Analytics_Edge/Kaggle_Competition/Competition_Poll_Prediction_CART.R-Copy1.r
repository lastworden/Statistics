
PollTrainImp = read.csv("pollTrain_imputed.csv")

str(PollTrainImp)

library(caTools)

t = PollTrainImp$Party
t = ifelse(PollTrainImp$Party=="Democrat", 1, 0)

PollTrainImp$Party = t

table(PollTrainImp$Party)

PollTrainImp$Party = as.factor(PollTrainImp$Party)

table(PollTrainImp$Party)

library(caret)

library(e1071)

numFolds = trainControl(method = "cv",number = 10)

cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

summary(cpGrid)

train(Party~ . - USER_ID,data = PollTrainImp, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

CARTModel1 = rpart(Party~. - USER_ID, data = PollTrainImp, method = "class", cp = 0.02)

PollTestImp = read.csv("pollTest_imputed.csv")

Predictions = predict(CARTModel1, newdata = PollTestImp, type = "class")

table(Predictions)

PredTestLabels = as.factor(ifelse(Predictions == 1, "Democrat", "Republican"))

table(PredTestLabels)

MySubmission = data.frame(USER_ID = PollTestImp$USER_ID, Predictions = PredTestLabels)
str(MySubmission)
write.csv(MySubmission, "SubmissionCART_CV.csv", row.names=FALSE)

predTrain = predict(CARTModel1, newdata = PollTrainImp, type = "class")

t= table(PollTrainImp$Party,predTrain)
t

sum(diag(t))/nrow(PollTrainImp)

pollTrain = read.csv("train2016.csv")

str(pollTrain)

pollTrain$Party = ifelse(pollTrain$Party=="Democrat", 1, 0)

table(pollTrain$Party)

split = sample.split(pollTrain$Party, SplitRatio = 0.7)

Train = subset(pollTrain,split == TRUE)
Test = subset(pollTrain, split == FALSE)

numFolds = trainControl(method = "cv",number = 10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
summary(cpGrid)

pollTrain$Party = as.factor(pollTrain$Party)

train(Party~ . - USER_ID,data = pollTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

CARTModel2 = rpart(Party~. -USER_ID, data = pollTrain, method = "class", cp = 0.04)

pollTest = read.csv("test2016.csv")

PredCartTest = predict(CARTModel2, newdata = pollTest, type = "class")

table(PredCartTest)

PredTestLabels2 = as.factor(ifelse(PredCartTest==1, "Democrat", "Republican"))
table(PredTestLabels2)

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):




MySubmission = data.frame(USER_ID = pollTest$USER_ID, Predictions = PredTestLabels2)

write.csv(MySubmission, "SubmissionRPART_NO_IMP_CV.csv", row.names=FALSE)


