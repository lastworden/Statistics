
stevens = read.csv("stevens.csv")

str(stevens)

summary(stevens)

library(caTools)

set.seed(3000)

split = sample.split(stevens$Reverse, SplitRatio = 0.70)

Train = subset(stevens, split == TRUE)

Test = subset(stevens, split == FALSE)

str(Train)

library(rpart)

.Library

library(rpart.plot)

StevensTree = rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket=25)

summary(StevensTree)

library(rpart.plot)

prp(StevensTree)

PredictCart = predict(StevensTree,newdata = Test,type = "class")

table(Test$Reverse,PredictCart)

(41+71)/(41+71+36+22)

library(ROCR)

PredictROC = predict(StevensTree,newdata = Test)

str(PredictROC)

summary(PredictROC)

pred = prediction(PredictROC[,2],Test$Reverse)

perf = performance(pred,"tpr","fpr")

plot(perf)

auc = as.numeric(performance(pred,"auc")@y.values)

auc

StevensTree2 = rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket=100)

prp(StevensTree2)

StevensTree3 = rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", minbucket=5)

prp(StevensTree3)

library(randomForest)

StevensForest = randomForest(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, nodesize = 25, ntree = 200)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest = predict(StevensForest, newdata = Test)

table(Test$Reverse,PredictForest)

(40+74)/(40+74+37+19)

set.seed(100)

StevensForest = randomForest(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest = predict(StevensForest, newdata = Test)

table(Test$Reverse,PredictForest)

(43+74)/(43+74+34+19)

set.seed(200)

StevensForest = randomForest(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest = predict(StevensForest, newdata = Test)

table(Test$Reverse,PredictForest)

(44+76)/(44+76+33+17)

getwd()

library(caret)

library(e1071)

numFolds = trainControl(method = "cv",number = 10)

cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

summary(cpGrid)

class(cpGrid)

train(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train, method = "rpart", trControl = numFolds, 
      tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data = Train, method = "class", cp=0.19)

PredictCV = predict(StevensTreeCV,newdata = Test, type = "class")

table(Test$Reverse,PredictCV)

(59+64)/(59+18+29+64)

prp(StevensTreeCV)


