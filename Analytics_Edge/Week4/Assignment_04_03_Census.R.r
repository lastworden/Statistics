
census = read.csv("census.csv")

set.seed(2000)

library(caTools)

split = sample.split(census$over50k,SplitRatio = 0.6)

Train = subset(census,split == TRUE)
Test = subset(census,split == FALSE)

nrow(Train)
nrow(Test)

LogCensus = glm(over50k~., data = Train, family = binomial)

summary(LogCensus)

LogPredTest = predict(LogCensus, newdata = Test, type="response")

t = table(Test$over50k, LogPredTest>0.5)

t

sum(diag(t))/sum(t)

table(Test$over50k)

9713/(9713+3078)

library("ROCR")

ROCRPredTst = prediction(LogPredTest,Test$over50k)

as.numeric(performance(ROCRPredTst,"auc")@y.values)


perfLog = performance(ROCRPredTst,"tpr","fpr")
plot(perfLog)

library("rpart")
library("rpart.plot")
censusTree = rpart(over50k~., data = Train, method = "class")

prp(censusTree)

PredTreeTest = predict(censusTree, newdata = Test, type = "class")

t = table(Test$over50k, PredTreeTest)

t

sum(diag(t))/sum(t)

PredTreeTestProb = predict(censusTree, newdata = Test)

str(PredTreeTestProb)

PredTreeTestProb_g50k = PredTreeTestProb[,2]

PredTreeTestProb_g50k[1:10]

ROCRTree = prediction(PredTreeTestProb_g50k, Test$over50k)

as.numeric(performance(ROCRTree,"auc")@y.values)

perfTree = performance(ROCRTree,"tpr","fpr")
plot(perfTree)

set.seed(1)

trainSmall = Train[sample(nrow(Train), 2000), ]

set.seed(1)
library("randomForest")
censusForest = randomForest(over50k~., data = trainSmall)

censusForestPredTest = predict(censusForest, newdata = Test)

summary(censusForestPredTest)

t = table(Test$over50k, censusForestPredTest)
t

sum(diag(t))/sum(t)

vu = varUsed(censusForest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

varImpPlot(censusForest)

set.seed(2)

library(caret)
library(e1071)

numFolds = trainControl(method = "cv",number = 10)

str(numFolds)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

dim(cartGrid)

train(over50k~.,data = Train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

censusCPTree = rpart(over50k~., data = Train, method = "class",cp = 0.002)

censusCPTreePredTest = predict(censusCPTree, newdata = Test, type = "class")

summary(censusCPTreePredTest)

t=table(Test$over50k,censusCPTreePredTest)

t

sum(diag(t))/sum(t)

prp(censusCPTree)


