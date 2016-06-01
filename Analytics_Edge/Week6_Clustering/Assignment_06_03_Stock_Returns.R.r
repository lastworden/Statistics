
stocks = read.csv("StocksCluster.csv")

str(stocks)

summary(stocks$PositiveDec)

sum(stocks$PositiveDec)/nrow(stocks)

which.max(cor(stocks))

colnames(stocks)

sort(cor(stocks), decreasing = TRUE)

cor(stocks)

sort(colMeans(stocks))

library(caTools)

set.seed(144)
split = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, split == TRUE)
stocksTest = subset(stocks, split == FALSE)

StocksModel = glm(PositiveDec~.,data = stocksTrain, family = binomial)

stocksPredTrain = predict(StocksModel, type = "response")

stocksPredTrain[1:10]

t =table(stocksTrain$PositiveDec, stocksPredTrain>0.5)
t

sum(diag(t))/nrow(stocksTrain)

stocksPredTest = predict(StocksModel, newdata = stocksTest, type = "response")

t =table(stocksTest$PositiveDec, stocksPredTest>0.5)
t

sum(diag(t))/nrow(stocksTest)

table(stocksTest$PositiveDec)

1897/(1897+1577)

limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)

mean(normTest$ReturnJan)

set.seed(144)
km = kmeans(normTrain, centers = 3)

table(km$cluster)

library(flexclust)

km.kcca = as.kcca(km,normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata = normTest)

table(clusterTest)

table(clusterTrain)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

StocksModel1 = glm(PositiveDec~., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec~., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec~., data = stocksTrain3, family = binomial)

summary(StocksModel1)

summary(StocksModel2)

summary(StocksModel3)

predictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
predictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
predictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

t = table(stocksTest1$PositiveDec, predictTest1>0.5)
t
sum(diag(t))/sum(t)

t = table(stocksTest2$PositiveDec, predictTest2>0.5)
t
sum(diag(t))/sum(t)

t = table(stocksTest3$PositiveDec, predictTest3>0.5)
t
sum(diag(t))/sum(t)

AllPredictions = c(predictTest1>0.5,predictTest2>0.5,predictTest3>0.5)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec,stocksTest3$PositiveDec)

t = table(AllOutcomes, AllPredictions)
t
sum(diag(t))/sum(t)


