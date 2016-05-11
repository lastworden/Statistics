
FluTrain = read.csv("FluTrain.csv")

str(FluTrain)

which.max(FluTrain$ILI)

FluTrain$Week[which.max(FluTrain$ILI)]

FluTrain$ILI[which.max(FluTrain$ILI)]

summary(FluTrain)

FluTrain$Week[which.max(FluTrain$Queries)]

hist(FluTrain$ILI)

plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 = lm(log(ILI)~Queries, data = FluTrain)

summary(FluTrend1)

c = cor(log(FluTrain$ILI),FluTrain$Queries)
c

c^2

FluTest = read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FluTest$Week

str(FluTest)

which(FluTest$Week == '2012-03-11 - 2012-03-17')

pred_ILI= PredTest1[11]
pred_ILI

PredTest1

ob_ILI = FluTest$ILI[11]
ob_ILI

(ob_ILI-pred_ILI)/ob_ILI

pred_diff = FluTest$ILI-PredTest1

rmse = sqrt(mean(pred_diff^2))

rmse

library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

summary(FluTrain$ILILag2)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

str(FluTrain)

FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data = FluTrain)

summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

FluTest$ILILag2 = coredata(ILILag2)

summary(FluTest$ILILag2)

nrow(FluTrain)

FluTest$ILILag2[1]
FluTest$ILILag2[2]
FluTest$ILILag2[3]

FluTest$ILILag2[1] = FluTrain$ILI[416]

FluTest$ILILag2[2] = FluTrain$ILI[417]

FluTest$ILILag2[1]
FluTest$ILILag2[2]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

sqrt(mean((PredTest2-FluTest$ILI)^2))


sqrt(mean((PredTest1-FluTest$ILI)^2))


