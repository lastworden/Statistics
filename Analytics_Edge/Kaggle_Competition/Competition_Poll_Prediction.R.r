
pollTrain = read.csv("train2016.csv")

str(pollTrain)

pollTest = read.csv("test2016.csv")

summary(pollTrain$Party)

names(pollTrain)

table(pollTrain$Party)

2951/(2951+2617)

library(caTools)

split = sample.split(pollTrain, SplitRatio = 0.7)
split[1:10]

nrow(pollTrain)
Train = pollTrain[split,setdiff(names(pollTrain), "USER_ID")]
nrow(Train)
Test = pollTrain[!split,setdiff(names(pollTrain), "USER_ID")]
nrow(Test)

setdiff(c("12","as","de"), "de")

setdiff(names(pollTrain), "USER_ID")

summary(Train$Party)

Train$Party = as.numeric(Train$Party=="Democrat")
Test$Party = as.numeric(Test$Party=="Democrat")

table(Train$Party)
table(Test$Party)

LogModel1 = glm(Party~., data = Train, family = binomial)

sum(Train$Gender=="")
nrow(Train)

summary(LogModel1)

PrdTrain = predict(LogModel1,newdata = Train, type = "response")

t = table(Train$Party,PrdTrain>0.5)
t

sum(diag(t))/sum(t)

table(Train$Party)

2038/(2038+1828)

PrdTest = predict(LogModel1,newdata = Test, type = "response")

t = table(Test$Party,PrdTest>0.5)
t

sum(diag(t))/sum(t)

prdFinTest = predict(LogModel1,newdata = pollTest, type = "response")

prdFinTest2 = prdFinTest

summary(prdFinTest>0.5)

prdFinTest2[is.na(prdFinTest2)] = 1

dim(as.vector(prdFinTest2))

sum(is.na(prdFinTest2))

length(prdFinTest2)

t = table(prdFinTest2>0.5)
t

prdFinTest2 = prdFinTest2>0.5

prdFinTest2[1:10]

df = data.frame(pollTest$USER_ID,as.numericprdFinTest2>0.5)

Predictions = prdFinTest2

Predictions[prdFinTest2] = "Democrat"

Predictions[!prdFinTest2] = "Republican"

table(Predictions)

df = data.frame(pollTest$USER_ID,Predictions)

str(df)

names(df) = c("USER_ID","Predictions")

str(df)

summary(df)

table(pollTrain$Party)

write.csv(df,"submit1.csv",row.names=FALSE)


