
pollTrain = read.csv("train2016.csv",na.strings=c("","NA"))

str(pollTrain)

pollTest = read.csv("test2016.csv",na.strings=c("","NA"))#, na.strings=c("","NA"))

summary(pollTrain$Party)

names(pollTrain)

table(pollTrain$Party)

2951/(2951+2617)

library(mice)

# We will do only partial impute for now 
vars_for_impute = setdiff(names(pollTrain),c("USER_ID", "Party"))
vars_for_impute
imputed_train = complete(mice(pollTrain[vars_for_impute]))
pollTrain[vars_for_impute] = imputed_train
str(pollTrain)

vars_for_impute = setdiff(names(pollTest),c("USER_ID", "Party"))
vars_for_impute

imputed_test = complete(mice(pollTest[vars_for_impute]))
pollTest[vars_for_impute] = imputed_test
str(pollTest)

library(caTools)
pollTrainImp = read.csv("pollTrain_imputed.csv")

split = sample.split(pollTrainImp$Party, SplitRatio = 0.7)
split[1:10]

nrow(pollTrain)
Train = pollTrainImp[split,setdiff(names(pollTrainImp), "USER_ID")]
nrow(Train)
Test = pollTrainImp[!split,setdiff(names(pollTrainImp), "USER_ID")]
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

summary(PrdTrain>0.5)

t = table(Train$Party,PrdTrain>0.5)
t

sum(diag(t))/sum(t)

table(Train$Party)

2038/(2038+1828)

PrdTest = predict(LogModel1,newdata = Test, type = "response")

t = table(Test$Party,PrdTest>0.5)
t

sum(diag(t))/sum(t)

library(ROCR)


ROCRPred = prediction(PrdTest,Test$Party)
ROCRPerf = performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize = TRUE, print.cutoffs.at = seq(0,1,0.1))

prdFinTest = predict(LogModel1,newdata = pollTest, type = "response")

str(pollTest)

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

write.csv(pollTrain,"pollTrain_imputed.csv",row.names=FALSE)

write.csv(pollTest,"pollTest_imputed.csv",row.names=FALSE)


