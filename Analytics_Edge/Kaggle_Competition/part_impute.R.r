
main_cols=c("USER_ID","YOB","Gender","Income","HouseholdStatus","EducationLevel","Party")

TrainImp = read.csv("pollTrain_imputed.csv")

table(TrainImp$YOB)

otrain1 = read.csv("train2016.csv")
nrow(otrain1)

trlab = subset(otrain1$Party,!otrain1$YOB %in% c("2011","2013","2039","1880","1881","1896"))
length(trlab)

TrainImp$Party = ifelse(TrainImp$Party=="Democrat",1,0)
table(TrainImp$Party)

t=c()
t
for (col in setdiff(names(TrainImp),c("USER_ID","Party"))){
  #if mean(is.na(pollTrain[,col])
    cat(col,"cor:",cor(as.numeric(TrainImp[,col]),as.numeric(TrainImp$Party)),"\n")
    val = cor(as.numeric(TrainImp[,col]),as.numeric(TrainImp$Party))
    t = c(t,c(col=val))
    
  #print(paste("The year is", year))
}
names(t) = setdiff(names(TrainImp),c("USER_ID","Party"))

t1=which(abs(t)>0.09)

names(t1)

TrainImp$Party = as.factor(TrainImp$Party)

library(caTools)
split = sample.split(TrainImp$Party, SplitRatio = 0.75)
Train = subset(TrainImp, split == TRUE)
Test = subset(TrainImp, split == FALSE)

library(rpart)
library(e1071)
library(randomForest)

rfModel1 = randomForest(Party~ Gender+Q115611+Q113181+Q109244+Q98197, data = Train)#, ntree = 1000, sampsize = 300)

predTrain = predict(rfModel1)

t = table(Train$Party,predTrain)
t

sum(diag(t))/sum(t)

predTest = predict(rfModel1,newdata = Test)

t = table(Test$Party,predTest)
t

sum(diag(t))/sum(t)

LogModel = glm(Party~YOB+Gender+HouseholdStatus+EducationLevel+Q98197+Q98078+
               Q98869+Q99480+Q100689+Q101163+Q105840+Q107869+Q109244, data = Train, family=binomial)

summary(LogModel)

predLogTrain = predict(LogModel,type = "response")

t = table(Train$Party,predLogTrain>0.5)
t
sum(diag(t))/sum(t)

predLogTest = predict(LogModel,newdata = Test,type="response")
t = table(Test$Party,predLogTest>0.5)
t
sum(diag(t))/sum(t)

Tree1 = rpart(Party~YOB+Gender+Income+HouseholdStatus+EducationLevel+Q122771+Q121699+
              Q120379+Q120472+Q119851+Q116881+Q115611+Q115899+Q113181+Q110740+Q109244+
              Q107869+Q106272+Q106042+Q105840+Q102089+Q101163+Q101596+
               Q100689+Q100680+Q99716+Q99480+Q98869+Q98078+Q98197, data = Train, method="class")

predtreeTest = predict(Tree1,newdata = Test, type = "class")
t = table(Test$Party,predtreeTest)
t
sum(diag(t))/sum(t)

library(e1071)
library(caret)
numFolds = trainControl(method = "cv",number = 10)
cpGrid = expand.grid(.cp=seq(0.001,0.01,0.001))
summary(cpGrid)

train(Party~ YOB+Gender+Income+HouseholdStatus+EducationLevel+Q122771+Q121699+Q120379+Q120472+Q119851+Q116881+Q115611+Q115899+Q113181+Q110740+Q109244+Q107869+Q106272+Q106042+Q105840+Q102089+Q101163+Q101596+
               Q100689+Q100680+Q99716+Q99480+Q98869+Q98078+Q98197,data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

TestFinal = read.csv("pollTest_imputed.csv")

PredTreeTestFin = predict(Tree1,newdata = TestFinal,type = "class")
PredTreeTestFin = as.factor(ifelse(PredTreeTestFin==1, "Democrat", "Republican"))
table(PredTreeTestFin)
MySubmission = data.frame(USER_ID = TestFinal$USER_ID, Predictions = PredTreeTestFin)
str(MySubmission)
write.csv(MySubmission, "SubmissionRPART_IMP_FE_01.csv", row.names=FALSE)

Train2 = Train[,c("YOB","Gender","Income","HouseholdStatus","EducationLevel")]
Test2 = Test[,c("YOB","Gender","Income","HouseholdStatus","EducationLevel")]

setdiff(names(Train2),names(Test2))

for (col in setdiff(names(Train2),c("Party"))){
  #if mean(is.na(pollTrain[,col])
    Train2[,col] = as.numeric(Train2[,col])
    Test2[,col] = as.numeric(Test2[,col])


    
  #print(paste("The year is", year))
}

str(Train2)


preproc = preProcess(Train2)

normTrain = predict(preproc, Train2)
normTest = predict(preproc, Test2)

str(normTest)

km = kmeans(normTrain, centers = 2)

table(km$cluster)

library(flexclust)

km.kcca = as.kcca(km,normTrain)

clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)

table(clusterTest)

length(clusterTest)

length(clusterTrain)

cTrain1 = subset(Train, clusterTrain == 1)
cTrain2 = subset(Train, clusterTrain == 2)


names(cTrain1)

cTest1 = subset(Test, clusterTest == 1)
cTest2 = subset(Test, clusterTest == 2)

cTree1 = rpart(Party~YOB+Gender+Income+HouseholdStatus+EducationLevel+Q122771+Q121699+Q120379+Q120472+Q119851+Q116881+Q115611+Q115899+Q113181+Q110740+Q109244+Q107869+Q106272+Q106042+Q105840+Q102089+Q101163+Q101596+
               Q100689+Q100680+Q99716+Q99480+Q98869+Q98078+Q98197, data=cTrain1, method = "class",cp=0.006)

t= table(cTrain1$Party,predict(cTree1,type="class"))
t
sum(diag(t))/sum(t)

t= table(cTest1$Party,predict(cTree1,newdata = cTest1,type="class"))
t
sum(diag(t))/sum(t)

cTree2 = rpart(Party~YOB+Gender+Income+HouseholdStatus+EducationLevel+Q122771+Q121699+Q120379+Q120472+Q119851+Q116881+Q115611+Q115899+Q113181+Q110740+Q109244+Q107869+Q106272+Q106042+Q105840+Q102089+Q101163+Q101596+
               Q100689+Q100680+Q99716+Q99480+Q98869+Q98078+Q98197, data=cTrain2, method = "class")

t= table(cTrain2$Party,predict(cTree2,type="class"))
t
sum(diag(t))/sum(t)

t= table(cTest2$Party,predict(cTree2,newdata = cTest2,type="class"))
t
sum(diag(t))/sum(t)

cLog1 = glm(Party~YOB+Gender+Income+HouseholdStatus+EducationLevel+Q122771+Q121699+Q120379+Q120472+Q119851+Q116881+Q115611+Q115899+Q113181+Q110740+Q109244+Q107869+Q106272+Q106042+Q105840+Q102089+Q101163+Q101596+
               Q100689+Q100680+Q99716+Q99480+Q98869+Q98078+Q98197, data=cTrain1, family=binomial)

t= table(cTrain1$Party,predict(cLog1,type="response")>0.5)
t
sum(diag(t))/sum(t)

t= table(cTest1$Party,predict(cLog1,newdata = cTest1,type="response")>0.5)
t
sum(diag(t))/sum(t)

cLog2 = glm(Party~YOB+Gender+Income+HouseholdStatus+EducationLevel+Q122771+Q121699+Q120379+Q120472+Q119851+Q116881+Q115611+Q115899+Q113181+Q110740+Q109244+Q107869+Q106272+Q106042+Q105840+Q102089+Q101163+Q101596+
               Q100689+Q100680+Q99716+Q99480+Q98869+Q98078+Q98197, data=cTrain2, family=binomial)

t= table(cTrain2$Party,predict(cLog2,type="response")>0.5)
t
sum(diag(t))/sum(t)

t= table(cTest2$Party,predict(cLog2,newdata = cTest2,type="response")>0.6)
t
sum(diag(t))/sum(t)

library(class)

knn(cTrain1,cTest1,cTrain1$Party)

Train3 = Train
Test3 = Test

tr_lab= Train3$Party
ts_lab = Test3$Party

Train3$USER_ID = NULL
Test3$USER_ID = NULL
Train3$Party =NULL
Test3$Party = NULL

library(caret)
library(class)

for (col in setdiff(names(Train3),c("Party"))){
  #if mean(is.na(pollTrain[,col])
    Train3[,col] = as.numeric(Train3[,col])
    Test3[,col] = as.numeric(Test3[,col])


    
  #print(paste("The year is", year))
}

preproc1 = preProcess(Train3)
Train4 = predict(preproc1,Train3)
Test4= predict(preproc1,Test3)

str(Train4)

PredKnn=knn(Train4,Test4,tr_lab,k=40)

t=table(ts_lab,PredKnn)
t
sum(diag(t))/sum(t)

?knn


