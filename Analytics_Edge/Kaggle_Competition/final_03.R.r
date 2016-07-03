
pollTrain = read.csv("normPollElabTrain.csv")
pollTest = read.csv("normPollElabTest.csv")

names(pollTrain)

names(pollTest)

tr_label = pollTrain$Party
tst_ids = pollTest$USER_ID

Train = pollTrain
Train$Party = NULL
Test = pollTest
Test$USER_ID = NULL


library(rpart)
library(e1071)
library(randomForest)
library(caret)

preproc = preProcess(Train)

normTrain = predict(preproc,Train)
normTest = predict(preproc, Test)

km = kmeans(normTrain, centers = 3)

library(flexclust)

km.kcca = as.kcca(km,normTrain)

clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)

table(clusterTest)

table(clusterTrain)

cTrain1 = subset(Train, clusterTrain == 1)
cTrain2 = subset(Train, clusterTrain == 2)
cTrain3 = subset(Train, clusterTrain == 3)


cTest1 = subset(Test, clusterTest == 1)
cTest2 = subset(Test, clusterTest == 2)
cTest3 = subset(Test, clusterTest == 3)

cTest1$USER_ID = subset(tst_ids, clusterTest == 1)
cTest2$USER_ID = subset(tst_ids, clusterTest == 2)
cTest3$USER_ID = subset(tst_ids, clusterTest == 3)

cTrain1$Party = subset(tr_label, clusterTrain == 1)
cTrain2$Party = subset(tr_label, clusterTrain == 2)
cTrain3$Party = subset(tr_label, clusterTrain == 3)

table(cTrain3$Party)

Tree1 = rpart(Party~exp(Q109244Yes) + exp(Q115611Yes) + exp(Q98197Yes) + 
    exp(Q98869Yes), data = cTrain1, method = "class")
Tree2 = rpart(Party~exp(Q109244Yes) + exp(Q115611Yes) + exp(Q98197Yes) + 
    exp(Q98869Yes), data = cTrain2, method = "class")
Tree3 = rpart(Party~exp(Q109244Yes) + exp(Q115611Yes) + exp(Q98197Yes) + 
    exp(Q98869Yes), data = cTrain3, method = "class")

predTree1= predict(Tree1,type="class")
predTree2= predict(Tree2,type="class")
predTree3= predict(Tree3,type="class")

t1 = table(cTrain1$Party,predTree1)
t1
sum(diag(t1))/sum(t1)

t2 = table(cTrain2$Party,predTree2)
t2
sum(diag(t2))/sum(t2)

t3 = table(cTrain3$Party,predTree3)
t3
sum(diag(t3))/sum(t3)

predTree1= predict(Tree1,newdata = cTest1,type="class")
predTree2= predict(Tree2,newdata = cTest2,type="class")
predTree3= predict(Tree3,newdata = cTest3,type="class")

result = data.frame(USER_ID = c(cTest1$USER_ID,cTest2$USER_ID,cTest3$USER_ID), Predictions=c(predTree1,predTree2,predTree3))

result$Predictions = ifelse(result$Predictions==1, "Democrat","Republican")
str(result)


table(result$Predictions)

write.csv(result, "Submission_FE_03.csv", row.names=FALSE)

datpca=prcomp(normTrain, scale. = T)

dim(datpca$x)

summary(datpca)

str(as.data.frame(datpca$x))

pcaDat = as.data.frame(datpca$x)

str(pcaDat)

pcaDat = pcaDat [,1:50]

str(pcaDat)

pcaDat$Party = pollTrain$Party

str(pcaDat$Party)

Log1 = randomForest(Party~., data = pcaDat)

t = table(pcaDat$Party,predict(Log1))
t
sum(diag(t))/sum(t)


