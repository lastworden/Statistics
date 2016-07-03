
PollTrainImp = read.csv("pollTrain_imputed.csv")
PollTestImp = read.csv("pollTest_imputed.csv")

library(caTools)

PollTrainImp$Party = ifelse(PollTrainImp$Party=="Democrat",1,0)
table(PollTrainImp$Party)





split = sample.split(PollTrainImp$Party, SplitRatio = 0.75)

PollTrainImp$Party = as.factor(PollTrainImp$Party)

PollTrainImp$USER_ID = NULL
str(PollTrainImp)

Train = subset(PollTrainImp, split == TRUE)
Test = subset(PollTrainImp, split == FALSE)

#LogModel = glm(Party~., data = Train, family = binomial)

#PredTrain = predict(LogModel, type = "response")

Train_for_clust = Train

Train_for_clust$Party = NULL

distances = dist(Train_for_clust, method = "euclidean")

summary(distances)

var(distances)

clusterGrps = hclust(distances, method = "ward")

plot(clusterGrps)

clusterGroups = cutree(clusterGrps, k = 2)

str(clusterGroups)

table(clusterGroups)

cluster1 = subset(Train,clusterGroups==1)
cluster2 = subset(Train, clusterGroups==2)

table(cluster1$Party)

table(cluster2$Party)

library(flexclust)

Test_for_clust = Test
Test_for_clust$Party = NULL

Test_for_clust = lapply(Test_for_clust,as.numeric)
str(Test_for_clust)

Train_for_clust = lapply(Train_for_clust,as.numeric)
str(Train_for_clust)

set.seed(144)


#write.csv(Test_for_clust, "Test_clust.csv", row.names=FALSE)

class(Test_for_clust)

#write.csv(Train_for_clust, "Train_clust.csv", row.names=FALSE)

Train_for_clust = read.csv("Train_clust.csv")
Test_for_clust = read.csv("Test_clust.csv")

str(Train_for_clust)

km = kmeans(Train_for_clust, centers = 2)

table(km$cluster)

km.kcca = as.kcca(km,Train_for_clust)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata = Test_for_clust)

pollTrain1 = subset(Train, clusterTrain == 1)
pollTrain2 = subset(Train, clusterTrain == 2)

pollTest1 = subset(Test, clusterTest == 1)
pollTest2 = subset(Test, clusterTest == 2)

model1 = glm(Party~., data = pollTrain1, family = binomial)
model2 = glm(Party~., data = pollTrain2, family = binomial)

predTest1 = predict(model1, newdata = pollTest1, type = "response")

table(pollTest1$Party)

summary(predTest1)

t=table(pollTest1$Party,predTest1>0.5)
t
sum(diag(t))/sum(t)

predTest2 = predict(model2, newdata = pollTest2, type = "response")

table(pollTest2$Party, predTest2>0.5)

cor(as.numeric(pollTrain1$EducationLevel), as.numeric(pollTrain1$Party))

names(Train)

cor(Train_for_clust$EducationLevel, as.numeric(Train$Party))

cor(as.numeric(pollTrain1$Q96024), as.numeric(pollTrain1$Party))


