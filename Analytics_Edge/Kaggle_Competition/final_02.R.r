
pollTrain = read.csv("poll_train_IMP_FE.csv")
pollTest = read.csv("poll_test_IMP_FE.csv")

str(pollTrain)

library(caret)
library(mlbench)
control <- rfeControl(functions=rfFuncs, method="cv", number=20)

esults <- rfe(pollTrain[,setdiff(names(pollTrain),c("USER_ID","Party"))], pollTrain$Party, sizes=c(1:15), rfeControl=control)

predictors(esults)

plot(esults)

library(rpart)

library(caTools)

Tree1 = rpart(Party~Q109244+Q115611+Q98197+Q113181+Q98869, data = pollTrain, method = "class",cp=0.001)

t = table(pollTrain$Party,predict(Tree1,type = "class"))
t
sum(diag(t))/sum(t)

predTest = predict(Tree1, newdata = pollTest, type = "class")

table(predTest)

result = data.frame(USER_ID=pollTest$USER_ID,Predictions=predTest)

#write.csv(result, "Submission_FE_02.csv", row.names=FALSE)

library(e1071)

svmModel = svm(Party~Q109244+Q115611+Q98197+Q113181+Q98869, data = pollTrain, gamma=0.01,nu=0.4)#, gamma = 0.1, c=1000)

summary(svmModel)

t = table(pollTrain$Party,predict(svmModel))
t
sum(diag(t))/sum(t)

table(predict(svmModel,newdata = pollTest))


