
pollTrain = read.csv("train2016.csv",na.strings=c("","NA"))
pollTest = read.csv("test2016.csv",na.strings=c("","NA"))

trn_id = pollTrain$USER_ID
tst_id = data.frame(USER_ID=pollTest$USER_ID)
trn_lab = data.frame(USER_ID=pollTrain$USER_ID,Party=pollTrain$Party)


pollTrain$Party = NULL

poll = rbind(pollTrain,pollTest)

str(poll)

poll$Income = as.character(poll$Income)

poll$Income = ifelse(poll$Income=="under $25,000","12",poll$Income)

table(poll$Income)

poll_bu = poll

poll = poll_bu

poll$Income = as.numeric(poll$Income)

table(poll$YOB)

orig = read.csv("train2016.csv")
table(orig$YOB)

summary(2015-poll$YOB)

str(poll)

poll=subset(poll,!poll$YOB %in% c(2011,2013,2039,1880,1881,1896))

nrow(poll)

poll$YOB = ifelse(is.na(poll$YOB),poll$YOB,2015-poll$YOB)

str(poll)

library(mice)

vars_for_impute = setdiff(names(poll),c("USER_ID"))
vars_for_impute
imputed_poll = complete(mice(poll[vars_for_impute]))
poll[vars_for_impute] = imputed_poll
str(poll)

#write.csv(poll, "poll_full_IMP_FE.csv", row.names=FALSE)

poll_imp = read.csv("poll_full_IMP_FE.csv")

str(poll_imp)

poll_train_imp = merge(poll_imp,trn_lab,by="USER_ID")
nrow(poll_train_imp)

table(poll_train_imp$Party)

poll_test_imp = merge(poll_imp,tst_id,by="USER_ID")
nrow(poll_test_imp)

#write.csv(poll_train_imp, "poll_train_IMP_FE.csv", row.names=FALSE)
#write.csv(poll_test_imp, "poll_test_IMP_FE.csv", row.names=FALSE)

str(poll_train_imp)

library(caTools)

split = sample.split(poll_train_imp$Party,SplitRatio = 0.7)
Train = subset(poll_train_imp, split == TRUE)
Test = subset(poll_train_imp,split==FALSE)

nrow(Train)

LogModel = glm(Party~YOB+Gender, data = Train, family = binomial)

t = table(Train$Party,predict(LogModel,type = "response")>0.5)
t

sum(diag(t))/sum(t)

t = table(Test$Party,predict(LogModel,newdata = Test, type = "response")>0.5)
t
sum(diag(t))/sum(t)

library(randomForest)

rf1 = randomForest(Party~.-USER_ID, data = Train, ntree = 300, nodesize = 30)

t = table(Train$Party,predict(rf1))
t
sum(diag(t))/sum(t)

t = table(Test$Party,predict(rf1,newdata = Test))
t
sum(diag(t))/sum(t)

library(rpart)

Tree1 = rpart(Party~.-USER_ID, data = Train, method = "class",cp=0.008)

t = table(Train$Party,predict(Tree1,type = "class"))
t
sum(diag(t))/sum(t)

t = table(Test$Party,predict(Tree1,newdata = Test, type = "class"))
t
sum(diag(t))/sum(t)

Tree2 = rpart(Party~.-USER_ID, data = poll_train_imp, method = "class")

t = table(poll_train_imp$Party,predict(Tree2,type = "class"))
t
sum(diag(t))/sum(t)

predFinTest = predict(Tree1, newdata = poll_test_imp,type="class")

table(predFinTest)

result = data.frame(USER_ID=poll_test_imp$USER_ID,Predictions=predFinTest)
str(result)

#write.csv(result, "Submission_FE_01.csv", row.names=FALSE)

library(caret)

table(poll_train_imp$Party)
train_poll = as.data.frame(model.matrix(Party~.,poll_train_imp))
train_poll$Party = poll_train_imp$Party

table(train_poll$Party)
names(train_poll$(Intercept) = NULL
str(train_poll)

str(train_poll)


