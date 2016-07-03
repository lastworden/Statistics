
pollTrain = read.csv("poll_train_IMP_FE.csv")
pollTest = read.csv("poll_test_IMP_FE.csv")

str(pollTrain)

table(pollTrain$Party)
pollTrainElab = as.data.frame(model.matrix(Party~.,pollTrain))
pollTrainElab$Party= pollTrain$Party
#pollTrainElab$USER_ID = NULL
pollTrainElab[,"(Intercept)"] = NULL
table(pollTrainElab$Party)

str(pollTrainElab)

names(pollTest)

nrow(pollTest)

test_uids = pollTest$USER_ID
pollTestElab = as.data.frame(model.matrix(USER_ID~.,pollTest))
pollTestElab$USER_ID = test_uids
pollTestElab[,"(Intercept)"] = NULL

str(pollTestElab)

length(pollTestElab$USER_ID)

#write.csv(pollTestElab, "pollTestImpElab.csv", row.names=FALSE)
#write.csv(pollTrainElab, "pollTrainImpElab.csv", row.names=FALSE)

library(caTools)
tr_labl = pollTrainElab$Party

library(caret)
preproc = preProcess(pollTrainElab)

normPollElabTrain = predict(preproc,pollTrainElab)
normPollElabTrain$USER_ID = NULL
normPollElabTest = predict(preproc, pollTestElab)
normPollElabTest$USER_ID = pollTestElab$USER_ID

str(normPollElabTest)

split = sample.split(normPollElabTrain$Party,SplitRatio = 0.7)

Train = subset(normPollElabTrain,split == TRUE)
Test = subset(normPollElabTrain, split == FALSE)

names(Train)

#write.csv(normPollLbTrain, "normPollElabTrain.csv", row.names=FALSE)

Log1 = glm(Party~exp(YOB)+exp(GenderMale)+exp(Income)+exp(Q124742Yes)+exp(Q124122Yes)+exp(Q123464Yes)+exp(Q123621Yes)+exp(Q122769Yes)+exp(Q122770Yes)+exp(Q122771Public)+exp(Q122120Yes)+exp(Q121699Yes)+exp(Q121700Yes)+exp(Q120978Yes)+exp(Q121011Yes)+exp(Q120379Yes)+exp(Q120650Yes)+exp(Q120472Science)+exp(Q120012Yes)+exp(Q120014Yes)+exp(Q119334Yes)+exp(Q119851Yes)+exp(Q119650Receiving)+exp(Q118892Yes)+exp(Q118117Yes)+exp(Q118232Pragmatist)+exp(Q118233Yes)+exp(Q118237Yes)+exp(Q116797Yes)+exp(Q116881Right)+exp(Q116953Yes)+exp(Q116601Yes)+exp(Q116441Yes)+exp(Q116448Yes)+exp(Q116197P.M.)+exp(Q115602Yes)+exp(Q115777Start)+exp(Q115610Yes)+exp(Q115611Yes)+exp(Q115899Me)+exp(Q115390Yes)+exp(Q114961Yes)+exp(Q114748Yes)+exp(Q115195Yes)+exp(Q114517Yes)+exp(Q114386TMI)+exp(Q113992Yes)+exp(Q114152Yes)+exp(Q113583Tunes)+exp(Q113584Technology)+exp(Q113181Yes)+exp(Q112478Yes)+exp(Q112512Yes)+exp(Q112270Yes)+exp(Q111848Yes)+exp(Q111580Supportive)+exp(Q111220Yes)+exp(Q110740PC)+exp(Q109367Yes)+exp(Q109244Yes)+exp(Q108617Yes)+exp(Q108856Space)+exp(Q108754Yes)+exp(Q108342Online)+exp(Q108343Yes)+exp(Q107869Yes)+exp(Q107491Yes)+exp(Q106993Yes)+exp(Q106272Yes)+exp(Q106388Yes)+exp(Q106389Yes)+exp(Q106042Yes)+exp(Q105840Yes)+exp(Q105655Yes)+exp(Q104996Yes)+exp(Q103293Yes)+exp(Q102906Yes)+exp(Q102674Yes)+exp(Q102687Yes)+exp(Q102289Yes)+exp(Q102089Rent)+exp(Q101162Pessimist)+exp(Q101163Mom)+exp(Q101596Yes)+exp(Q100689Yes)+exp(Q100680Yes)+exp(Q100562Yes)+exp(Q99982Nope)+exp(Q100010Yes)+exp(Q99716Yes)+exp(Q99581Yes)+exp(Q99480Yes)+exp(Q98869Yes)+exp(Q98578Yes)+exp(Q98059Yes)+exp(Q98078Yes)+exp(Q98197Yes)+exp(Q96024Yes)
           , data = Train, family = binomial)

t=table(Train$Party,predict(Log1,type="response")>0.5)
t
sum(diag(t))/sum(t)

t=table(Test$Party,predict(Log1,newdata = Test,type="response")>0.5)
t
sum(diag(t))/sum(t)

names(normPollElabTrain)

t=normPollElabTrain
t$Party=NULL


library(mlbench)

.Library

correlationMatrix <- cor(t)

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

highlyCorrelated

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

?rfe

results <- rfe(t, normPollElabTrain$Party, sizes=c(1:15), rfeControl=control)


predictors(results)

plot(results, type=c("g", "o"))

library(rpart)

Log1 = glm(Party~exp(Q109244Yes)+exp(Q115611Yes)+exp(Q98197Yes)+exp(Q98869Yes), data = Train, family = binomial)

summary(Log1)

t=table(Train$Party,predict(Log1,type="response")>0.5)
t
sum(diag(t))/sum(t)

t=table(Test$Party,predict(Log1,newdata = Test,type="response")>0.5)
t
sum(diag(t))/sum(t)

library(e1071)

svmModel = svm(Party~exp(Q109244Yes) + exp(Q115611Yes) + exp(Q98197Yes) + 
    exp(Q98869Yes), kernel = "linear", data = Train, c=10000)#, gamma = 0.1, c=1000)

summary(svmModel)

t=table(Train$Party,predict(svmModel))
t
sum(diag(t))/sum(t)

t=table(Test$Party,predict(svmModel,newdata = Test))
t
sum(diag(t))/sum(t)


