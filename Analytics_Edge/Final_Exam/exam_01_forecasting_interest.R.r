
fedFunds = read.csv("federalFundsRate.csv", stringsAsFactors = FALSE)

str(fedFunds)

table(fedFunds$RaisedFedFunds)

294/(294+291)

table(fedFunds$Chairman, fedFunds$RaisedFedFunds)

table(fedFunds$Chairman)

fedFunds$Chairman= as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

set.seed(201)
library(caTools)


spl = sample.split(fedFunds$RaisedFedFunds, SplitRatio = 0.70)

training = subset(fedFunds, spl == TRUE)
testing = subset(fedFunds, spl == FALSE)

LogRegModel1 = glm(RaisedFedFunds~PreviousRate+Streak+Unemployment
                   +HomeownershipRate+DemocraticPres+MonthsUntilElection, data=training, family = binomial)

summary(LogRegModel1)

cor(fedFunds$Streak,as.numeric(fedFunds$RaisedFedFunds))

v=9.121012+(-0.003427*1.7)+(0.157658*-3)+(-0.047449*5.1)+(-0.136451*65.3)+(0.347829*0)+(-0.006931*18)

fedFunds$DemocraticPres[1:10]

nrow(testing)

t = data.frame(Streak=-3,PreviousRate=1.7,Unemployment=5.1,HomeownershipRate=65.3,DemocraticPres=factor(0),MonthsUntilElection=18)

t

tres = predict(LogRegModel1,newdata = t,type = "response")

tres

p1=1/(1+exp(-v))
p1

v2= 9.121012+(-0.003427*1.7)+(0.157658*-3)+(-0.047449*5.1)+(-0.136451*65.3)+(0.347829*1)+(-0.006931*18)

p2=1/(1+exp(-v2))
p2

p2/p1

t=table(fedFunds$RaisedFedFunds)

t

294/(294+291)

predRes = predict(LogRegModel1, newdata = testing, type = "response")

t = table(testing$RaisedFedFunds,predRes>0.5)

t

sum(predRes<0.5)

predResBin = as.numeric(predRes>=0.5)
sum(predResBin)

length(predResBin)

library(ROCR)

ROCRpredTest = prediction(predRes, testing$RaisedFedFunds)

auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)

auc

ROCRPerf = performance(ROCRpredTest,"tpr","fpr")


plot(ROCRPerf, colorize = TRUE,print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

set.seed(201)
library(caret)

numFolds = trainControl(method = "cv",number = 10)

cpGrid = expand.grid(.cp=seq(0.001,0.05,0.001))

dim(cpGrid)

train(RaisedFedFunds~PreviousRate+Streak+Unemployment
                   +HomeownershipRate+DemocraticPres+MonthsUntilElection,data = training, method = "rpart", trControl = numFolds, 
      tuneGrid = cpGrid)

Tree1 = rpart(RaisedFedFunds~PreviousRate+Streak+Unemployment
                   +HomeownershipRate+DemocraticPres+MonthsUntilElection,data = training,method = "class", cp=0.016)

library(rpart.plot)
prp(Tree1)

predCart = predict(Tree1, newdata = testing, type = "class")

t = table(testing$RaisedFedFunds,predCart)
t

sum(diag(t))/sum(t)


