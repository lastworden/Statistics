
loans = read.csv("loans.csv")

str(loans)

table(loans$not.fully.paid)

1533/(8045+1533)

summary(loans)

str(loans)

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

loans = read.csv("loans_imputed.csv")

summary(loans)

str(loans)

set.seed(144)



library(caTools)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train = subset(loans, split == TRUE)

test = subset(loans, split == FALSE)

nrow(train)
nrow(test)

loansLog = glm(not.fully.paid~.,family = binomial,data = train)

summary(loansLog)

-9.317e-03 * -10

exp(0.09317)

predicted.risk = predict(loansLog, type = "response", newdata = test)

test$predicted.risk = predicted.risk

table(test$not.fully.paid,predicted.risk>0.5)

(2400+3)/(2400+3+457+13)

table(test$not.fully.paid)

2413/(2413+460)

library(ROCR)

ROCRPred = prediction(predicted.risk,test$not.fully.paid)

auc = as.numeric(performance(ROCRPred,"auc")@y.values)

auc

loansLog1 = glm(not.fully.paid~int.rate,family = binomial,data = train)

summary(loansLog1)

predRisk1 = predict(loansLog1, type = "response", newdata = test)

max(predRisk1)

sum(predRisk1>0.5)

ROCRPred1 = prediction(predRisk1,test$not.fully.paid)

auc = as.numeric(performance(ROCRPred1,"auc")@y.values)
auc

10*exp(0.06*3)

test$profit = exp(test$int.rate*3)-1

test$profit[test$not.fully.paid == 1] = -1

max(test$profit)

mean(test$profit)*100

highInterest = subset(test,int.rate>=0.15)

mean(highInterest$profit)

table(highInterest$not.fully.paid)

110/(327+110)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highInterest, predicted.risk<=cutoff)

nrow(selectedLoans)

sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)


