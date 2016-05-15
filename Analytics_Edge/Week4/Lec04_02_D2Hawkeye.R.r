
Claims = read.csv("ClaimsData.csv")

str(Claims)

table(Claims$bucket2009)/nrow(Claims)

library(caTools)

set.seed(88)

split = sample.split(Claims$bucket2009, SplitRatio = 0.6 )


claimsTrain = subset(Claims, split == TRUE)
str(claimsTrain)

claimsTest = subset(Claims, split == FALSE)

mean(claimsTrain$age)

mean(claimsTrain$diabetes)

t=table(claimsTest$bucket2009, claimsTest$bucket2008)
t

(110138+10721+2774+1539+104)/(sum(t))

diag(t)

sum(diag(t))/sum(t)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)

PenaltyMatrix

names(PenaltyMatrix) = list(c('act_buck1','act_buck2','act_buck3','act_buck4','act_buck5'), c('pred_buck1','pred_buck2','pred_buck3','pred_buck4','pred_buck5'))

PenaltyMatrix

rownames(PenaltyMatrix) = c('act_buck1','act_buck2','act_buck3','act_buck4','act_buck5')

PenaltyMatrix

colnames(PenaltyMatrix)=c('pred_buck1','pred_buck2','pred_buck3','pred_buck4','pred_buck5')

PenaltyMatrix

t*PenaltyMatrix

sum(t*PenaltyMatrix)/nrow(claimsTest)

sort(table(claimsTrain$bucket2009))

one(5)

baseLine1 = rep(1,nrow(claimsTest))

t2 = table(claimsTest$bucket2009,baseLine1)
t2

122978/(sum(t2))

sum(t2*PenaltyMatrix[,1])/nrow(claimsTest)

library(rpart)

library(rpart.plot)

names(claimsTest)

ClaimsTree = rpart(bucket2009~age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=claimsTrain, method = "class", cp =0.00005)

prp(ClaimsTree)

PredictTest = predict(ClaimsTree, newdata = claimsTest, type = "class")

tPred1 = table(claimsTest$bucket2009, PredictTest)
tPred1

sum(diag(tPred1))/sum(tPred1)

sum(tPred1*PenaltyMatrix)/nrow(claimsTest)

ClaimsTree = rpart(bucket2009~age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=claimsTrain,
                   method = "class", cp =0.00005, parms = list(loss = PenaltyMatrix))

PredictTest2 = predict(ClaimsTree,newdata = claimsTest,type = "class")

tPred2 = table(claimsTest$bucket2009,PredictTest2)
tPred2

sum(diag(tPred2))/sum(tPred2)

sum(tPred2*PenaltyMatrix)/nrow(claimsTest)

sum(tPred1[,1])/nrow(claimsTest)

sum(tPred2[,1])/nrow(claimsTest)


