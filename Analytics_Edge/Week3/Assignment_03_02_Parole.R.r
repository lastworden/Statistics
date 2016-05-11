
parole = read.csv("parole.csv")

str(parole)

summary(parole)

table(parole$state)

table(parole$violator)

table(parole$crime)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

str(parole)

summary(parole)

 set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

nrow(train)
nrow(test)

ParoleLog1 = glm(violator~., family = "binomial", data = train)

summary(ParoleLog1)

exp(1.61)

logit = (-4.2411574
+0.3869904*1
+0.8867192*1
-0.0001756*50
-0.1238867*3
+0.0802954*12
+0.6837143*1)

logit

exp(logit)

1/(1+exp(-1*logit))

predTest = predict(ParoleLog1,type = "response", newdata = test)

max(predTest)

table(test$violator,predTest>0.5)

#sesitivity 
12/(11+12)

#specificity
167/(167+12)

#accuracy
(167+12)/(167+12+12+11)

table(test$violator)

179/(179+23)

library(ROCR)

ROCRPred = prediction(predTest,test$violator)

as.numeric(performance(ROCRPred,"auc")@y.values)

ROCRPerf = performance(ROCRPred,"tpr","fpr")

plot(ROCRPerf,colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))


