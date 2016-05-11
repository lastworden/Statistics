
framingham = read.csv("framingham.csv")

summary(framingham)

str(framingham)


library(caTools)

set.seed(1000)

split = sample.split(framingham$TenYearCHD,SplitRatio = 0.65)

train = subset(framingham, split == TRUE)

str(train)

test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD~., data = train, family = binomial)

summary(framinghamLog)

predictTest = predict(framinghamLog, type = "response" , newdata = test)

summary(predictTest)

table(test$TenYearCHD, predictTest>0.5)

#accuracy
(1069+11)/(1069+11+6+187)

#baseline
1-mean(test$TenYearCHD,na.rm = TRUE)

(1069+6)/(1069+11+6+187)

library(ROCR)

ROCRPred = prediction(predictTest, test$TenYearCHD)

ROCRPerf = performance(ROCRPred,"tpr","fpr")

plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

auc = as.numeric(performance(ROCRPred, "auc")@y.values)



auc

11/(11+187)

1069/(1069+6)

table(test$TenYearCHD,predictTest>0.2)

sensitivity = 110/(110+88)

sensitivity

specificity = 865/(865+210)

specificity

#accuracy
(865+110)/(865+210+88+110)


