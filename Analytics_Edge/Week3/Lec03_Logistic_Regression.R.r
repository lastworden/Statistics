
wt = -1.5+3*1-0.5*5
wt

exp(wt)

1/(1+exp(-1*wt))

getwd()

quality = read.csv("quality.csv")

str(quality)

table(quality$PoorCare)

98/(98+33)

install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

split

summary(split)

str(quality)

which(split==FALSE)

qualityTrain = subset(quality,split == TRUE)

str(qualityTrain)

qualityTest = subset(quality,split == FALSE)

str(qualityTest)

QualityLog = glm(PoorCare~OfficeVisits+Narcotics, data = qualityTrain, family = binomial)

summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")

summary(predictTrain)

tapply(predictTrain,qualityTrain$PoorCare, mean)

QualityLog2 = glm(PoorCare~StartedOnCombination+ProviderCount, data = qualityTrain, family = binomial)

summary(QualityLog2)

table(qualityTrain$PoorCare,predictTrain>0.5)

#sensitivity
10/(10+15)

#selectivity
70/(70+4)

table(qualityTrain$PoorCare,predictTrain>0.7)

8/25

73/74

table(qualityTrain$PoorCare,predictTrain>0.2)

16/25

54/74

15/25

install.packages("ROCR")

library(ROCR)

ROCRPred = prediction(predictTrain,qualityTrain$PoorCare)

ROCRPerf = performance(ROCRPred,"tpr","fpr")

plot(ROCRPerf, colorize = TRUE)

plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

predictTest = predict(QualityLog, type="response", newdata=qualityTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


auc


