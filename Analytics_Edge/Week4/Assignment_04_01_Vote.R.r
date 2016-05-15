

gerber = read.csv("gerber.csv")

str(gerber)

table(gerber$control)

mean(gerber$voting)


table(gerber$voting, gerber$neighbors)


12021/(12021+26197)

12316/(12316+25888)

13191/(13191+25027)

14438/(14438+23763)

tapply(gerber$voting, gerber$civicduty, mean)

gerberLog = glm(voting~hawthorne+civicduty+self+neighbors, data = gerber, family = binomial)

summary(gerberLog)

predGerber1 = predict(gerberLog, type = "response")

t = table(gerber$voting,predGerber1>0.5)
t

235388/sum(t)

0.541957777751944

library(ROCR)

ROCRPred = prediction(predGerber1,gerber$voting)

auc = as.numeric(performance(ROCRPred,"auc")@y.values)

auc

1-mean(gerber$voting)

library("rpart")

library("rpart.plot")

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)

prp(CARTmodel3)

CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)

CARTgender = rpart(voting ~ control+sex, data=gerber, cp=0.0)

prp(CARTcontrol,digits = 6)

abs(0.296638-0.34)

prp(CARTgender, digits = 6)

abs(0.34-0.345818)

abs(0.34-0.334176)

gerberLog2 = glm(voting~control+sex, data = gerber, family = binomial)

summary(gerberLog2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerberLog2, newdata=Possibilities, type="response")

Possibilities

abs(0.290806452512016-0.290456)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")

abs(0.290455779247779-0.290456)


