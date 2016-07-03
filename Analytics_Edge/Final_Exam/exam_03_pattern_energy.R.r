
energy = read.csv("energy.csv")

str(energy)

which.max(energy$GenTotalRenewable)

energy[169,]

tapply(energy$AllSourcesCO2, energy$presidential.results==0, mean, na.rm=TRUE)

summary(energy$AllSourcesCO2
       )

tapply(energy$AllSourcesNOx, energy$presidential.results==0, mean, na.rm=TRUE)

summary(energy$YEAR)

cor(energy$AllSourcesCO2,energy$EsalesIndustrial, use = "complete")

cor(energy$AllSourcesSO2,energy$EsalesIndustrial, use = "complete")

cor(energy$AllSourcesNOx,energy$EsalesResidential, use = "complete")

cor(energy$AllSourcesCO2,energy$EsalesCommercial, use = "complete")

boxplot(EPriceTotal~STATE,data = energy,varwidth=TRUE)

sort(tapply(energy$EPriceTotal,energy$STATE,mean))

sort(tapply(energy$GenTotal,energy$STATE,mean))

sort(tapply(energy$EPriceTotal,energy$STATE,max))

set.seed(144)

spl = sample(1:nrow(energy), size = 0.7*nrow(energy))

train = energy[spl,]

test = energy[-spl,]

LRM1 = glm(GenSolarBinary~GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, data = train, family = binomial)

summary(LRM1)

predReg1 = predict(LRM1, newdata = test, type = "response")

t = table(test$GenSolarBinary,predReg1>0.5)
t

sum(diag(t))/sum(t)

indDem = (test$presidential.results == 1)

summary(indDem)

PredBin = predReg1>0.5

demOrig = test$GenSolarBinary[indDem]
demPred = PredBin[indDem]

t = table(demOrig,demPred)
t

sum(diag(t))/sum(t)

repOrig = test$GenSolarBinary[!indDem]
repPred = PredBin[!indDem]

t = table(repOrig,repPred)
t

sum(diag(t))/sum(t)

table(test$GenSolarBinary,predReg1>0.5,test$presidential.results)

train.limited = train[,c("CumlRegulatory","CumlFinancial","presidential.results","Total.salary","Import")]
test.limited = test[,c("CumlRegulatory","CumlFinancial","presidential.results","Total.salary","Import")]

library(caret)

preproc = preProcess(train.limited)

normTrain = predict(preproc,train.limited)
normTest = predict(preproc, test.limited)

set.seed(144)
KMC = kmeans(normTrain, centers = 2, iter.max = 1000)

library(flexclust)

KMC.kcca = as.kcca(KMC, normTrain)

trainClusters = predict(KMC.kcca, newdata = normTrain)

table(trainClusters)

table(KMC$cluster)

train1 = train[trainClusters==1,]
train2 = train[trainClusters==2,]

sum(train1$presidential.results==0)

sum(train2$presidential.results==0)

sum(train1$CumlFinancial)
sum(train1$CumlRegulatory)

sum(train2$CumlFinancial)
sum(train2$CumlRegulatory)

sum(train2$AllSourcesCO2, na.rm = TRUE)
sum(train2$AllSourcesSO2, na.rm = TRUE)
sum(train2$AllSourcesNOx, na.rm = TRUE)

sum(train1$AllSourcesCO2, na.rm = TRUE)
sum(train1$AllSourcesSO2, na.rm = TRUE)
sum(train1$AllSourcesNOx, na.rm = TRUE)

mod1 = glm(GenSolarBinary~GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, data =  train1, family = binomial)

summary(mod1)

testClusters = predict(KMC.kcca, newdata = normTest)


test1 = test[testClusters==1,]
test2 = test[testClusters==2,]

predTest1 = predict(mod1, newdata = test1, type = "response")

t = table(test1$GenSolarBinary,predTest1>0.5)
t
sum(diag(t))/sum(t)

predTestOrig = predict(LRM1, newdata = test1, type = "response")

t = table(test1$GenSolarBinary,predTestOrig>0.5)
t
sum(diag(t))/sum(t)

mod2 = glm(GenSolarBinary~GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, data =  train2, family = binomial)

summary(mod2)

predTest2 = predict(mod2, newdata = test2, type = "response")

t = table(test2$GenSolarBinary,predTest2>0.5)
t
sum(diag(t))/sum(t)

predTestOrig2 = predict(LRM1, newdata = test2, type = "response")

t = table(test2$GenSolarBinary,predTestOrig2>0.5)
t
sum(diag(t))/sum(t)

n1=nrow(test1)
n1

n2=nrow(test2)
n2

(n1*0.908396946564885+n2*0.746835443037975)/(n1+n2)


