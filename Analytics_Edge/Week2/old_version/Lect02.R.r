
getwd()

wine = read.csv("wine.csv")

str(wine)

summary(wine)

##Create a linear regression model. Price Vs AGST alone

model1 = lm(Price~AGST,data=wine)

ls()

rm(model)

ls()

summary(model1)

model1$residuals


SSE = sum(model1$residuals^2)

SSE

model2 = lm(Price ~ AGST + HarvestRain,data = wine)

summary(model2)

SSE = sum(model2$residuals^2)

SSE

names(wine)

model3 = lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop, data = wine)

summary(model3)

SSE = sum(model3$residuals^2)
SSE

model4 = lm(Price~AGST+HarvestRain+WinterRain+Age,data=wine)

summary(model4)

SSE = sum(model4$residuals^2)
SSE

model5 = lm(Price~AGST+HarvestRain+WinterRain+FrancePop,data = wine)

summary(model5)

model6 = lm(Price~HarvestRain+WinterRain,data = wine)
summary(model6)

sum(model6$residuals^2)

#correlation between winter rain and price
cor(wine$WinterRain,wine$Price)

cor(wine$Age,wine$FrancePop)

plot(wine$Age,wine$FrancePop)

str(wine)
nrow(wine)
ncol(wine)

class(wine)

t=cor(wine)
t

t[,"Price"]

model7 = lm(Price~AGST+HarvestRain+WinterRain,data = wine)
summary(model7)

t>0.7 | t< -0.7

t

cor(wine$HarvestRain,wine$WinterRain)

### Evaluate performance on test data

wine_test = read.csv("wine_test.csv")

str(wine_test)

TestPredict = predict(model4, newdata = wine_test)

TestPredict


wine_test$Price

MSE = sum((TestPredict-wine_test$Price)^2)/nrow(wine_test)

MSE

SSE = sum((TestPredict-wine_test$Price)^2)

SSE

mean(wine_test$Price)

SST = sum((wine_test$Price-mean(wine$Price))^2)

SST

R_sq = 1-(SSE/SST)

R_sq



