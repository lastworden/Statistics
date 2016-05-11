
2+3

1:10

Countries = c("India","China","USA","Australia")
Countries

Population = c(2000,3000,200,50)

CountryData1 = data.frame(Countries,Population)
CountryData1

str(CountryData1)

Countries = c("England","Cuba")
Population = c(100,15)

CountryData2 = data.frame(Countries,Population)

CountryData2

CountryTot = rbind(CountryData1,CountryData2)
CountryTot

CountryTot$NGI = c(12,45,12,32,15,20)

CountryTot

summary(CountryTot)

str(CountryTot)

getwd()

WHO = read.csv("WHO.csv")

str(WHO)

summary(WHO)

WHO_Europe = subset(WHO,Region == "Europe")

WHO_Europe

str(WHO_Europe)

#Max population
which.max(WHO$Population)

#Country with max population 
WHO$Country[which.max(WHO$Population)]

summary(WHO$Population)

sd(WHO$Population)

min(WHO$Population)

mean(WHO$Population)

median(WHO$Population)

#Country with min population 
WHO$Country[which.min(WHO$Population)]

plot(WHO$GNI,WHO$FertilityRate)

Outliers = subset(WHO, GNI>10000 & FertilityRate >2.5)
Outliers

Outliers[c("Country","GNI","FertilityRate")]

nrow(Outliers)

hist(WHO$CellularSubscribers)

boxplot(WHO$LifeExpectancy~WHO$Region)

boxplot(WHO$LifeExpectancy~WHO$Region,ylab = "Life Expectancy", main = "Life Expectancy for Different Regions")

table(WHO$Region)

tapply(WHO$Over60,WHO$Region, mean)

tapply(WHO$LiteracyRate,WHO$Region, min)

tapply(WHO$LiteracyRate,WHO$Region, min,na.rm=TRUE)

tapply(WHO$ChildMortality,WHO$Region,mean)

which.min(c(4,1,6))


