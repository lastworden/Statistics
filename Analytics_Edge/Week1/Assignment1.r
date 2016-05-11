
setwd("/Users/varunm/Projects/vm/R/Analytics_Edge/Week1")

getwd()

mvt = read.csv("mvtWeek1.csv")

str(mvt)

max(mvt$ID)

min(mvt$Beat)

nrow(subset(mvt,Arrest == TRUE))

sum(mvt$Arrest == TRUE)

sum(mvt$LocationDescription == "ALLEY")

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

str(mvt)


mvt$Date = DateConvert

str(mvt)

table(mvt$Month)
which.min(table(mvt$Month))

table(mvt$Weekday)
which.max(table(mvt$Weekday))

?which.min

table(subset(mvt,Arrest == TRUE)$Month)
which.max(table(subset(mvt,Arrest == TRUE)$Month))

table(mvt$Arrest,mvt$Month)

hist(mvt$Date, breaks=100)

boxplot(mvt$Date~mvt$Arrest)

table(mvt$Arrest,mvt$Year)

2152/(18517+2152)

1212/(1212+13068)

550/(550+13542)

sort(table(mvt$LocationDescription))

Top5 = subset(mvt,LocationDescription == "STREET"
             |LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"
             |LocationDescription == "ALLEY"
             |LocationDescription == "GAS STATION"
             |LocationDescription == "DRIVEWAY - RESIDENTIAL")

str(Top5)

TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")

Top5 = subset(mvt, LocationDescription %in% TopLocations)

nrow(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)

table(Top5$LocationDescription)

summary(Top5)

table(Top5$Arrest,Top5$LocationDescription)

249/(249+2059)

132/(132+1543)

439/(439+1672)

1603/(1603+13249)

11595/(11595+144969)

table(Top5$LocationDescription,Top5$Weekday)


IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

str(IBM)
str(GE)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

min(IBM$Date)
min(GE$Date)
min(CocaCola$Date)
min(ProcterGamble$Date)
min(Boeing$Date)

max(IBM$Date)
max(GE$Date)
max(CocaCola$Date)
max(ProcterGamble$Date)
max(Boeing$Date)

mean(IBM$StockPrice)

min(GE$StockPrice)

max(CocaCola$StockPrice)

summary(Boeing$StockPrice)

summary(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],col = "red", ylim = c(0,210),type = "l")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col = "blue")
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col = "green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col = "purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col = "orange")

#Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],col = "red", ylim = c(0,210),type = "l")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col = "blue")
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col = "green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col = "purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col = "orange")

#Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)
abline(v=as.Date(c("2004-01-01")), lwd=2)

plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432], ylim = c(0,210),type = "l")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],lty=2)
lines(IBM$Date[301:432],IBM$StockPrice[301:432],lty=3)
lines(GE$Date[301:432],GE$StockPrice[301:432],lty=4)
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],lty=5)


ls()

tapply(IBM$StockPrice,months(IBM$Date),mean)

mean(IBM$StockPrice)

tapply(IBM$StockPrice,months(IBM$Date),mean)
tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date),mean)

tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)
tapply(GE$StockPrice,months(GE$Date),mean)
tapply(Boeing$StockPrice,months(Boeing$Date),mean)


