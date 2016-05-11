
getwd()

CPS = read.csv("CPSData.csv")

str(CPS)

table(CPS$Industry)

sort(table(CPS$State))

table(CPS$Citizenship)

levels(CPS$Citizenship)

(116639+7073)/(116639+7073+7590)

table(CPS$Hispanic)

Hisp = subset(CPS,Hispanic == 1)

str(Hisp)

sort(table(Hisp$Race))

summary(CPS)

table(CPS$Region,is.na(CPS$Married))

table(CPS$Age,is.na(CPS$Married))

table(CPS$State,is.na(CPS$MetroAreaCode))

table(CPS$Region,is.na(CPS$MetroAreaCode))

tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)

abs(-2.3)


which.min(abs(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)-0.3))

sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

str(MetroAreaMap)

str(CountryMap)

CPS = merge(CPS,MetroAreaMap, by.x = "MetroAreaCode", by.y = "Code",all.x = TRUE)


str(CPS)

head(CPS,100)

CPS[1:4,]

summary(CPS)

sum(is.na(CPS$MetroArea))

t1 = subset(CPS,MetroArea %in% c("Atlanta-Sandy Springs-Marietta, GA","Baltimore-Towson, MD","Boston-Cambridge-Quincy, MA-NH","San Francisco-Oakland-Fremont, CA"))


sort(table(t1$MetroArea),decreasing=TRUE)

sort(tapply(CPS$Hispanic,CPS$MetroArea,mean),decreasing = TRUE)

sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean),decreasing = TRUE)

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

table(CPS$Education)

c(1,2,3)*4


CPS = merge(CPS,CountryMap,by.x = "CountryOfBirthCode",by.y = "Code",all.x = TRUE )

str(CPS)

summary(CPS$Country)

sort(table(CPS$Country),decreasing = TRUE)

table(CPS$Region)

sum(CPS$Country != "United States",na.rm= TRUE)

table(CPS$MetroArea, CPS$Country != "United States")

1668/(1668+3736)

table(CPS$MetroArea, CPS$Country == "India")

sort(tapply(CPS$Country == "India",CPS$MetroArea,sum,na.rm=TRUE),decreasing = TRUE)

sort(tapply(CPS$Country == "Somalia",CPS$MetroArea,sum,na.rm=TRUE),decreasing = TRUE)


