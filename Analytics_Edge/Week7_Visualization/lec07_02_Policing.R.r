
mvt = read.csv("mvt.csv", stringsAsFactor = FALSE)

str(mvt)

mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")

mvt$Weekday = weekdays(mvt$Date)

mvt$Hour = mvt$Date$hour

str(mvt)

table(mvt$Weekday)

WeekdayCounts = as.data.frame(table(mvt$Weekday))

str(WeekdayCounts)

library(ggplot2)

ggplot(WeekdayCounts, aes(x=Var1, y=Freq))+geom_line(aes(group = 1))

WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered = TRUE, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(WeekdayCounts, aes(x=Var1, y=Freq))+geom_line(aes(group = 1))

# Change x and y labels

ggplot(WeekdayCounts, aes(x=Var1, y=Freq))+geom_line(aes(group = 1))+xlab("Day of the Week")+ylab("Number of Crimes")

ggplot(WeekdayCounts, aes(x=Var1, y=Freq))+geom_line(aes(group = 1), linetype = 2)+xlab("Day of the Week")+ylab("Number of Crimes")

ggplot(WeekdayCounts, aes(x=Var1, y=Freq))+geom_line(aes(group = 1), alpha = 0.3)+xlab("Day of the Week")+ylab("Number of Crimes")

table(mvt$Weekday, mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts)

DayHourCounts$Hour =as.numeric(as.character(DayHourCounts$Var2))

str(DayHourCounts)

ggplot(DayHourCounts, aes(x=Hour,y=Freq)) + geom_line(aes(group = Var1))

ggplot(DayHourCounts, aes(x=Hour,y=Freq)) + geom_line(aes(group = Var1, color = Var1),size=2)

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(DayHourCounts,aes(x = Hour, y = Var1))+geom_tile(aes(fill = Freq))

ggplot(DayHourCounts,aes(x = Hour, y = Var1))+geom_tile(aes(fill = Freq))+scale_fill_gradient(name="Total MV Theft")+ ylab("Day of the Week")

ggplot(DayHourCounts,aes(x = Hour, y = Var1))+geom_tile(aes(fill = Freq))+scale_fill_gradient(name="Total MV Theft")+theme(axis.title.y= element_blank())

ggplot(DayHourCounts,aes(x = Hour, y = Var1))+geom_tile(aes(fill = Freq))+scale_fill_gradient(name="Total MV Theft",low="white",high="red")+theme(axis.title.y= element_blank())

library(maps)

library(ggplot2)
library(ggmap)

chicago = get_map(location = "chicago",zoom = 11)

ggmap(chicago)

ggmap(chicago)+geom_point(data= mvt[1:100,], aes(x = Longitude, y= Latitude))

LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))

str(LatLonCounts)

LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago)+geom_point(data = LatLonCounts, aes(x=Long, y= Lat,color=Freq,size=Freq))

ggmap(chicago)+geom_point(data = LatLonCounts, aes(x=Long, y= Lat,color=Freq,size=Freq))+scale_color_gradient(low = "yellow", high = "red")

ggmap(chicago)+geom_tile(data = LatLonCounts, 
                          aes(x=Long, y= Lat,alpha=Freq), fill = "red")

LatLonCounts2 = subset(LatLonCounts, Freq >0)

nrow(LatLonCounts)-nrow(LatLonCounts2)

ggmap(chicago)+geom_tile(data = LatLonCounts2, 
                          aes(x=Long, y= Lat,alpha=Freq), fill = "red")

murders = read.csv("murders.csv")

str(murders)

statesMap = map_data("state")

str(statesMap)

ggplot(statesMap, aes(x=long,y=lat,group=group))+geom_polygon(fill="white",color = "black")

murders$region = tolower(murders$State)

murderMap = merge(murders,statesMap, by="region")

str(murderMap)

ggplot(murderMap, aes(x=long,y=lat,group=group, 
                      fill = Murders))+geom_polygon(color = "black")+scale_fill_gradient(low="black",
                                                                                         high="red",
                                                                                        guide = "legend")


ggplot(murderMap, aes(x=long,y=lat,group=group, 
                      fill = Population))+geom_polygon(color = "black")+scale_fill_gradient(low="black",
                                                                                         high="red",
                                                                                        guide = "legend")


murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000

ggplot(murderMap, aes(x=long,y=lat,group=group, 
                      fill = MurderRate))+geom_polygon(color = "black")+scale_fill_gradient(low="black",
                                                                                         high="red",
                                                                                        guide = "legend")

ggplot(murderMap, aes(x=long,y=lat,group=group, fill = MurderRate))+
       geom_polygon(color = "black")+
        scale_fill_gradient(low="black",high="red",  guide = "legend", limit = c(1,10))

ggplot(murderMap, aes(x=long,y=lat,group=group, 
                      fill = GunOwnership))+geom_polygon(color = "black")+scale_fill_gradient(low="white",
                                                                                         high="red",
                                                                                        guide = "legend")


