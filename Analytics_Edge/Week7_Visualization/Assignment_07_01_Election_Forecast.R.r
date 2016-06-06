
library(ggplot2)

library(maps)

library(ggmap)

statesMap = map_data("state")

str(statesMap)

table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")



polling = read.csv("PollingImputed.csv")

str(polling)

table(polling$Year)

Train = subset(polling, Year <= 2008)

Test = subset(polling, Year > 2008)

sum(table(Test$State)>0)

mod2 = glm(Republican~SurveyUSA+DiffCount, data = Train, family = binomial)

summary(mod2)

TestPrediction = predict(mod2, newdata = Test, type = "response")

TestPredictionBinary = as.numeric(TestPrediction>0.5)

predictionDataFrame = data.frame(TestPrediction,TestPredictionBinary,Test$State)

str(predictionDataFrame)

sum(TestPredictionBinary == TRUE)

mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap,predictionDataFrame, by = "region")

str(predictionMap)

predictionMap=predictionMap[order(predictionMap$order),]

str(predictionMap)

ggplot(predictionMap,aes(x=long,y=lat,group=group, fill = TestPredictionBinary))+geom_polygon(color = "black")

ggplot(predictionMap,aes(x=long,y=lat,group=group, fill = TestPredictionBinary))+
geom_polygon(color = "black")+
scale_fill_gradient(low="blue",high="red",guide="legend", 
                    breaks = c(0,1),labels =c("Demorat","Republican"), name ="Prediction 2012" )

ggplot(predictionMap,aes(x=long,y=lat,group=group, fill = TestPrediction))+
geom_polygon(color = "black")+
scale_fill_gradient(low="blue",high="red",guide="legend",
                    breaks = c(0,1),labels =c("Demorat","Republican"), name ="Prediction 2012" )

TestPrediction[which(Test$State=="Florida")]

?geom_polygon

?linetype

ggplot(predictionMap,aes(x=long,y=lat,group=group, fill = TestPredictionBinary))+
geom_polygon(color = "black",linetype=3)+
scale_fill_gradient(low="blue",high="red",guide="legend", 
                    breaks = c(0,1),labels =c("Demorat","Republican"), name ="Prediction 2012" )

ggplot(predictionMap,aes(x=long,y=lat,group=group, fill = TestPredictionBinary))+
geom_polygon(color = "black",size=3)+
scale_fill_gradient(low="blue",high="red",guide="legend", 
                    breaks = c(0,1),labels =c("Demorat","Republican"), name ="Prediction 2012" )

ggplot(predictionMap,aes(x=long,y=lat,group=group, fill = TestPredictionBinary))+
geom_polygon(color = "black",alpha=0.3)+
scale_fill_gradient(low="blue",high="red",guide="legend", 
                    breaks = c(0,1),labels =c("Demorat","Republican"), name ="Prediction 2012" )


