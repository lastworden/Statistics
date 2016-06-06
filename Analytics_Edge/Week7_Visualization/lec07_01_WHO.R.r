
WHO = read.csv("WHO.csv")

str(WHO)

plot(WHO$GNI,WHO$FertilityRate)

library(ggplot2)

scatterplot = ggplot(WHO, aes(x = GNI,y = FertilityRate))

scatterplot + geom_point()

scatterplot+geom_line()

scatterplot+geom_point(color = "blue", shape = 17, size = 3)

FR_GNIPlot = scatterplot+geom_point(color = "darkred", shape = 8, size = 3)+ggtitle("Fertility Rate Vs. Gross National Income")

FR_GNIPlot

pdf("generated_plot.pdf")
print(FR_GNIPlot)
dev.off()

scatterplot+geom_point(color = "darkred", shape = 15, size = 3)+ggtitle("Fertility Rate Vs. Gross National Income")

ggplot(WHO, aes(x = GNI,y = FertilityRate, color = Region))+ geom_point()

ggplot(WHO, aes(x = GNI,y = FertilityRate, color = LifeExpectancy))+ geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15))+ geom_point()

ggplot(WHO, aes(x = log(FertilityRate), y = Under15))+ geom_point()

cor(WHO)

model = lm(Under15~FertilityRate, data = WHO)

summary(model)

model = lm(Under15~log(FertilityRate), data = WHO)

summary(model)

ggplot(WHO, aes(x = log(FertilityRate), y = Under15))+ geom_point()+stat_smooth(method = lm)

ggplot(WHO, aes(x = log(FertilityRate), y = Under15))+ geom_point()+stat_smooth(method = lm, level = 0.99)

ggplot(WHO, aes(x = log(FertilityRate), y = Under15))+ geom_point()+stat_smooth(method = lm, se = FALSE)

ggplot(WHO, aes(x = log(FertilityRate), y = Under15))+ geom_point()+stat_smooth(method = lm, se = FALSE, color = "orange")

ggplot(WHO, aes(x = FertilityRate, y = Under15,  color = Region)) + geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15,  color = Region)) + geom_point()+scale_color_brewer(palette="Dark2")


