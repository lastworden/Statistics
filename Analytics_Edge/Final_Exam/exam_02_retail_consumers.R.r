
households = read.csv("Households.csv")

str(households)

summary(households$MorningPct)

sum(households$AfternoonPct==100)

sum(households$MorningPct==100)

tapply(households$AvgDiscount,households$AvgSalesValue>150, min)

tapply(households$AvgSalesValue,households$AvgDiscount>25, min)

table(households$NumVisits>=300)

148/(148+2352)

summary(households)

library(caret)

preproc = preProcess(households)

HouseHoldsNorm = predict(preproc,households)

summary(HouseHoldsNorm)

max(HouseHoldsNorm$NumVisits)

min(HouseHoldsNorm$AfternoonPct)


set.seed(200)
distances <- dist(HouseHoldsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(200)
KMC = kmeans(HouseHoldsNorm,centers = 10)

KMC$centers

HouseClusters = KMC$cluster

sort(table(HouseClusters))

spl = split(HouseHoldsNorm, HouseClusters)

lapply(spl, colMeans)

spl = split(households, HouseClusters)

lapply(spl, colMeans)

set.seed(5000)
KMC1 = kmeans(HouseHoldsNorm,centers = 5)

sort(table(KMC1$cluster))

spl = split(households, KMC1$cluster)
lapply(spl, colMeans)


