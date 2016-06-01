
airlines = read.csv("AirlinesCluster.csv")

str(airlines)

summary(airlines)

library(caret)

preproc =preProcess(airlines)

airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

n = nrow(airlines)
n*(n-1)/2

airDist = dist(airlinesNorm, method = "euclidean")

airHeirClust = hclust(airDist, method = "ward.D")

plot(airHeirClust)

airHierClusters = cutree(airHeirClust, k = 5)

table(airHierClusters)

airHierClustInd = split(airlines,airHierClusters)

colMeans(airHierClustInd[[1]])

tapply(airlines$DaysSinceEnroll, airHierClusters, mean)

lapply(split(airlines, airHierClusters), colMeans)

set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5,iter.max = 1000)

table(KMC$cluster)

table(KMC$cluster, airHierClusters)


