
healthy = read.csv("healthy.csv", header = FALSE)

str(healthy)

healthyMatrix = as.matrix(healthy)

str(healthyMatrix)

#image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

healthyVector = as.vector(healthyMatrix)

str(healthyVector)

n = 365636
n*(n-1)/2

#distance = dist(healthyVector, method = "euclidean")

k = 5
set.seed(1)

KMC = kmeans(healthyVector, centers = k, iter.max = 1000)

str(KMC)

healthyClusters = KMC$cluster

KMC$centers

dim(healthyClusters) = dim(healthyMatrix)

dim(healthyClusters)


#image(healthyClusters, axes = FALSE, col = rainbow(k))

tumor = read.csv("tumor.csv", header = FALSE)

tumorMatrix = as.matrix(tumor)

tumorVector = as.vector(tumorMatrix)

str(tumorVector)

dim(tumorMatrix)

library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)

tumorClusters = predict(KMC.kcca, newdata = tumorVector)

dim(tumorClusters) = dim(tumorMatrix)

#image(tumorClusters, axes = FALSE, col = rainbow(k))

KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)
KMC3 = kmeans(healthyVector, centers = 3, iter.max = 1000)
KMC4 = kmeans(healthyVector, centers = 4, iter.max = 1000)
KMC5 = kmeans(healthyVector, centers = 5, iter.max = 1000)
KMC6 = kmeans(healthyVector, centers = 6, iter.max = 1000)
KMC7 = kmeans(healthyVector, centers = 7, iter.max = 1000)
KMC8 = kmeans(healthyVector, centers = 8, iter.max = 1000)

NumClusters = seq(2,8,1)

SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC5$withinss), sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss))

plot(NumClusters, SumWithinss, type="b")


