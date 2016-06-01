
flower = read.csv("flower.csv", header = FALSE)

dim(flower)

str(flower)

flowerMatrix = as.matrix(flower)

str(flowerMatrix)

flowerVector = as.vector(flowerMatrix)

str(flowerVector)

distance = dist(flowerVector, method = "euclidean")

str(distance)

clusterIntensity = hclust(distance, method = "ward")

plot(clusterIntensity)

plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 3, border = "red")


flowerClusters = cutree(clusterIntensity, k = 3)

dim(flowerClusters)

flowerClusters

tapply(flowerVector,flowerClusters, mean)

dim(flowerClusters) = c(50,50)

image(flowerClusters)

image(flowerMatrix, axes = FALSE, col = grey(seq(0,1, length = 256)))


