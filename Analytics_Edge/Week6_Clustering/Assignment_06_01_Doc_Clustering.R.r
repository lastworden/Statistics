
dailykos = read.csv("dailykos.csv")

str(dailykos)

distance = dist(dailykos, method = "euclidean")

kosHierClust = hclust(distance, method = "ward.D")

plot(kosHierClust)

kosClusterGroups = cutree(kosHierClust, k = 7)

cluster1 = subset(dailykos, kosClusterGroups ==1 )
cluster2 = subset(dailykos, kosClusterGroups ==2 )
cluster3 = subset(dailykos, kosClusterGroups ==3 )
cluster4 = subset(dailykos, kosClusterGroups ==4 )
cluster5 = subset(dailykos, kosClusterGroups ==5 )
cluster6 = subset(dailykos, kosClusterGroups ==6 )
cluster7 = subset(dailykos, kosClusterGroups ==7 )

str(cluster3)

rep(1, 10)

tapply(rep(1,nrow(dailykos)), kosClusterGroups, sum)

table(kosClusterGroups)

HierCluster = split(dailykos, kosClusterGroups)

str(HierCluster)

tail(sort(colMeans(cluster1)))

tail(sort(colMeans(cluster2)))

tail(sort(colMeans(cluster3)))

tail(sort(colMeans(cluster4)))

tail(sort(colMeans(cluster5)))

tail(sort(colMeans(cluster6)))

tail(sort(colMeans(cluster7)))

set.seed(1000)
KMC = kmeans(dailykos, centers = 7)

table(KMC$cluster)

KMClusters = split(dailykos, KMC$cluster)

tail(sort(colMeans(KMClusters[[1]])))

tail(sort(colMeans(KMClusters[[2]])))

tail(sort(colMeans(KMClusters[[3]])))

table(kosClusterGroups, KMC$cluster)




