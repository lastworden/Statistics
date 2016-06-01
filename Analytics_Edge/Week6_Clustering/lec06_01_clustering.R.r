
movies = read.table("movieLens.txt", header = FALSE, sep = "|", quote = "\"")

summary(movies)

str(movies)

colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

movies = unique(movies)

str(movies)

table(movies$Comedy)

table(movies$Western)

table(movies$Romance,movies$Drama)

distances = dist(movies[2:20], method = "euclidean")


str(distances)

nrow(movies)

1664**2

class(distances)

clusterMovies = hclust(distances, method = "ward")

plot(clusterMovies)

clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)

tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title == "Men in Black (1997)")

clusterGroups[257]

cluster2 = subset(movies,clusterGroups == 2)

cluster2$Title[1:10]

clusterGroups2 = cutree(clusterMovies, k = 2)

cluster_new1 = subset(movies, clusterGroups2 == 1)

summary(cluster_new1)

cluster_new2 = subset(movies, clusterGroups2 == 2)

summary(cluster_new2)

colMeans(subset(movies[2:20], clusterGroups2 == 1))

colMeans(subset(movies[2:20], clusterGroups2 == 2))

spl = split(movies[2:20], clusterGroups2)



lapply(spl, colMeans)


