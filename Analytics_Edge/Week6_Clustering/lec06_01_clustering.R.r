
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


