
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)

str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

# Install package tm, SnowballC

library("tm")

library("SnowballC")

corpus = Corpus(VectorSource(tweets$Tweet))

corpus

corpus[[1]]

corpus = tm_map(corpus,tolower)

corpus

corpus[1][1]


