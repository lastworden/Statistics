
tweets= read.csv("tweets.csv",stringsAsFactors = FALSE)
str(tweets)

library(tm)

library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus[[1]]$content

corpus = tm_map(corpus,tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus[[1]]$content

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus[[1]]$content

DTM = DocumentTermMatrix(corpus)

str(DTM)

findFreqTerms(DTM, lowfreq = 20)

allTweets = as.data.frame(as.matrix(DTM))

str(allTweets)

library(wordcloud)

.Library

?wordcloud

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4,.5))

corpus = Corpus(VectorSource(tweets$Tweet))

corpus[[1]]$content

corpus = tm_map(corpus,tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus[[1]]$content

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))


DTM = DocumentTermMatrix(corpus)


allTweets = as.data.frame(as.matrix(DTM))


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4,.5))

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4,.5), random.order = FALSE,max.words= 100)

corpus1 = Corpus(VectorSource(subset(tweets$Tweet,tweets$Avg<=-1)))

corpus1[[1]]$content

corpus1 = tm_map(corpus1,tolower)

corpus1 = tm_map(corpus1, PlainTextDocument)

corpus1[[1]]$content

corpus1 = tm_map(corpus1, removePunctuation)

corpus1 = tm_map(corpus1, removeWords, c("apple",stopwords("english")))


DTM1 = DocumentTermMatrix(corpus1)


allTweets1 = as.data.frame(as.matrix(DTM1))

wordcloud(colnames(allTweets1),colSums(allTweets),scale=c(4,.5),max.words= 500,random.order = FALSE)

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets)) 


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4,.5), color="purple", random.color=FALSE,max.words= 100)

library(RColorBrewer)

display.brewer.all()

?brewer.pal


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4,.5), colors=brewer.pal(9, "Blues"))

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4,.5), colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)] )


