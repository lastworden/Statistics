
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)

str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

# Install package tm, SnowballC

Sys.setlocale("LC_ALL", "C")

library(tm)

library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus

corpus[[1]]$content

corpus = tm_map(corpus,tolower)

corpus

corpus[[1]]

#Convert to plain text
corpus = tm_map(corpus, PlainTextDocument)

corpus[[1]]$content

corpus = tm_map(corpus, removePunctuation)

corpus

corpus[[1]]$content

stopwords("english")[1:10]

corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

corpus[[1]]$content

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)

frequencies

inspect(frequencies[1000:1005,505:515])

findFreqTerms(frequencies, lowfreq = 20)

sparse = removeSparseTerms(frequencies, 0.995)

sparse

tweetsSparse = as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

nrow(tweetsSparse)

nrow(tweets)

tweetsSparse$Negative = tweets$Negative

library(caTools)

set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

findFreqTerms(frequencies, lowfreq = 100)

library(rpart)

library(rpart.plot)

tweetCART = rpart(Negative~., data = trainSparse, method = "class")

prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type = "class")

t = table(testSparse$Negative,predictCART)
t

sum(diag(t))/sum(t)

table(testSparse$Negative)

1-55/355

library(randomForest)

set.seed(123)

tweetRF = randomForest(Negative~., data = trainSparse)

predictRF = predict(tweetRF, newdata = testSparse)

t = table(testSparse$Negative,predictRF)
t

sum(diag(t))/sum(t)

tweetLog= glm(Negative~., data= trainSparse, family = binomial)

predictLOG = predict(tweetLog, newdata = testSparse, type = "response")

summary(predictLOG)

predictLOG[1:10]

t = table(testSparse$Negative,predictLOG > 0.5)
t

sum(diag(t))/sum(t)


