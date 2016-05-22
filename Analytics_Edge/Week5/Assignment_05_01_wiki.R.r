
wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)

str(wiki)

sum(wiki$Vandal)

library(tm)

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded[[1]]$content

corpusAdded

length(stopwords("english"))

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded[[1]]$content

corpusAdded = tm_map(corpusAdded, stemDocument)

corpusAdded[[1]]$content

dtmAdded = DocumentTermMatrix(corpusAdded)

dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)

sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))

str(wordsAdded)

colnames(wordsAdded) = paste("A",colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument)

corpusRemoved[[2]]$content

dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

str(wordsRemoved)

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded,wordsRemoved)

str(wikiWords)

wikiWords$Vandal = wiki$Vandal

library(caTools)

set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

Train = subset(wikiWords, split == TRUE)
Test = subset(wikiWords, split == FALSE)

table(Train$Vandal)

1443/(1443+1270)

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal~., data = Train, method = "class")

CARTPredWiki = predict(wikiCART, newdata = Test, type = "class")

t = table(Test$Vandal,CARTPredWiki)
t

sum(diag(t))/sum(t)

prp(wikiCART)

wikiWords2 = wikiWords

wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE),1,0)

sum(wikiWords2$HTTP)

Train2 = subset(wikiWords2, split == TRUE)
Test2 = subset(wikiWords2, split == FALSE)

wikiCART2 = rpart(Vandal~., data = Train2, method = "class")

CARTPredWiki2 = predict(wikiCART2, newdata = Test2, type = "class")

t = table(Test2$Vandal,CARTPredWiki2)
t

sum(diag(t))/sum(t)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

Train3 = subset(wikiWords2, split == TRUE)
Test3 = subset(wikiWords2, split == FALSE)

wikiCART3 = rpart(Vandal~., data = Train3, method = "class")

CARTPredWiki3 = predict(wikiCART3, newdata = Test3, type = "class")

t = table(Test3$Vandal,CARTPredWiki3)
t

sum(diag(t))/sum(t)

wikiWords4 = wikiWords2

wikiWords4$Minor = wiki$Minor
wikiWords4$Loggedin = wiki$Loggedin

Train4 = subset(wikiWords4, split == TRUE)
Test4 = subset(wikiWords4, split == FALSE)

wikiCART4 = rpart(Vandal~., data = Train4, method = "class")

CARTPredWiki4 = predict(wikiCART4, newdata = Test4, type = "class")

t = table(Test4$Vandal,CARTPredWiki4)
t

sum(diag(t))/sum(t)

prp(wikiCART4)


