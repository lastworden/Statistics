
emails = read.csv("emails.csv", stringsAsFactors = FALSE)

str(emails)

sum(emails$spam)

emails[1:5,]

max(nchar(emails$text))

which.min(nchar(emails$text))

library(tm)
library(SnowballC)
library(caTools)

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

dtm

spdtm = removeSparseTerms(dtm, 0.95)

spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))

str(emailsSparse)

colnames(emailsSparse) = make.names(colnames(emailsSparse))

colnames(emailsSparse)

t=colSums(emailsSparse)
which.max(t)


which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam

max(t)

sort(colSums(emailsSparse), decreasing = TRUE) 

sum(emailsSparse$spam)

sort(colSums(subset(emailsSparse, spam ==0)), decreasing = TRUE) 

sort(colSums(subset(emailsSparse, spam ==1)), decreasing = TRUE)

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)

split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

library(rpart)
library(rpart.plot)
library(randomForest)

spamLog = glm(spam~., data = train, family = binomial)

spamCART = rpart(spam~., data = train, method = "class")

set.seed(123)
spamRF = randomForest(spam~., data = train)

predTrainLog = predict(spamLog, type = "response")

summary(predTrainLog)

sum(predTrainLog < 0.00001)

sum(predTrainLog > 0.99999)

sum(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

length(predTrainLog)

nrow(train)

predTrainCART = predict(spamCART)[,2]

predTrainRF = predict(spamRF, type = "prob")[,2]

summary(spamLog)

prp(spamCART)

t = table(train$spam, predTrainLog> 0.5)
t

sum(diag(t))/nrow(train)

library(ROCR)

as.numeric(performance(prediction(predTrainLog,train$spam),"auc")@y.values)

t = table(train$spam, predTrainCART>0.5)
t

sum(diag(t))/nrow(train)

as.numeric(performance(prediction(predTrainCART,train$spam),"auc")@y.values)

t = table(train$spam, predTrainRF>0.5)
t

sum(diag(t))/nrow(train)

as.numeric(performance(prediction(predTrainRF,train$spam),"auc")@y.values)

predTestLog = predict(spamLog,newdata = test, type = "response")

t = table(test$spam, predTestLog>0.5)
t

sum(diag(t))/nrow(test)

as.numeric(performance(prediction(predTestLog,test$spam), "auc")@y.values)

predTestCART = predict(spamCART, newdata = test)[,2]

t = table(test$spam, predTestCART>0.5)
t
sum(diag(t))/nrow(test)
as.numeric(performance(prediction(predTestCART,test$spam), "auc")@y.values)

predTestRF = predict(spamRF, newdata = test, type = "prob")[,2]

t = table(test$spam, predTestRF>0.5)
t
sum(diag(t))/nrow(test)
as.numeric(performance(prediction(predTestRF,test$spam), "auc")@y.values)


