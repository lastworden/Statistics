
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)

str(trials)

summary(trials)

max(nchar(trials$abstract))

sum(nchar(trials$abstract) == 0)

which.min(nchar(trials$title))

trials$title[1258]

library(tm)

library(SnowballC)

corpusTitle = Corpus(VectorSource(trials$title))

corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)

corpusTitle[[1]]$content

dtmTitle = DocumentTermMatrix(corpusTitle)

corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
corpusAbstract[[1]]$content

dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle

dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract

dtmTitle = as.data.frame(as.matrix(dtmTitle))

dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)

str(dtmAbstract)

 length(stopwords("english"))

t = colSums(dtmAbstract)

which.max(t)

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A",colnames(dtmAbstract))

colnames(dtmTitle)

dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial = trials$trial

str(dtm)

set.seed(144)

library(caTools)

split = sample.split(dtm$trial, SplitRatio = 0.7)

train = subset(dtm, split == TRUE)

test = subset(dtm, split == FALSE)

table(train$trial)

730/(730+572)
library(rpart)
library(rpart.plot)

trialCART = rpart(trial~., data = train, method = "class")

prp(trialCART)



CARTTrainPred = predict(trialCART, newdata = train)

max(CARTTrainPred[,2])

CARTTestPred = predict(trialCART, newdata = test)

max(CARTTestPred[,2])

CARTTrainPred = predict(trialCART, newdata = train, type = "class")

summary(CARTTrainPred)

t= table(train$trial, CARTTrainPred)
t


sum(diag(t))/sum(t)

441/(441+131)

631/(631+99)

CARTTestPred = predict(trialCART, newdata = test)

t = table(test$trial, CARTTestPred[,2]>0.5)
t

sum(diag(t))/sum(t)

library(ROCR)

CARTTestPredRes = CARTTestPred[,2]

as.numeric(performance(prediction(CARTTestPredRes,test$trial),"auc")@y.values)

plot(performance(prediction(CARTTestPredRes,test$trial),"tpr","fpr"))


