
letters = read.csv("letters_ABPR.csv")

str(letters)

summary(letters$letter)

levels(letters$letter)

letters$isB = as.factor(letters$letter == "B")

library(caTools)

set.seed(1000)

split = sample.split(letters$isB, SplitRatio = 0.5)

Train = subset(letters, split == TRUE)
nrow(Train)
Test = subset(letters, split == FALSE)
nrow(Test)

table(Train$isB)

1175/(1175+383)

library("rpart")
library("rpart.plot")

CARTb = rpart(isB ~ . - letter, data = Train, method = "class")

bPredTest = predict(CARTb, newdata = Test, type = "class")

t = table(Test$isB,bPredTest)

t

sum(diag(t))/sum(t)

library("randomForest")

set.seed(1000)
bForest = randomForest(isB ~ . - letter, data = Train)

bForestPredTest = predict(bForest, newdata = Test)

t = table(Test$isB,bForestPredTest)

t

sum(diag(t))/sum(t)

letters$letter = as.factor(letters$letter)

set.seed(2000)

split = sample.split(letters$letter, SplitRatio =  0.5)

Train1 = subset(letters, split == TRUE)
Test1 = subset(letters, split == FALSE)

table(Train1$letter)

402/nrow(Train1)

CARTall = rpart(letter~.-isB, data = Train1, method = "class")

allPredTest = predict(CARTall, newdata = Test1, type = "class")

t = table(Test1$letter, allPredTest)
t

sum(diag(t))/sum(t)

set.seed(1000)
allForest = randomForest(letter~.-isB, data = Train1)

allPredForestTest = predict(allForest, newdata = Test1)

t = table(Test1$letter,allPredForestTest)

t

sum(diag(t))/sum(t)


