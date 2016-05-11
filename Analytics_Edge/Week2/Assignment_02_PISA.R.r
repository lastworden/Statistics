
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

str(pisaTrain)

#Using tapply() on pisaTrain, what is the average reading test score of males?

tapply(pisaTrain$readingScore,pisaTrain$male,mean)

summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

str(pisaTest)

str(pisaTrain)

table(pisaTrain$grade)

table(pisaTrain$male)

sort(table(pisaTrain$raceeth))

table(pisaTrain$raceeth)

str(pisaTrain$raceeth)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

table(pisaTrain$raceeth)

lmScore = lm(readingScore ~ ., data = pisaTrain)

summary(lmScore)

sqrt(mean(lmScore$residuals^2))

sqrt(9)

  29.542707*2

predTest = predict(lmScore,newdata = pisaTest)

summary(predTest)

637.7 - 353.2

predDiff = predTest-pisaTest$readingScore

SSE = sum(predDiff^2)
SSE

sqrt(mean(predDiff^2))

baseline = mean(pisaTrain$readingScore)
baseline

SST = sum((pisaTest$readingScore-baseline)^2)
SST

1-(SSE/SST)


