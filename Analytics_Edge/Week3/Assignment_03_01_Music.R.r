
songs = read.csv("songs.csv")

str(songs)

sum(songs$year == 2010)

table(songs$artistname)

sum(songs$artistname == "Michael Jackson")

subset(songs,songs$artistname == "Michael Jackson" & songs$Top10 == 1)

sum(songs$artistname == "Michael Jackson" & songs$Top10 == 1)

subset(songs$songtitle,songs$artistname == "Michael Jackson" & songs$Top10 == 1)

sort(table(songs$timesignature))

songs$songtitle[which.max(songs$tempo)]

SongsTrain = subset(songs,songs$year<2010)

SongsTest = subset(songs,songs$year>=2010)

str(SongsTrain)

names(SongsTest)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

 names(SongsTrain) %in% nonvars

SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest) %in% nonvars)]

str(SongsTest)

model1 = glm(Top10~.,family = binomial,data = SongsTrain)

summary(model1)

cor(SongsTrain$loudness,SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

summary(SongsLog3)

predTest3 = predict(SongsLog3,type = "response", newdata = SongsTest)

table(SongsTest$Top10,predTest3 > 0.45)

#accuracy
(309+19)/(309+19+5+40)

table(SongsTest$Top10)

314/(314+59)

#sesitivity
19/(40+19)

#specificity
309/(309+5)

library(ROCR)

ROCRPred3 = prediction(predTest3,SongsTest$Top10)

ROCRPerf3 = performance(ROCRPred3,"tpr","fpr")

plot(ROCRPerf3,colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

auc3 = as.numeric(performance(ROCRPred3, "auc")@y.values)

auc3


