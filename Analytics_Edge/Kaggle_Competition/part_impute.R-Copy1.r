
pollTrain = read.csv("train2016.csv")

pollTrain$Party = ifelse(pollTrain$Party=="Democrat",1,0)
table(pollTrain$Party)

names(pollTrain)
nrow(pollTrain)

main_cols=c("USER_ID","YOB","Gender","Income","HouseholdStatus","EducationLevel","Party")

t=c()
t

for (col in setdiff(names(pollTrain),c("USER_ID","Party"))){
  #if mean(is.na(pollTrain[,col])
    cat(col,"cor:",cor(as.numeric(pollTrain[,col]),as.numeric(pollTrain$Party)),"\n")
    val = cor(as.numeric(pollTrain[,col]),as.numeric(pollTrain$Party))
    t = c(t,c(col=val))
    
  #print(paste("The year is", year))
}


which(abs(t)>0.05)

t["Gender"]


