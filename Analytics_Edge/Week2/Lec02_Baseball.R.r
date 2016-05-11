
baseball = read.csv("baseball.csv")

str(baseball)

moneyball = subset(baseball,Year < 2002)

str(moneyball)

moneyball$RD = moneyball$RS-moneyball$RA

summary(moneyball$RD)

mean(moneyball$RS)
mean(moneyball$RA)
mean(moneyball$RD)

plot(moneyball$RD,moneyball$W)

WinsReg= lm(W~RD, data = moneyball)

summary(WinsReg)

# for win to  be > 95
# RD >(95-80.881375)/0.105766
(95-80.881375)/0.105766

80.881375 + 0.105766*(713-614)

str(moneyball)

cor(moneyball$W,moneyball$RD)

class(moneyball)

cor(moneyball$RS,moneyball$OBP)
cor(moneyball$RS,moneyball$SLG)
cor(moneyball$RS,moneyball$BA)

RunsReg = lm(RS~OBP+SLG+BA,data = moneyball)

summary(RunsReg)

sqrt(mean(RunsReg$residuals^2))

RunsReg = lm(RS~OBP+SLG,data = moneyball)

summary(RunsReg)

RunsReg = lm(RS~OBP+BA,data = moneyball)
summary(RunsReg)

mean(b^2)

RunsAllowedReg = lm(RA~OOBP+OSLG,data = moneyball)

summary(RunsAllowedReg)

-837+2913.60*0.297+1514.29*0.370

 
-804.63+2737.77*0.311+1584.91*0.405

RunsReg = lm(RS~OBP+SLG,data = moneyball)

summary(RunsReg)

func1 = function(obp,slg)
    return (-804.63+obp*2737.77+slg*1584.91)

func1(0.338,0.540)
func1(0.391,0.450)
func1(0.369,0.374)
func1(0.313,0.447)
func1(0.361,0.500)

teamRank = c(1,2,3,3,4,4,4,4,5,5)

teamRank

wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

#What is the correlation between teamRank and wins2012?
cor(teamRank,wins2012)

cor(teamRank,wins2013)


