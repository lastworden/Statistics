
climate_change = read.csv("climate_change.csv")

str(climate_change)

climate_train = subset(climate_change,Year <= 2006)
climate_test = subset(climate_change,Year > 2006)
str(climate_train)
str(climate_test)

cl_model1 = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=climate_train)

summary(cl_model1)

cor(climate_change)>0.7 | cor(climate_change) < -0.7

cor(climate_change)

cor(climate_change)["CFC.11", ]

summary(cl_model1)

cl_model2 = lm(Temp ~ MEI + N2O +  
    TSI + Aerosols, data = climate_train)

summary(cl_model2)

cl_model3 = step(cl_model1)

summary(cl_model3)

pred_temp = predict(cl_model3,newdata = climate_test)
pred_temp

mean(climate_change$Temp)

climate_test$Temp

climate_test$Temp-mean(climate_change$Temp)

TE = sum((climate_test$Temp-mean(climate_train$Temp))^2)

TE

ME = sum((pred_temp-climate_test$Temp)^2)
ME

1-(ME/TE)




