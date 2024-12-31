#IMPORT LIBRARIES
library(corrplot) #For correlation
library(caret) #For Testing
library(caTools) #For Splitting the data
library(psych) # For Pair.panel chart
library(HH)
library(tidyverse)
library(fastDummies)
library(lmtest)
library(skedastic)

#READ AND VIEW THE DATASET
MH <- read.csv("Mission Hospital.csv",stringsAsFactors = TRUE)
summary(MH)
View(MH)
str(MH)


#MH <- na.omit(MH)
#OR
#To replace blank cell with NA
#MH <- read.csv("mission_hospital.csv",stringsAsFactors = TRUE, na.strings=c(""," ","NA"))
#summary(MH)
#Transform the variable


library(fastDummies)
MH <- dummy_cols(MH, select_columns = c('MODE.OF.ARRIVAL','GENDER','TYPE.OF.ADMSN'))
View(MH)

#FIND THE CORRELATION
#cr <- cor(MH[c("AGE","BODY.WEIGHT","BODY.HEIGHT", "TOTAL.LENGTH.OF.STAY","LENGTH.OF.STAY...ICU","COST.OF.IMPLANT","TOTAL.COST.TO.HOSPITAL")])
cr <- cor(MH[sapply(MH,is.numeric)]) 

#to see the result stored in cr
cr
#plot correlation chart (dots)
corrplot(cr, type="full")
#plot correlation chart (number)
corrplot.mixed(cr)
write.csv(cr,"MH_COR.csv")

#Since Columns with missing values do not have high correlation dropping those columns

MH <- subset(MH,select =  -c(BP..HIGH,BP.LOW,HB,UREA,CREATININE))
summary(MH)

pairs.panels(MH[c("AGE","BODY.WEIGHT","BODY.HEIGHT", 
                  "TOTAL.LENGTH.OF.STAY",
                  "LENGTH.OF.STAY...ICU",
                  "COST.OF.IMPLANT","TOTAL.COST.TO.HOSPITAL")])

#The pairs.panels function is commonly used for creating scatterplot matrices, which show scatterplots of all possible pairs of variables along the diagonal, and additional summary plots (e.g., histograms) on the off-diagonal elements.

MH$TOTAL.COST.TO.HOSPITAL <- log(MH$TOTAL.COST.TO.HOSPITAL)
View(MH)

#Splitting the data set into Training and testing
#split <- sample.split(MH$TOTAL.COST.TO.HOSPITAL,SplitRatio = 0.7)
#training_data <- subset(MH, split=="TRUE")
#testing_data <- subset(MH, split=="FALSE")
#View(training_data)
#View(testing_data)

#LINEAR REGRESSION MODEL
library(car) ###car or HH any one
library(HH)
library(lmtest)
library(skedastic)

model1 <- lm(TOTAL.COST.TO.HOSPITAL~AGE,data=MH) 
summary(model1)
lmtest::bptest(model1)
skedastic::white(model1)
plot(model1$fitted.values,model1$residuals)

model2 <- lm(TOTAL.COST.TO.HOSPITAL~AGE+BODY.WEIGHT,data=MH) 
summary(model2)
vif(model2)
lmtest::bptest(model2)
skedastic::white(model2)
plot(model2$fitted.values,model2$residuals)

model3 <- lm(TOTAL.COST.TO.HOSPITAL~AGE+BODY.WEIGHT+BODY.HEIGHT,data=MH) 
summary(model3)
vif(model3)
lmtest::bptest(model3)
skedastic::white(model3)
plot(model3$fitted.values,model3$residuals)

model4 <- lm(TOTAL.COST.TO.HOSPITAL~AGE+BODY.WEIGHT+BODY.HEIGHT+TOTAL.LENGTH.OF.STAY,data=MH) 
summary(model4)
vif(model4)
lmtest::bptest(model4)
skedastic::white(model4)
plot(model4$fitted.values,model4$residuals)

model5 <- lm(TOTAL.COST.TO.HOSPITAL~AGE+BODY.WEIGHT+BODY.HEIGHT+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU,data=MH) 
summary(model5)
vif(model5)
lmtest::bptest(model5)
skedastic::white(model5)
plot(model5$fitted.values,model5$residuals)

model6 <- lm(TOTAL.COST.TO.HOSPITAL~AGE+BODY.WEIGHT+BODY.HEIGHT+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+COST.OF.IMPLANT,data=MH) 
summary(model6)
vif(model6)
lmtest::bptest(model6)
skedastic::white(model6)
plot(model6$fitted.values,model6$residuals)

model7 <- lm(TOTAL.COST.TO.HOSPITAL~AGE+BODY.WEIGHT+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+COST.OF.IMPLANT,data=MH) 
summary(model7)
vif(model7)
lmtest::bptest(model7)
skedastic::white(model7)
plot(model7$fitted.values,model7$residuals)

model8 <- lm(TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT+LENGTH.OF.STAY..WARD+LENGTH.OF.STAY...ICU+COST.OF.IMPLANT,data=MH) 
summary(model8)
vif(model8)
lmtest::bptest(model8) #Breusch-Pagan test , White test and VIF for heteroscedasticity in linear regression models
# VIF shoule be less than 5
# p value in BP test and White test should be less than 0.05
skedastic::white(model8) 
plot(model8$fitted.values,model8$residuals)

#Prediction for same dataset
prediction <- predict(model8,MH)

#To convert log transformation to exponetial
prediction <- exp(prediction)
head(prediction)

#To convert transformation to exponetial
ML <- exp(MH$TOTAL.COST.TO.HOSPITAL)
head(ML)

plot(ML,type="l",col="green")
lines(prediction,type="l",col="blue")