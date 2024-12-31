setwd("D:/Term 2/Machine Learning/Working Directory") #Working directory so that we can import by placing the file in the same folder
insurance <- read.csv("insurance_LR1.csv",stringsAsFactors = TRUE)
insurance
str(insurance)
summary(insurance)
View(insurance) #charges is dependent variable

insurance$children <- as.factor(insurance$children)
summary(insurance)

#is.na(insurance$bmi) → Checking for missing values
# → is.na(insurance$bmi)

#Impute missing values in BMI with mean
# → insurance$bmi[is.na(insurance$bmi)] <- mean(insurance$bmi,na.rm=TRUE)

#na.rm means to calculate mean and using total values in the denominator equal to the total values - missing values

#Replacing missing values with 0
# → insurance$bmi[is.na(insurance$bmi)] <- 0

#Removing rows with missing values
# → insurance <- na.omit(insurance)

library(dplyr)
#using dplyr package to remove missing values from specific column
insurance <- insurance %>% filter(!is.na(insurance$bmi))
# ! is the not symbol. that means update the data without the missing values
View(insurance)
summary(insurance)


library(car)
library(corrplot) # to get correlation chart
library(caret)
library(caTools)
library(psych)

#get correlation
cr <- cor(insurance[c("age","bmi","charges")])
cr
corrplot(cr,type='full') #to get correlation chart
write.csv(cr,"corbda.csv") #to export the file
corrplot.mixed(cr) #used to create a correlation plot when you have both continuous and categorical variables.

###
insurance$smoke_yes <- ifelse(insurance$smoker=="yes",1,0)
insurance$smoke_no <- ifelse(insurance$smoker=="no",1,0)
View(insurance)

library(fastDummies)
insurance <- dummy_cols(insurance,select_columns = c('region'))
View(insurance)

cr <- cor(insurance[c("age","bmi","charges","smoke_yes")])
cr
corrplot(cr,type='full')

library(caTools) #Training & Testing

### Step - 4
split <- sample.split(insurance$charges,SplitRatio = 0.7)  #charges is a reference column
training_data <- subset(insurance,split=="TRUE")  #Randomly writing TRUE to 70% of the dataset
testing_data <- subset(insurance,split=="FALSE")
View(training_data)
View(testing_data)

pairs.panels(training_data[c("age","bmi","smoke_yes","charges")]) #create a scatterplot matrix 

## Step - 5

model1 <- lm(charges ~ age, data = training_data)
summary(model1)

model2 <- lm(charges ~ age+bmi, data = training_data)
summary(model2)

model3 <- lm(charges ~ age+bmi+smoke_yes, data = training_data) #smoking status is a strong predictor of charges
summary(model3)

library(HH)
vif(model3)
plot(model3$fitted.values,model3$residuals)
lmtest::bptest(model3)

#Step -6 Testing the model
Prediction <- predict(model3,testing_data)
head(Prediction)
head(testing_data$charges)

plot(testing_data$charges,type="l",col="green")
lines(Prediction,type="l",col="blue")