setwd("/cloud/project")

sleepdata <- read.csv("Sleep_health_and_lifestyle_dataset.csv", header = TRUE)

attach(sleepdata)
#this will allows to name variables the way they are

names(sleepdata)

#Ho: data is not normal; ha: data is normal type I error set to 0.05 reject null
#hypothesis that the outcome is not normal and conclude data is normal
shapiro.test(Sleep.Duration)
#Histogram of the outcome shows to be a bimodal distribution.
#This could be an indication that sleep duration might differ
#by a particular group - maybe gender.
#Given our large sample size of n=374 observations, by the
#central limit theorem, we can conclude normality approximation.
#P-value is normal

hist(Sleep.Duration)
#creates a new variable systolic extracting the first 3 digits of 
#the bloodpressure

is.character(Blood.Pressure)

sleepdata$systolic = substr(Blood.Pressure, 1, 3)

sleepdata$systolic = as.numeric(sleepdata$systolic)

sleepdata$diastolic = substr(Blood.Pressure, 5, 6) 

sleepdata$diastolic = as.numeric(sleepdata$diastolic)

install.packages("leaps")

library(leaps)

#Now we run the regsubsets to find the best model

output <- regsubsets(Sleep.Duration ~ Gender + Age + Occupation + Quality.of.Sleep + Physical.Activity.Level +
                       Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
                       Sleep.Disorder + systolic + diastolic, data=sleepdata, nvmax=12)

summOut1 <- summary(output)

summOut1

n1 <- length(Sleep.Duration)
n1

p1 <- apply(summOut1$which, 1, sum)

aic1 <- summOut1$bic - log(n1) * p1 + 2 * p1

plot(p1, aic1, ylab = "AIC1")

#best model is the one with all the predictors as it has the lowest AIC
model1 <- lm(Sleep.Duration ~ Gender + Age + Occupation + Quality.of.Sleep + 
               Physical.Activity.Level +
               Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
               Sleep.Disorder + systolic + diastolic, data=sleepdata)

summary(model1)

table(Occupation)

table(BMI.Category)

table(Sleep.Disorder)


#interpretation of the significant variables from this model:
#Sleep duration increases significantly by 0.027 units for every unit
#increase in age, adjusting for everything else

#Sleep Duration increases significantly by 0.027 units for every unit
#increase in age, adjusting for everything else

#Sleep Duration increases significantly by 0.83 units for Doctors
# vs accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.78 units for Engineers
# vs. Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.73 units for Lawyers
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.24 units for Nurses
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 1.45 units for Sales Reps
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.63 units for SalesPerson
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.46 units for Scientists
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.63 units for SoftwareEngineers
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.29 units for Teachers
#vs Accountants, adjusting for everything else

#Sleep Duration increases significantly by 0.29 units for every unit
#increase in quality of sleep score, adjusting for everything else

#Sleep Duration increases significantly by 0.009 units for every unit
#increase in physical activity level score, adjusting for everything else

#Sleep Duration decreases significantly by 0.16 units for every unit
#increase in stress level score, adjusting for everything else

#Increase in daily steps, adjusting for everything else

#Sleep Duration decreases significantly by 0.121 units for every unit
#Increase in systolic reading, adjusting for everything else

#Sleep Duration increases significantly by 0.13 units for every unit
#increase in diastolic reading, adjusting for everything else

#we check for multicollinearity using vif and tolerance

install.packages("car")
library(car)

vif(model1)
#if the vif shows greater than 10, it means that there is such a strong
#relationship between the variables, like that these may be collinear
#If collinear, this will bias the results of the model from our results
#we see that systolic and diastolic might be collinear

#tolerance the inverse of vif; we run this as an extra check
1/vif(model1)
#With these results we look for the last column to be >0.10, if its less, than
#this means its collinear, the two variables are systolic and diastolic