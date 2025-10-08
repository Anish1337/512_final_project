Final<-read.csv("/Users/eshitavani/Downloads/Student_Performance.csv")

#Normalize variables
Final$HStudy<-scale(Final$Hours.Studied)
Final$PrevScore<-scale(Final$Previous.Scores)
Final$HSleep<-scale(Final$Sleep.Hours)
Final$NumPaperPrac<-scale(Final$Sample.Question.Papers.Practiced)
Final$PI<-scale(Final$Performance.Index)

#Convert categorical to dummy variables
Final$Extracurricular.Activities <- as.factor(ifelse(Final$Extracurricular.Activities=="Yes", "Y", "N")) 
Final$exa_contrast <- ifelse(Final$Extracurricular.Activities=='Y', 1, 0) 
write.csv(Final,"/Users/eshitavani/Downloads/NormalizedData.csv", row.names = FALSE)

#Create full model
full <-lm(data=Final, PI~HStudy+PrevScore+exa_contrast+HSleep+NumPaperPrac)
summary(full)

#ANOVA for full
anova(full)

#Added Variable plots for full
library(car)
class(full)  # should be "lm"

avPlots(full)

#Outliers for full
influencePlot(full)
cooks.distance(full)
#No x outliers but some Y outliers (outside of -2,2 range) 
#No Cook's D above 0.5 so no influence on model

#Multicolinearity for full
vif(full)
#No VIF greater than 10 so no multicolinearity issues

#Qq plot
qqnorm(resid(full))

#K-fold cross validation
library(MASS)
library(leaps)
install.packages("caret")
library(caret)
set.seed(123)
train.control <-trainControl(method="cv",number=10)
full_model<-train(PI~HStudy+PrevScore+exa_contrast+HSleep+NumPaperPrac, data=Final, method="leapBackward", 
              tuneGrid=data.frame(nvmax=5),
              trControl=train.control)
full_model$results

# Reduced model - HStudy + exa_contrast + interaction
reduced_model <- lm(PI ~ HStudy * exa_contrast, data = Final)

library(car)
avPlots(reduced_model)

install.packages("farver")


#Outliers for reduced
influencePlot(reduced_model)
#No x outliers but some Y outliers (outside of -2,2 range) 
#No Cook's D above 0.5 so no influence on model

#Multicolinearity for reduced
vif(reduced_model)
vif(reduced_model, type = "predictor")
#No VIF greater than 10 so no multicolinearity issues

#Qq plot
qqnorm(residuals(reduced_model))
qqline(residuals(reduced_model))

#BP test
library(lmtest)
bptest(reduced_model)

#K-fold cross validation for reduced
library(MASS)
library(leaps)
library(caret)
set.seed(123)
train.control <-trainControl(method="cv",number=10)
reduced_model <- train(PI ~ HStudy * exa_contrast, 
                data = Final, 
                method = "leapBackward", 
                tuneGrid = data.frame(nvmax = 3),  # only 3 terms: HStudy, exa_contrast, and their interaction
                trControl = train.control)

reduced_model$results

#Compare models
anova(reduced_model, full)
