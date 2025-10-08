# Final Project R Code - Student Performance

# Load data
library("openxlsx")
setwd("C:/Users/ISABE/OneDrive/Desktop/Stats/")
Final <- read.csv("/Users/eshitavani/Downloads/Student_Performance.csv")

# Normalize data
Final$HStudy <- scale(Final$Hours.Studied)
Final$PrevScore <- scale(Final$Previous.Scores)
Final$HSleep <- scale(Final$Sleep.Hours)
Final$NumPaperPrac <- scale(Final$Sample.Question.Papers.Practiced)
Final$PI <- scale(Final$Performance.Index)

# Convert categorical variable
Final$exa_contrast <- ifelse(Final$Extracurricular.Activities == 'Yes', 1, 0)

# Save normalized data
write.csv(Final, "/Users/eshitavani/Downloads/NormalizedData.csv", row.names = FALSE)


#Research question 4:
#Ho:B Hours studied*extracurricular activities /= 0
#Ha:B Hours studied*extracurricular activities = 0
full <- lm(PI ~ HStudy*exa_contrast + HSleep + PrevScore + NumPaperPrac,
           data = Final)
summary(full)

reduced_model4 <-lm(data=Final, PI~HStudy+PrevScore+exa_contrast+HSleep+NumPaperPrac)

#Assess assumptions for reduced model for question 1:
#Added variable plots to assess linearity
library(car)
avPlots(full)
avPlots(reduced_model4)
#All plots look linear so no problems with non linear relationships
#Influence plot and cooks distance to check for influential points
influencePlot(reduced_model4)
influencePlot(full)
cooks.distance(reduced_model4)
#Compare cooks distance to F(7, 9993) for 50%
qf(0.50, 7, 9993)
#No cooks distance above threshold of F(7,9993), so no major influential points
#Check for multicollinearity using VIF
vif(reduced_model4)
vif(full)
#No VIF greater than 10 so no multicollinearity issues
#Qq plot to check for normality
qqnorm(resid(reduced_model4))
qqnorm(resid(full))

install.packages("lmtest")
library(lmtest)
bptest(full)
bptest(reduced_model4)
#QQ plot appears close to normal
#K-fold cross validation
library(MASS)
library(leaps)
library(caret)
set.seed(123)
train.control <-trainControl(method="cv",number=10)
reduced4.k<-train(PI~HStudy*exa_contrast+PrevScore+HSleep+NumPaperPrac, data=Final,
                  method="leapBackward",
                  tuneGrid=data.frame(nvmax=6),
                  trControl=train.control)
reduced4.k$results
#Compare full and reduced models
anova(reduced_model4, full)
