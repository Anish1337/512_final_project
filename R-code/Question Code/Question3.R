df<-read.csv("/Users/shreya/Desktop/stat/NormalizedData.csv")
df$Extracurricular.Activities <- as.factor(ifelse(df$Extracurricular.Activities=="Y", "Y", "N"))
df$exa_contrast <- ifelse(df$Extracurricular.Activities=='Y', 1, 0) 

#research question
#Ho (reduced model) : β1 = β4  The number of hours of study versus sleep do not impact the performance index as they are the same. 
#Y = β0+βnew(X1+X4)+β2X2+β3X3+β5X5+𝜀 
#Ha (full model) : β1 ≠ β4 The number of hours of sleep and study are not the same and one did have an impact on the performance index. 
#Y = β0+β1X1+β2X2+β3X3+β4X4+β5X5+𝜀

full<-lm(PI~HStudy+PrevScore+exa_contrast+HSleep+NumPaperPrac,data=df)
summary(full)
df$combined<-df$HStudy+df$HSleep
df$combined<-scale(df$combined)
reduced<-lm(PI~combined+PrevScore+exa_contrast+NumPaperPrac,data=df)
summary(reduced)
anova(reduced,full)
#p-val is low so reject Ho so B1 and B4 are significant


#import functions and libraries
bftest<-function(fit,group,alpha=.05){
  
  f<-fit$fitted
  e<-fit$res
  e1<-e[group==unique(group)[1]]
  e2<-e[group==unique(group)[2]]
  d1<-abs(e1-median(e1))
  d2=abs(e2-median(e2))
  n1<-length(e1)
  n2<-length(e2)
  n<-length(group)################# deghat, barasi, neveshtan stop
  
  s=sqrt( (  (n1-1)*var(d1)+   (n2-1)*var(d2)   )/(n-2))
  t=(mean(d1)-mean(d2))/(s*sqrt((1/n1)+(1/n2))   )
  
  out<-cbind(t.value=abs(t),P.Value=2*(1-pt(abs(t),n-2)),alpha=alpha,df=(n-2))###????
  return(out)
}

library(lmtest)
library(car)
library(MASS)
library(leaps)
library(caret)

#data testing for reduced model
#outliers
influencePlot(reduced)
#multicolinearity
vif(reduced)
#K-fold Cross-validation
set.seed(123)
train.control<-trainControl(method='cv', number=10)
step.model<-train(PI~combined+PrevScore+exa_contrast+NumPaperPrac,data=df,method="leapBackward", tuneGrid=data.frame(nvmax=4),trControl=train.control)
step.model$results
#linearity
avPlost(reduced)
plot(fitted(reduced), resid(reduced),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)
#variance
bptest(reduced)
df$groupStudy<-cut(fitted(reduced), breaks=2)
bftest(reduced,df$groupStudy)
#normality
hist(residuals(reduced), main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(reduced))
qqline(residuals(reduced), col = "red")
library(nortest)
ad_test <- ad.test(residuals(reduced))
print(ad_test)



#data testing for full model
#outliers
influencePlot(full)
#multicolinearity
vif(full)
#K-fold Cross-validation
set.seed(123)
train.control<-trainControl(method='cv', number=10)
step.model<-train(PI~HStudy+HSleep+PrevScore+exa_contrast+NumPaperPrac,data=df,method="leapBackward", tuneGrid=data.frame(nvmax=5),trControl=train.control)
step.model$results
#linearity
avPlots(full)
plot(fitted(reduced), resid(reduced),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)
#variance
bptest(full)
df$groupStudy<-cut(fitted(full), breaks=2)
bftest(full,df$groupStudy)
#normality
hist(residuals(full), main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(full))
qqline(residuals(full), col = "red")
library(nortest)
ad_test <- ad.test(residuals(full))
print(ad_test)

