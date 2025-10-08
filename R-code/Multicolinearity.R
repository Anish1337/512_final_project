#Load data
library("openxlsx")
setwd("C:/Users/ISABE/OneDrive/Desktop/Stats/")
Final<-read.xlsx("Student_Performance.xlsx", sheet="Student_Performance", check.names=TRUE)
levels(Final$Extracurricular.Activities) <-c(1,0)
plot(Final)
full <-lm(data=Final, Performance.Index~Hours.Studied+Previous.Scores+Extracurricular.Activities+Sleep.Hours+Sample.Question.Papers.Practiced)
summary(full)
library(GGally)
X<-Final[,1:5]
ggpairs(X)
library(car)
vif(full)
