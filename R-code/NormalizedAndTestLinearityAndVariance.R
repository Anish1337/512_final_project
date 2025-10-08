library(readxl)
df<-read_excel("/Users/shreya/Desktop/stat/Student_Performance.xlsx")

#normalized columns (did not do extracurricular because categorical but did change it to a factor variable)
df$HStudy<-scale(df$`Hours Studied`)
df$PrevScore<-scale(df$`Previous Scores`)
df$HSleep<-scale(df$`Sleep Hours`)
df$NumPaperPrac<-scale(df$`Sample Question Papers Practiced`)
df$PI<-scale(df$`Performance Index`)
#made it so that the variable is 1 for yes and 0 for no but also if you want to keep categorical you can because that is untouched aside from being Y and N instead of Yes and No
df$`Extracurricular Activities` <- as.factor(ifelse(df$`Extracurricular Activities`=="Yes", "Y", "N")) 
df$exa_contrast <- ifelse(df$`Extracurricular Activities`=='Y', 1, 0) 
write.csv(df,"/Users/shreya/Desktop/stat/NormalizedData.csv", row.names = FALSE)

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
#full model
full<-lm(PI~HStudy+PrevScore+HSleep+NumPaperPrac+exa_contrast, data=df)
summary(full)

library(lmtest)
#linearity
avPlots(full)
#variance
bptest(full)
df$groupStudy<-cut(fitted(full), breaks=2)
bftest(full,df$groupStudy)