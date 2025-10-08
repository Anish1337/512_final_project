# imports
library(lmtest) # BP
library(car) # VIF


# Data processing
df = read.csv("C:\\Users\\anish\\OneDrive\\Desktop\\NormalizedData.csv")
y = df$Performance.Index
x1 = df$Hours.Studied
x2 = df$Previous.Scores
x3 = df$Extracurricular.Activities
x4 = df$Sleep.Hours
x5 = df$Sample.Question.Papers.Practiced

# fit models model
full_model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = df)
reduced_model <- lm(Y ~ X1 + X2 + X5, data = df)

# partial f_test
f_test_result <- anova(reduced_model, full_model)
print(f_test_result)
alpha <- 0.05
p_value <- f_test_result$`Pr(>F)`[2] 

# analysis
if (is.na(p_value)) {
  print("Could not extract p-value. Please check the anova output.")
} else if (p_value < alpha) {
  cat(sprintf("\nP-value (%.4f) is less than alpha (%.2f).\n", p_value, alpha))
  cat("Reject Ho. At least one of X3 (Extracurriculars) or X4 (Sleep Hours) significantly affects Y (Performance Index).\n")
} else {
  cat(sprintf("\nP-value (%.4f) is greater than or equal to alpha (%.2f).\n", p_value, alpha))
  cat("Fail to reject Ho. There is insufficient evidence that X3 and X4 jointly affect Y significantly.\n")
}

# BP
bp_test_reduced <- bptest(reduced_model)
print(bp_test_reduced)

# QQ
# Extract the residuals (the differences between observed and predicted Y)
model_residuals <- residuals(reduced_model)
# Create the QQ plot comparing residual quantiles to theoretical normal quantiles
qqnorm(model_residuals, main = "QQ Plot for RQ1")
# Add the theoretical reference line to the plot
qqline(model_residuals, col = "steelblue", lwd = 2) 

# VIF
vif_reduced <- vif(reduced_model)
print(vif_reduced)

# Variable Plots
avPlots(reduced_model, main = "Added-Variable Plots for Reduced Model")

# Influence points
# Cook's Distance and threshold
cooksD <- cooks.distance(reduced_model)
p = 4
n = nrow(df)
threshold <- qf(0.5,p,n-p)
outlier_indices <- which(cooksD > threshold)
# Print percentage of outliers
print(paste(length(outlier_indices) / nrow(df) * 100, "% of the data had outliers."))

# k fold cross validation
library(caret)
library(ggplot2)

set.seed(123)

train.control<-trainControl(method='cv', number=10)

step.model<-train(PI~HStudy+PrevScore+NumPaperPrac,data=df,method="leapBackward", tuneGrid=data.frame(nvmax=4),trControl=train.control)