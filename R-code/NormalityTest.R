# Load necessary libraries
library(ggplot2)
library(car)
library(MASS)

# Load your dataset (replace 'student_data.csv' with actual dataset)
student_data <- read.csv("/Users/eshitavani/Library/CloudStorage/OneDrive-purdue.edu/SPRING 2025/STAT 512_GroupProject/Student_Performance.csv")

colnames(student_data)


# Fit the full regression model
full_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = student_data)

# Check normality using a histogram and QQ plot
par(mfrow = c(1,2))  # Set plotting area
hist(residuals(full_model), main="Histogram of Residuals", xlab="Residuals", col="lightblue", breaks=30)
qqPlot(residuals(full_model), main="QQ Plot of Residuals")


#Anderson-Darling test
library(nortest)
ad_test <- ad.test(residuals(full_model))
print(ad_test)


#RQ1

# Full model
model1_full <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = student_data)

# Reduced model
model1_reduced <- lm(Performance.Index ~ Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = student_data)

# Residuals
residuals1 <- residuals(model1_full)


# Take a random sample of 5000 residuals
set.seed(123)  # for reproducibility
sample_residuals1 <- sample(residuals1, 5000)

# Now perform Shapiro-Wilk
shapiro.test(sample_residuals1)

# QQ plot
qqnorm(residuals1)
qqline(residuals1)


# 2. Histogram
hist(residuals1, breaks = 50, main = "Histogram of Residuals", xlab = "Residuals")

# 3. Density plot
plot(density(residuals1), main = "Density Plot of Residuals")


#RQ2


