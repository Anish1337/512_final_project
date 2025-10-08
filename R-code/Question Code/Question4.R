library(readxl)
library(openxlsx)
install.packages("munsell")
install.packages("pillar")
install.packages("ggplot2")
install.packages("car")


library(car)
library(lmtest)
library(nortest)
library(ggplot2)
##Research Question 4

student_data <- read.csv("/Users/eshitavani/Downloads/NormalizedData.csv")
head(student_data, n=4)

##HYPOTHESIS

# Factor
student_data$Extracurricular.Activities <- as.factor(student_data$Extracurricular.Activities)

# Extra-curricular to exa_contrast
student_data$exa_contrast <- ifelse(student_data$Extracurricular.Activities == "Y", 1, 0)


# Fit the full regression model
full_model <- lm(PI ~ Hours.Studied + Previous.Scores + exa_contrast + Sleep.Hours + Sample.Question.Papers.Practiced, data = student_data)
summary(full_model)

# Reduced model - HStudy + exa_contrast + interaction
reduced_model <- lm(PI ~ HStudy * exa_contrast, data = student_data)

# Summary
summary(reduced_model)

# Compare models
anova(reduced_model, full_model)

## ASSUMPTIONS

#Linearity 

# Full model
plot(full_model, which = 1)

# Reduced model
plot(reduced_model, which = 1)

# Normality
residuals1 <- residuals(full_model)
residuals2 <- residuals(reduced_model)

set.seed(123)  # for reproducibility
sample_residuals1 <- sample(residuals1, 5000)
sample_residuals2 <- sample(residuals2, 5000)



# Full model
hist(residuals(full_model), breaks = 50, main = "Histogram of Residuals (Full Model)")
qqnorm(residuals(full_model))
qqline(residuals(full_model))


# Reduced model
hist(residuals(reduced_model), breaks = 50, main = "Histogram of Residuals (Reduced Model)")
qqnorm(residuals(reduced_model))
qqline(residuals(reduced_model))

# Anderson-Darling Test
ad.test(residuals(full_model))
ad.test(residuals(reduced_model))

# Now perform Shapiro-Wilk
shapiro.test(sample_residuals1)
shapiro.test(sample_residuals2)

##Homoscedasity

# Full model
bptest(full_model)

# Reduced model
bptest(reduced_model)

##Multicollinearity

# Full model
vif(full_model)

# Reduced model
vif(reduced_model)

##Independence of errors:
# Full model
dwtest(full_model)

# Reduced model
dwtest(reduced_model)

##Outlier
# Full model
influencePlot(full_model)

# Reduced model
influencePlot(reduced_model)

