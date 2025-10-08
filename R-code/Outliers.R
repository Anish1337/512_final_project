# Data processing
df = read.csv("C:\\Users\\anish\\OneDrive\\Desktop\\data.csv")
y = df$Performance.Index
x1 = df$Hours.Studied
x2 = df$Previous.Scores
x3 = df$Extracurricular.Activities
x4 = df$Sleep.Hours
x5 = df$Sample.Question.Papers.Practiced

# Full model (with outliers)
full_model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = df)

# Cook's Distance and threshold
cooksD <- cooks.distance(full_model)
p = 6
n = nrow(df)
threshold <- qf(0.5,p,n-p)

#print(threshold)
# Print outliers
#outlier_indices <- which(cooksD > 0.8914)
#cat("Number of Cook's Distance outliers:", length(outlier_indices), "\n")
#cat("Outlier row indices:", outlier_indices, "\n")

# Graph of Cook's Distance
plot(cooksD, type = "h", main = "Cook's Distance", ylab = "Cook's D")
abline(h = threshold, col = "red", lty = 2)

# Print percentage of outliers
print(paste(length(outlier_indices) / nrow(df) * 100, "% of the data had outliers."))
