# Outlier detection


# Methods to detect outliers:
# 1. Stuentized Residuals (for outliers in response variable)
# 2. High leverage (for outliers in predictor variable)
# 3. High Influential Observations (extreme values in both response and predictors)

# PART 2
model_best <- final_model
# Standardized residuals (z-scores of residuals)
model_data$std_resid <- rstandard(model_best)

# Cook's distance (influence)
model_data$cooks <- cooks.distance(model_best)

# Leverage
model_data$leverage <- hatvalues(model_best)

# DFBetas (how each point affects coefficients)
dfb <- dfbetas(model_best)

n <- nrow(model_data)
p <- length(coef(model_best)) - 1

outliers <- which(abs(model_data$std_resid) > 3)
high_cooks <- which(model_data$cooks > (4 / n))
high_leverage <- which(model_data$leverage > (2 * (p + 1) / n))

cat("Outliers (|standardized residual| > 3):", outliers, "\n")
cat("High Cook's Distance (> 4/n):", high_cooks, "\n")
cat("High Leverage (> 2*(p+1)/n):", high_leverage, "\n")

# Since Cook's distance is used to measure Influential Observations, let's clean data using cook's distance

clean_data <- model_data[-unique(c(high_cooks)), ]

model_best_clean <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, data = clean_data)

summary(model_best_clean)
nrow(clean_data) 

# Residual Analysis: We removed 25 observations. Now we have 375 Observations.

# Question: Is final_model better with or without ?
# Let's do Cross-validation:

# Prediction: Converted:
# Initialize vectors

# CV to choose the best one

set.seed(12345678)

K <- 10
N <- nrow(clean_data)
validSetSplits <-sample((1:N)%%K + 1)
clean_RMSE_best <- numeric(K)

for (k in 1:K) {
  validSet <- clean_data[validSetSplits == k, ]
  trainSet <- clean_data[validSetSplits != k, ]
  
  # Model 1: Model_best 1 
  # model_original
  model_best <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, data = trainSet)
  
  pred1 <- predict(model_best, newdata = validSet)
  
  clean_RMSE_best[k] <- sqrt(mean((validSet$balance- pred1)^2))
  
  
  
}

# Now compare properly
cat("Best RMSE 1:", mean(clean_RMSE_best), "\n")


# Calculate improvement percentage
improvement <- ((mean(RMSE_best) - mean(clean_RMSE_best)) / mean(RMSE_best)) * 100


# Final model selection based on CV performance
final_model_without_Outliers <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, 
                  data = clean_data)
summary(final_model_without_Outliers)

final_model_with_Outliers <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, 
                                  data = model_data)
summary(final_model_with_Outliers)

cat("=== FINAL MODEL SELECTED ===\n")
cat("Based on cross-validation, model_best has superior performance:\n")
cat("RMSE:", round(mean(clean_RMSE_best), 2), "vs", round(mean(RMSE_best), 2), 
    "(", round(improvement, 1), "% improvement)\n")
cat("Formula: balance ~ rating + sqrt(income) + is_student + age + is_married\n")

# Final diagnostics
cat("\n=== FINAL MODEL SUMMARY ===\n")
print(summary(final_model))

# Check if assumptions are reasonably met
cat("Residual standard error:", summary(final_model_without_Outliers)$sigma, "\n")
cat("R-squared:", summary(final_model_without_Outliers)$r.squared, "\n")

cat("Residual standard error:", summary(final_model_with_Outliers)$sigma, "\n")
cat("R-squared:", summary(final_model_with_Outliers)$r.squared, "\n")

cat("\n=== PERFORMANCE IMPROVEMENT ===\n")
cat("Final Model without Outliers:", round(mean(clean_RMSE_best), 2), "\n")
cat("Final Model with Outliers:", round(mean(RMSE_best), 2), "\n")
cat("Absolute improvement:", abs(round(mean(clean_RMSE_best) - mean(RMSE_best), 2)), "\n")
cat("Improvement percentage:", round(improvement, 1), "%\n")


# without is_married variable, is model better??

set.seed(12345678)

K <- 10
N <- nrow(clean_data)
validSetSplits <-sample((1:N)%%K + 1)
clean_RMSE_best <- numeric(K)

for (k in 1:K) {
  validSet <- clean_data[validSetSplits == k, ]
  trainSet <- clean_data[validSetSplits != k, ]
  
  # Model 1: Model_best 1 
  # model_original
  model_best <- lm(balance ~ rating + income_sqrt + is_student + age, data = trainSet)
  
  pred1 <- predict(model_best, newdata = validSet)
  
  clean_RMSE_best[k] <- sqrt(mean((validSet$balance- pred1)^2))
  
  
  
}

# Now compare properly
cat("Best RMSE 1:", mean(clean_RMSE_best), "\n")


# Calculate improvement percentage
improvement <- ((mean(RMSE_best) - mean(clean_RMSE_best)) / mean(RMSE_best)) * 100


# Final model selection based on CV performance
final_model_without_Outliers <- lm(balance ~ rating + income_sqrt + is_student + age, 
                                   data = clean_data)
summary(final_model_without_Outliers)

final_model_with_Outliers <- lm(balance ~ rating + income_sqrt + is_student + age, 
                                data = model_data)
summary(final_model_with_Outliers)

cat("=== FINAL MODEL SELECTED ===\n")
cat("Based on cross-validation, model_best has superior performance:\n")
cat("RMSE:", round(mean(clean_RMSE_best), 2), "vs", round(mean(RMSE_best), 2), 
    "(", round(improvement, 1), "% improvement)\n")
cat("Formula: balance ~ rating + sqrt(income) + is_student + age + is_married\n")

# Final diagnostics
cat("\n=== FINAL MODEL SUMMARY ===\n")
print(summary(final_model))

# Check if assumptions are reasonably met
cat("Residual standard error:", summary(final_model_without_Outliers)$sigma, "\n")
cat("R-squared:", summary(final_model_without_Outliers)$r.squared, "\n")

cat("Residual standard error:", summary(final_model_with_Outliers)$sigma, "\n")
cat("R-squared:", summary(final_model_with_Outliers)$r.squared, "\n")

cat("\n=== PERFORMANCE IMPROVEMENT ===\n")
cat("Final Model without Outliers:", round(mean(clean_RMSE_best), 2), "\n")
cat("Final Model with Outliers:", round(mean(RMSE_best), 2), "\n")
cat("Absolute improvement:", abs(round(mean(clean_RMSE_best) - mean(RMSE_best), 2)), "\n")
cat("Improvement percentage:", round(improvement, 1), "%\n")



