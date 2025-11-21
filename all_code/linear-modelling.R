library(janitor)
library(dplyr)
library(ggplot2)
# Check skewness to guide decisions
library(moments)
library(car) # for vif analysis
library(caret) 
library(MASS) # for stepwise regression
library(purrr) # for map function

# Step 0: Load data, split to test and train
data <- read.csv("./data/credit.csv") %>% 
  janitor::clean_names()


# Step 0: do some cleaning
# a. remove irrelevant column 'X' because it is the index column.
data <- data[, -1]



# b. create category columns: One hot encoding:

data$gender <- trimws(data$gender)
data$male <- ifelse(data$gender == "Male", yes = 1, no = 0)
data$is_student <- ifelse(data$student == "Yes", yes = 1, no = 0)
data$is_married <- ifelse(data$married == "Yes", yes = 1, no = 0)
data$african_american <- ifelse(data$ethnicity == "African American", yes = 1, no = 0)
data$asian <- ifelse(data$ethnicity == 'Asian', yes = 1, no = 0)

str(data)

# c. select necessary columns after encoding
data <- data %>% 
  dplyr::select(-c("gender", "student", "married", "ethnicity"))



# Step 1 : EDA

## a. Data Structure and Distribution Checks
head(data)
tail(data)
str(data)
colnames(data)
summary(data)

# balance has minimum of 0 (We expect truncated normal distribution and address this under possible transformation.)


# let's check pair plot for visual idea of correlation
pairs(data)

# Notes for distribution checks, we use Histogram with density curves to see if our variables follow a normal distribution based on the normality assumptions in Multiple Linear Regression. Moreover, we use QQ-plot for normality check and skewness.

# let's check distribution of each plot
# Histograms with density curves: note that red is the actual curve and blue curve is the theoretical normal distribution curve
par(mfrow=c(2,3))
for(var in c("income", "limit", "rating", "age", "balance")) {
  hist(data[[var]], main=var, xlab=var, prob=TRUE)
  lines(density(data[[var]]), col="red", lwd=2)
  curve(dnorm(x, mean=mean(data[[var]]), sd=sd(data[[var]])), 
        add=TRUE, col="blue", lty=2)
}

# Plotted curves look truncated. Perhaps they may benefit from transformation. Here are other reasons for truncated normal curves in this dataset:
# Income: Can't be negative, often clustered above minimum wage
# Credit Limit: Always positive, often has minimum thresholds
# Balance: Can be 0 but not negative (unless it's debt)
# Rating: Often has minimum scores

# Observation from Histogram (ACTUAL) red curve: 
# 1. Income: Right-skewed with long tail (red curve peaks left, extends right)
# 2. Limit: Right-skewed with long tail  
# 3. Rating: Right-skewed with long tail
# 4. Age: Seems normal (red curve matches blue reasonably well). Though it has two peaks (take notice).
# 5. Balance: Right-skewed with long tail 

# Q-Q plots for normality check
par(mfrow=c(2,3))
for(var in c("income", "limit", "rating", "age", "balance")) {
  qqnorm(data[[var]], main=paste("Q-Q Plot for", var))
  qqline(data[[var]], col="red")
}


# Moreover, let's check our skew values to be on the safer side:

?skewness
skew_values <- sapply(data[, c("income", "limit", "rating", "balance", "age")], skewness)

# Noted: Rule of thumb:
# |skewness| < 0.5 : approximately symmetric (probably OK as-is)
# 0.5 < |skewness| < 1 : moderate skew (consider transformation)
# |skewness| > 1 : substantial skew (definitely transform)


# Step 2: Variable Transformation
data <- data %>% 
  mutate(log_income = log(income),
         log_limit = log(limit),
         log_rating = log(rating),
         log_balance = log(balance + 1))

data$sqrt_balance <- sqrt(data$balance)
skewness(data$sqrt_balance)

# Let's check for skewness again
skew_values <- sapply(data[, c("log_income", "log_limit", "log_rating", "sqrt_balance", "age")], skewness)


# Observation:
# 1. Other variables look ok,
# 2. Moderate skewness for log_limit: log-limit is now left-skewed
# 2. Huge skewness for log_balance 


par(mfrow=c(2,3))
for(var in c("log_income", "log_limit", "log_rating", "age", "sqrt_balance")) {
  hist(data[[var]], main=var, xlab=var, prob=TRUE)
  lines(density(data[[var]]), col="red", lwd=2)
  curve(dnorm(x, mean=mean(data[[var]]), sd=sd(data[[var]])), 
        add=TRUE, col="blue", lty=2)
}

# new columns:
model_data <- data 

# model_data <- data %>% 
  #select(-c("income", "limit", "rating"))


# Step 3: Initial Model Fitting: Fit our models
colnames(model_data)

# kind of have 3 models now:
# Model 1: Original balance
model_original <- lm(balance ~ log_income + log_limit + log_rating + 
                       cards + age + education + male + is_student + 
                       is_married + african_american + asian, data = data)

# Model 2: Square root balance
model_sqrt <- lm(sqrt_balance ~ log_income + log_limit + log_rating + 
                   cards + age + education + male + is_student + 
                   is_married + african_american + asian, data = data)

# Model 3: Log balance  
model_log <- lm(log_balance ~ log_income + log_limit + log_rating + 
                  cards + age + education + male + is_student + 
                  is_married + african_american + asian, data = data)


# summary of each model:
summary(model_original)
summary(model_log)
summary(model_sqrt)

# I will continue with model square root due to the Homoscedasticity assumption that the variance of the response variable's error is constant


# Step 4: Multicollinearity Check using VIF
# variance Inflation Factor (VIF)
vif(model_original)
vif(model_log)
vif(model_sqrt)

# Observation:
# it seems credit limit and credit rating are linearly dependent
# vif: credit limit (76.031766), credit rating (79.324432)
# some correlation there.

# Question 1: Is model better without credit limit?
model_original_noLimit <- lm(balance ~ log_income + log_rating + 
                       cards + age + education + male + is_student + 
                       is_married + african_american + asian, data = data)

# Model 2: Square root balance
model_sqrt_noLimit <- lm(sqrt_balance ~ log_income + log_rating + 
                   cards + age + education + male + is_student + 
                   is_married + african_american + asian, data = data)

# Model 3: Log balance  
model_log_noLimit <- lm(log_balance ~ log_income + log_rating + 
                  cards + age + education + male + is_student + 
                  is_married + african_american + asian, data = data)

# summary of each model:
summary(model_original)
summary(model_log)
summary(model_sqrt)



# Question 1: Is model better without credit rating?
model_original_noRating <- lm(balance ~ log_income + log_limit + 
                       cards + age + education + male + is_student + 
                       is_married + african_american + asian, data = data)

# Model 2: Square root balance
model_sqrt_noRating <- lm(sqrt_balance ~ log_income + log_limit + 
                   cards + age + education + male + is_student + 
                   is_married + african_american + asian, data = data)

# Model 3: Log balance  
model_log_noRating <- lm(log_balance ~ log_income + log_limit + 
                  cards + age + education + male + is_student + 
                  is_married + african_american + asian, data = data)

# summary of each model:
summary(model_original)
summary(model_log)
summary(model_sqrt)

summary(model_original_noLimit)
summary(model_log_noLimit)
summary(model_sqrt_noLimit)

summary(model_original_noRating)
summary(model_log_noRating)
summary(model_sqrt_noRating)


# Conclusion: Model seems better overall without credit limit. Moreover, credit limit is dependent on credit rating i.e Lenders use credit rating to determine credit limit. Note:
# a: Higher-rated borrowers are less risky, so they get higher limits
# b: Lower-rated borrowers are more risky, so they get lower limits to limit exposure

model_original <- model_original_noLimit
model_log <- model_log_noLimit
model_sqrt <- model_sqrt_noLimit

summary(model_original)
summary(model_log)
summary(model_sqrt)


# Check to see if our vif looks good: Multicollinearity
vif(model_original)
vif(model_log)
vif(model_sqrt)







# Step 6: Residual Diagnostics : Outliers done later



# Load required package
library(MASS)

# Check if sqrt_balance has non-positive values
summary(model_data$sqrt_balance)
sum(model_data$sqrt_balance <= 0)
sum(model_data$sqrt_balance <= 0)

# Check if sqrt_balance has non-positive values
min_value <- min(model_data$sqrt_balance)
if (min_value <= 0) {
  model_data$shifted_sqrt <- model_data$sqrt_balance - min_value + 0.001
  m_shifted <- lm(shifted_sqrt ~ log_income + is_student + log_rating, data = model_data)
  bc <- boxcox(m_shifted)
}

# Get optimal lambda
lambda <- bc$x[which.max(bc$y)]
cat("Optimal lambda:", lambda, "\n")

# Apply the Box-Cox transformation to the ORIGINAL balance variable
if (abs(lambda) < 0.001) {
  # If lambda ≈ 0, use log transformation
  model_data$bc_balance <- log(model_data$balance)
} else {
  # Standard Box-Cox transformation
  model_data$bc_balance <- (model_data$balance^lambda - 1) / lambda
}

# Fit the new model with Box-Cox transformed balance - FIXED THIS LINE
m_bc <- lm(bc_balance ~ log_income + as.factor(is_student) + log_rating, data = model_data)  # ← Use bc_balance!
summary(m_bc)

head(model_data)

# Plot of Residuals vs Fitted Values
plot(m_bc$fitted.values, m_bc$residuals, xlab="Fitted Values", ylab="Residuals")

#QQ-plot of residuals.
qqnorm(m_bc$residuals)
qqline(m_bc$residuals,col= "blue",lwd = 2)


# Plot of residuals vs log_income
plot(model_data$log_income, m_bc$residuals, xlab="log_income", ylab="Residuals")


# Plot of residuals vs is_student
plot(model_data$is_student, m_bc$residuals, xlab="is_student", ylab="Residuals")


colnames(model_data)
model_data <- model_data %>% mutate(income_sqrt = income^(1/2))

model_original <- lm(balance ~ cards + age + education + male + income_sqrt + african_american + is_married + asian + is_student + rating, data = model_data) 
summary(model_original)
par(mfrow = c(2,2))
plot(model_original)


# Conclusion for Residaul Diagnostics:
# - The Models from the Initial Model Selection violated Normality Assumption
# - I did a boxcox transformation which didn't  change much
# - However, a sqrt transformation on income made the Residual vs Fitted Values plot more agreeable and satisfactory.
# - Hence I moved forward with :
# `model_original <- lm(balance ~ cards + age + education + male + income_sqrt + african_american + is_married + asian + is_student + rating, data = model_data) ` 
# - I performed more model selection on model_original vs sqrt_balance model using income_sqrt variable instead of income. 

# Refined Model Selection - Part 1

# Step 5: Model Selection
# Model Selection Comparison for All Response Variables
library(MASS)

# Function to run stepwise selection for a given response
run_stepwise_selection <- function(response_var, data) {
  # Create formula strings
  full_formula <- as.formula(paste(response_var, " ~ cards + age + education + male + income_sqrt + african_american + is_married + asian + is_student + rating"))
  empty_formula <- as.formula(paste(response_var, "~ 1"))
  
  # Fit models
  full_model <- lm(full_formula, data = data)
  empty_model <- lm(empty_formula, data = data)
  
  # Stepwise selection
  forward_aic <- stepAIC(empty_model, 
                         scope = list(upper = full_model, lower = ~1),
                         direction = "forward", trace = 0)
  
  forward_bic <- stepAIC(empty_model,
                         scope = list(upper = full_model, lower = ~1), 
                         direction = "forward", k = log(nrow(data)), trace = 0)
  
  backward_aic <- stepAIC(full_model, direction = "backward", trace = 0)
  backward_bic <- stepAIC(full_model, direction = "backward", 
                          k = log(nrow(data)), trace = 0)
  
  stepwise_aic <- stepAIC(empty_model,
                          scope = list(upper = full_model, lower = ~1),
                          direction = "both", trace = 0)
  
  stepwise_bic <- stepAIC(empty_model,
                          scope = list(upper = full_model, lower = ~1),
                          direction = "both", k = log(nrow(data)), trace = 0)
  
  # Return all models
  return(list(
    forward_aic = forward_aic,
    forward_bic = forward_bic,
    backward_aic = backward_aic,
    backward_bic = backward_bic,
    stepwise_aic = stepwise_aic,
    stepwise_bic = stepwise_bic
  ))
}

# Function to extract model metrics
get_model_metrics <- function(model, model_name, response_name) {
  model_summary <- summary(model)
  
  data.frame(
    Response = response_name,
    Method = model_name,
    AIC = round(AIC(model), 2),
    BIC = round(BIC(model), 2),
    R_squared = round(model_summary$r.squared, 4),
    Adj_R_squared = round(model_summary$adj.r.squared, 4),
    Num_Predictors = length(coef(model)) - 1,
    Predictors = paste(names(coef(model))[-1], collapse = ", "),
    stringsAsFactors = FALSE
  )
}

# Run stepwise selection for all three response variables
balance_models <- run_stepwise_selection("balance", model_data)
log_balance_models <- run_stepwise_selection("log_balance", model_data)
sqrt_balance_models <- run_stepwise_selection("sqrt_balance", model_data)

# Create comparison table
comparison_table <- rbind(
  # Balance models
  get_model_metrics(balance_models$forward_aic, "Forward AIC", "balance"),
  get_model_metrics(balance_models$forward_bic, "Forward BIC", "balance"),
  get_model_metrics(balance_models$backward_aic, "Backward AIC", "balance"),
  get_model_metrics(balance_models$backward_bic, "Backward BIC", "balance"),
  get_model_metrics(balance_models$stepwise_aic, "Stepwise AIC", "balance"),
  get_model_metrics(balance_models$stepwise_bic, "Stepwise BIC", "balance"),
  
  # Log balance models
  get_model_metrics(log_balance_models$forward_aic, "Forward AIC", "log_balance"),
  get_model_metrics(log_balance_models$forward_bic, "Forward BIC", "log_balance"),
  get_model_metrics(log_balance_models$backward_aic, "Backward AIC", "log_balance"),
  get_model_metrics(log_balance_models$backward_bic, "Backward BIC", "log_balance"),
  get_model_metrics(log_balance_models$stepwise_aic, "Stepwise AIC", "log_balance"),
  get_model_metrics(log_balance_models$stepwise_bic, "Stepwise BIC", "log_balance"),
  
  # Sqrt balance models
  get_model_metrics(sqrt_balance_models$forward_aic, "Forward AIC", "sqrt_balance"),
  get_model_metrics(sqrt_balance_models$forward_bic, "Forward BIC", "sqrt_balance"),
  get_model_metrics(sqrt_balance_models$backward_aic, "Backward AIC", "sqrt_balance"),
  get_model_metrics(sqrt_balance_models$backward_bic, "Backward BIC", "sqrt_balance"),
  get_model_metrics(sqrt_balance_models$stepwise_aic, "Stepwise AIC", "sqrt_balance"),
  get_model_metrics(sqrt_balance_models$stepwise_bic, "Stepwise BIC", "sqrt_balance")
)

# Print the comprehensive comparison table
print(comparison_table)

# Optional: Sort by Response and AIC for better readability
library(dplyr)
comparison_sorted <- comparison_table %>%
  arrange(Response, AIC)

print(comparison_sorted)

# Create a summary table showing best model for each response
best_models_summary <- comparison_table %>%
  group_by(Response) %>%
  slice(which.min(AIC)) %>%
  dplyr::select(Response, Method, AIC, BIC, R_squared, Adj_R_squared, Num_Predictors, Predictors)

print("=== BEST MODELS BY RESPONSE ===")
print(best_models_summary)

# For a nice formatted table
library(knitr)
kable(comparison_sorted, caption = "Comprehensive Model Selection Results")



# Model Selection Conclusion: We choose balance model why?
# -- Variable transformation and Residual Diagnostics comparison showed balance model works is best
# -- Original Model after VIF and Transformation:
# ` model_original <- lm(balance ~ cards + age + education + male + income_sqrt + african_american + is_married + asian + is_student + rating, data = model_data) `
# -- At this time, the balance model and sqrt_balance are both great options, however, Residual Diagnostics in next step show that model might be first choice due to having a more agreeable Residual vs Fitted Values plot


# Step 6: Model Diagnostics after Refined Model Selection:
model_data <- model_data %>% 
  dplyr::select(balance, log_balance, sqrt_balance, cards, age, education, male,
                income_sqrt, african_american, is_married, asian, 
                is_student, rating)

# 2nd Choice: sqrt_balance model: 
model_best2 <- lm(sqrt(balance) ~ rating + income_sqrt + is_student, data = model_data)
par(mfrow = c(2,2))
plot(model_best2)


# 1st choice: balance model
model_best <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, data = model_data
)

par(mfrow = c(2,2))
plot(model_best)


par(mfrow=c(3,3))

# Plot of Residuals vs Fitted Values
plot(model_best$fitted.values, model_best$residuals, xlab="Fitted Values", ylab="Residuals")

m1 <- model_best
# Plot of residuals vs log_income
plot(model_data$income_sqrt, m1$residuals, xlab="income_sqrt", ylab="Residuals")

# Plot of residuals vs log_income
plot(model_data$age, m1$residuals, xlab="age", ylab="Residuals")

# Plot of residuals vs log_income
plot(model_data$is_married, m1$residuals, xlab="income_sqrt", ylab="Residuals")



# Plot of residuals vs log_rating
plot(model_data$rating, m1$residuals, xlab="rating", ylab="Residuals")

# Plot of residuals vs is_student
plot(model_data$is_student, m1$residuals, xlab="is_student", ylab="Residuals")


plot(model_data$rating, m1$residuals, xlab="rating", ylab="Residuals", 
     main = "Residuals vs log_rating")

#QQ-plot of residuals.
qqnorm(m1$residuals)
qqline(m1$residuals,col= "blue",lwd = 2)




# Residual Diagnosotics: Conclusion:
# -- balance model is more agreeable




# Step 7: Cross-Validation for Refined Model Selection: K-Fold Cross-Validation

set.seed(12345678)


# CV to choose the best one
K <- 10
N <- nrow(model_data)
validSetSplits <-sample((1:N)%%K + 1)
RMSE_best <- numeric(K)
RMSE_sqrt <- numeric(K)

# Prediction: Converted:
# Initialize vectors

for (k in 1:K) {
  validSet <- model_data[validSetSplits == k, ]
  trainSet <- model_data[validSetSplits != k, ]
  
  # Model 1: Model_best 1 
  # model_original
  model_best <- model_best <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, data = trainSet)
  
  pred1 <- predict(model_best, newdata = validSet)
  
  RMSE_best[k] <- sqrt(mean((validSet$balance- pred1)^2))
  
  
  # Model 2: Model Best 2: - CONVERT BACK to dollars
  model_sqrt <- lm(sqrt(balance) ~ rating + income_sqrt + is_student, data = trainSet)
  
  pred2_sqrt <- predict(model_sqrt, newdata = validSet)
  pred2_dollars <- pred2_sqrt^2  # Convert back to original scale
  RMSE_sqrt[k] <- sqrt(mean((validSet$balance - pred2_dollars)^2))
  
}

# Now compare properly
cat("Best RMSE 1:", mean(RMSE_best), "\n")
cat("Best RMSE 2:", mean(RMSE_sqrt), "\n") 

# Calculate improvement percentage
improvement <- ((mean(RMSE_sqrt) - mean(RMSE_best)) / mean(RMSE_sqrt)) * 100

# Final model selection based on CV performance
final_model <- lm(balance ~ rating + income_sqrt + is_student + age + is_married, 
                  data = model_data)

cat("=== FINAL MODEL SELECTED ===\n")
cat("Based on cross-validation, model_best has superior performance:\n")
cat("RMSE:", round(mean(RMSE_best), 2), "vs", round(mean(RMSE_sqrt), 2), 
    "(", round(improvement, 1), "% improvement)\n")
cat("Formula: balance ~ rating + sqrt(income) + is_student + age + is_married\n")

# Final diagnostics
cat("\n=== FINAL MODEL SUMMARY ===\n")
print(summary(final_model))

# Check if assumptions are reasonably met
cat("Residual standard error:", summary(final_model)$sigma, "\n")
cat("R-squared:", summary(final_model)$r.squared, "\n")

cat("\n=== PERFORMANCE IMPROVEMENT ===\n")
cat("model_best RMSE:", round(mean(RMSE_best), 2), "\n")
cat("sqrt_model RMSE:", round(mean(RMSE_sqrt), 2), "\n")
cat("Absolute improvement:", round(mean(RMSE_sqrt) - mean(RMSE_best), 2), "\n")
cat("Improvement percentage:", round(improvement, 1), "%\n")

# Conclusion: Refined Model Selection
# Due to having lower RMSE, the 1st choice Model is the best refined model to move forward with.


# Step 8: Outlier detection: 
# Question: Is the Model better with or without Outliers?
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



# Step 9: Compare final Models:
# List to store results
model_list <- list(
  list(name = "With Outliers, With is_married", formula = balance ~ rating + income_sqrt + is_student + age + is_married, data = model_data),
  list(name = "Without Outliers, With is_married", formula = balance ~ rating + income_sqrt + is_student + age + is_married, data = clean_data),
  list(name = "Without Outliers, Without is_married", formula = balance ~ rating + income_sqrt + is_student + age, data = clean_data),
  list(name = "With Outliers, Without is_married", formula = balance ~ rating + income_sqrt + is_student + age, data = model_data)
)

# Initialize dataframe
model_summary <- data.frame(
  Model = character(),
  Outliers_Removed = logical(),
  Includes_is_married = logical(),
  CV_RMSE = numeric(),
  Residual_SE = numeric(),
  Adjusted_R2 = numeric(),
  stringsAsFactors = FALSE
)

# Function for K-fold CV RMSE
cv_rmse <- function(formula, data, K=10, seed=123){
  set.seed(seed)
  N <- nrow(data)
  folds <- sample((1:N) %% K + 1)
  rmse_values <- numeric(K)
  
  for(k in 1:K){
    train <- data[folds != k, ]
    valid <- data[folds == k, ]
    model <- lm(formula, data = train)
    preds <- predict(model, newdata = valid)
    rmse_values[k] <- sqrt(mean((valid[[as.character(formula[[2]])]] - preds)^2))
  }
  
  mean(rmse_values)
}

# Loop through each model and compute metrics
for(m in model_list){
  fit <- lm(m$formula, data = m$data)
  cv <- cv_rmse(m$formula, m$data)
  
  model_summary <- rbind(model_summary, data.frame(
    Model = m$name,
    Outliers_Removed = ifelse(grepl("Without Outliers", m$name), TRUE, FALSE),
    Includes_is_married = ifelse(grepl("Without is_married", m$name), FALSE, TRUE),
    CV_RMSE = round(cv, 2),
    Residual_SE = round(summary(fit)$sigma, 2),
    Adjusted_R2 = round(summary(fit)$adj.r.squared, 4)
  ))
}

model_summary

# Conclusion: 
# 1. is_married column is nelgible. The Adjusted R^2 without is_married is about the same as with is_marrieed column in the model.
# 2. is_married column is not statistically significant in the with Outliers model at the 5% level. Hence there is no strong evidence of its effect on balance.
# 3. The best model is the one Without Outliers and Without is_married column due to having the lowest validation RMSE from Cross-Validation


# Step 10: Display and Explain Best Model:

# Calculate all metrics without hard-coding
best_final_model <- lm(balance ~ rating + income_sqrt + is_student + age, 
                       data = clean_data)

summary(best_final_model)
# Re-run CV to get the actual RMSE programmatically
set.seed(12345678)
K <- 10
N <- nrow(clean_data)
validSetSplits <- sample((1:N) %% K + 1)
final_rmse_values <- numeric(K)

for (k in 1:K) {
  validSet <- clean_data[validSetSplits == k, ]
  trainSet <- clean_data[validSetSplits != k, ]
  
  model_cv <- lm(balance ~ rating + income_sqrt + is_student + age, data = trainSet)
  pred <- predict(model_cv, newdata = validSet)
  final_rmse_values[k] <- sqrt(mean((validSet$balance - pred)^2))
}

final_cv_rmse <- mean(final_rmse_values)
final_cv_se <- sd(final_rmse_values) / sqrt(K)

cat("=== FINAL OPTIMAL MODEL CONFIRMED ===\n")
cat("Model: balance ~ rating + income_sqrt + is_student + age\n")
cat("Dataset:", nrow(clean_data), "observations (", nrow(model_data) - nrow(clean_data), "influential points removed)\n")
cat("Cross-Validation Performance:\n")
cat("  RMSE:", round(final_cv_rmse, 2), "±", round(final_cv_se, 2), "\n")
cat("  R²:", round(summary(best_final_model)$adj.r.squared, 4), "\n")
cat("  Residual SE:", round(summary(best_final_model)$sigma, 2), "\n")

cat("\nKey Insights:\n")
cat("• Credit rating is the strongest predictor (coefficient:", round(coef(best_final_model)["rating"], 2), ")\n")
cat("• Student status increases balances by $", round(coef(best_final_model)["is_student"], 2), "\n") 
cat("• Square-root income transformation works best\n")
cat("• Age has a small but significant effect (coefficient:", round(coef(best_final_model)["age"], 2), ")\n")

# Show coefficient significance
cat("\nStatistical Significance:\n")
coef_summary <- summary(best_final_model)$coefficients
for (predictor in rownames(coef_summary)[-1]) {  # Skip intercept
  p_value <- coef_summary[predictor, 4]
  significance <- ifelse(p_value < 0.001, "***", 
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*", "not significant")))
  cat("•", predictor, ":", significance, "(p =", round(p_value, 4), ")\n")
}

# Step 10: Prediction with Confidence Intervals
cat("=== PREDICTION WITH NEW OBSERVATIONS ===\n")

# Example prediction
new_observation <- data.frame(
  rating = 500,
  income_sqrt = sqrt(50),  # Convert income to sqrt scale
  is_student = 1,
  age = 25
)

# Step 11: Prediction using new observation on a 95% Prediction Interval
prediction <- predict(best_final_model, newdata = new_observation, 
                      interval = "prediction", level = 0.95)

cat("Prediction for new observation:\n")
cat("Expected balance: $", round(prediction[1], 2), "\n")
cat("95% Prediction interval: [$", round(prediction[2], 2), ", $", 
    round(prediction[3], 2), "]\n")

# Interpretation
cat("\nBusiness Interpretation:\n")
cat("A student with credit rating 500, income $50k, and age 25 would have:\n")
cat("• Predicted balance: $", round(prediction[1], 2), "\n")
cat("• 95% chance actual balance between: $", round(prediction[2], 2), 
    "and $", round(prediction[3], 2), "\n")


cat("=== TECHNICAL EXPLANATION ===\n")
cat("Model: balance = -175.05 + 3.91*rating - 108.34*√income + 420.30*student - 0.93*age\n\n")
cat("Interpretation:\n")
cat("1. RATING: Each 1-point increase in credit rating → $3.91 higher balance (p < 0.001)\n")
cat("2. INCOME: Square root transformation - higher income → lower balances (counterintuitive)\n")
cat("3. STUDENT: Students have $420 higher balances than non-students (p < 0.001)\n") 
cat("4. AGE: Each year older → $0.93 lower balance (p = 0.002)\n")
cat("5. INTERCEPT: Theoretical balance with all predictors at zero: -$175\n")


# Impact of Negative Intercept:
cat("=== COMPLETE INTERCEPT EXPLANATION ===\n")
cat("The negative intercept (-$175) is mathematically necessary because:\n\n")
cat("1. CREDIT RATING CONSTRAINT: Minimum rating is", min(clean_data$rating), "\n")
cat("   • No such thing as rating = 0 in reality\n")
cat("   • Intercept at rating=0 is pure mathematical extrapolation\n\n")
cat("2. INCOME CONSTRAINT: Minimum income is $", min(clean_data$income), "\n") 
cat("   • No customers with $0 income\n")
cat("   • income_sqrt = 0 means income = $0 (impossible)\n\n")
cat("3. AGE CONSTRAINT: Minimum age is", min(clean_data$age), "\n")
cat("   • No newborn credit card holders\n")
cat("   • Age = 0 is biologically impossible for this context\n")
