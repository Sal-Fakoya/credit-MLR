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
