
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
# We do Residual Diagnostics
# Model Selected: sqrt_balance model using BIC metric: 




