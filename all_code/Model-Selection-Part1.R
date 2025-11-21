# Model Selection (Rough Work)


# Model Selection:
library(MASS)

full_model <- lm(sqrt_balance ~ log_income + log_rating + 
                   cards + age + education + male + is_student + 
                   is_married + african_american + asian, data = model_data)

empty_model <- lm(sqrt_balance ~ 1, data = model_data)

# Forward selection
forward_aic <- stepAIC(empty_model, 
                       scope = list(upper = full_model, lower = ~1),
                       direction = "forward", trace = 0)

forward_bic <- stepAIC(empty_model,
                       scope = list(upper = full_model, lower = ~1), 
                       direction = "forward", k = log(nrow(model_data)), trace = 0)
summary(forward_aic)
summary(forward_bic)

# Backward elimination
backward_aic <- stepAIC(full_model, direction = "backward", trace = 0)
backward_bic <- stepAIC(full_model, direction = "backward", 
                        k = log(nrow(model_data)), trace = 0)

summary(backward_aic)
summary(backward_bic)

# Stepwise (mixed)
stepwise_aic <- stepAIC(empty_model,
                        scope = list(upper = full_model, lower = ~1),
                        direction = "both", trace = 0)
stepwise_bic <- stepAIC(empty_model,
                        scope = list(upper = full_model, lower = ~1),
                        direction = "both", k = log(nrow(model_data)), trace = 0)

summary(stepwise_aic)
summary(stepwise_bic)


# For log_balance ---------------------------------------------------


# Model Selection:
library(MASS)

full_model <- lm(log_balance ~ log_income + log_rating + 
                   cards + age + education + male + is_student + 
                   is_married + african_american + asian, data = model_data)

empty_model <- lm(log_balance ~ 1, data = model_data)

# Forward selection
forward_aic <- stepAIC(empty_model, 
                       scope = list(upper = full_model, lower = ~1),
                       direction = "forward", trace = 0)

forward_bic <- stepAIC(empty_model,
                       scope = list(upper = full_model, lower = ~1), 
                       direction = "forward", k = log(nrow(model_data)), trace = 0)
summary(forward_aic)
summary(forward_bic)

# Backward elimination
backward_aic <- stepAIC(full_model, direction = "backward", trace = 0)
backward_bic <- stepAIC(full_model, direction = "backward", 
                        k = log(nrow(model_data)), trace = 0)

summary(backward_aic)
summary(backward_bic)

# Stepwise (mixed)
stepwise_aic <- stepAIC(empty_model,
                        scope = list(upper = full_model, lower = ~1),
                        direction = "both", trace = 0)
stepwise_bic <- stepAIC(empty_model,
                        scope = list(upper = full_model, lower = ~1),
                        direction = "both", k = log(nrow(model_data)), trace = 0)

summary(stepwise_aic)
summary(stepwise_bic)




# For original balance model ---------------------------------------------------



# Model Selection:
library(MASS)

full_model <- lm(balance ~ log_income + log_rating + 
                   cards + age + education + male + is_student + 
                   is_married + african_american + asian, data = model_data)

empty_model <- lm(balance ~ 1, data = model_data)

# Forward selection
forward_aic <- stepAIC(empty_model, 
                       scope = list(upper = full_model, lower = ~1),
                       direction = "forward", trace = 0)

forward_bic <- stepAIC(empty_model,
                       scope = list(upper = full_model, lower = ~1), 
                       direction = "forward", k = log(nrow(model_data)), trace = 0)
summary(forward_aic)
summary(forward_bic)

# Backward elimination
backward_aic <- stepAIC(full_model, direction = "backward", trace = 0)
backward_bic <- stepAIC(full_model, direction = "backward", 
                        k = log(nrow(model_data)), trace = 0)

summary(backward_aic)
summary(backward_bic)

# Stepwise (mixed)
stepwise_aic <- stepAIC(empty_model,
                        scope = list(upper = full_model, lower = ~1),
                        direction = "both", trace = 0)
stepwise_bic <- stepAIC(empty_model,
                        scope = list(upper = full_model, lower = ~1),
                        direction = "both", k = log(nrow(model_data)), trace = 0)

summary(stepwise_aic)
summary(stepwise_bic)


# Initial Model Selection:
# After residual diagnostics, I found that Initial Models from the Model Selection here violate Normality Assumption so I went on to do Boxcox transformation on original varible which did not also change much.


