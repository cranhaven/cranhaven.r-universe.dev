## ----eval = FALSE-------------------------------------------------------------
# # Run a linear model
# fit <- lm(100 / mpg ~ disp, data = mtcars)
# 
# # Compute the confidence intervals
# fit_confint <- confint(fit)
# 
# # Create an empty list
# statistics <- list()
# 
# # Add linear model and confidence intervals to the list
# statistics <- statistics %>%
#   add_stats(fit) %>%
#   add_stats(fit_confint)

## ----eval = FALSE-------------------------------------------------------------
# statistics <- statistics %>%
#   add_stats(fit) %>%
#   add_stats(fit_confint, class = "confint")

## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(14)

# Simulate some data
intercept_data <- data.frame(score = scale(rnorm(40), center = 0.72))

# Run two models and calculate the BIC
full_lm <- lm(score ~ 1, intercept_data)
null_lm <- lm(score ~ 0, intercept_data)

BF_BIC <- exp((BIC(null_lm) - BIC(full_lm)) / 2)

## ----eval = FALSE-------------------------------------------------------------
# # Load the tidystats package
# library(tidystats)
# 
# # Create an empty list
# statistics <- list()
# 
# # Add BIC to the list using add_stats()
# statistics <- add_stats(statistics, BF_BIC)

## ----eval = FALSE-------------------------------------------------------------
# # Create a list of custom statistics
# BIC <- custom_stats(
#   method = "BIC",
#   statistics = custom_stat(
#     name = "BIC Bayes Factor",
#     value = BF_BIC,
#     symbol = "BF",
#     subscript = "10"
#   )
# )
# 
# # Add the statistics to the list
# statistics <- add_stats(statistics, BIC)

