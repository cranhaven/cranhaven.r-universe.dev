# Test script for student_censored distribution

library(greta)
library(testthat)
library(reticulate)

test_that("student_censored distribution works correctly", {
  if (!reticulate::py_module_available("tensorflow") ||
      !reticulate::py_module_available("tensorflow_probability")) {
    skip("Required Python modules are not available for testing.")
  }

  # Simulate data
  set.seed(789)
  n <- 100
  true_df <- 5
  true_loc <- 0
  true_scale <- 1
  y <- rt(n, df = true_df) * true_scale + true_loc

  # Introduce interval censoring between -1 and 1
  lower_bound <- -1
  upper_bound <- 1
  is_censored <- y > lower_bound & y < upper_bound
  y_obs <- y
  y_obs[is_censored] <- NA # Interval censored data

  # Data preparation
  y_greta <- as_data(ifelse(is.na(y_obs), 0, y_obs)) # Placeholder for censored data
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  df <- variable(lower = 1)
  loc <- variable()
  scale <- variable(lower = 0)

  distribution(y_greta) <- student_censored(
    df = df,
    loc = loc,
    scale = scale,
    is_censored = is_censored_greta,
    censor = "interval",
    lower = lower_bound,
    upper = upper_bound,
    dim = n
  )

  # Model fitting
  # m <- model(shape, rate)

  # Add expectations
  # expect_s3_class(m, "greta_model")
  expect_error(py_last_error(), NA)
})
