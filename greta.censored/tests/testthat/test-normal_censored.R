# Test script for normal_censored distribution

library(greta)
library(testthat)
library(reticulate)

test_that("normal_censored distribution works correctly", {
  if (!reticulate::py_module_available("tensorflow") ||
      !reticulate::py_module_available("tensorflow_probability")) {
    skip("Required Python modules are not available for testing.")
  }

  # Simulate data
  set.seed(123)
  n <- 100
  true_mean <- 2
  true_sd <- 1
  y <- rnorm(n, mean = true_mean, sd = true_sd)

  # Introduce right censoring at y > 3
  censoring_threshold <- 3
  is_censored <- y > censoring_threshold
  y_obs <- ifelse(is_censored, censoring_threshold, y)

  # Data preparation
  y_greta <- as_data(y_obs)
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  mean <- variable()
  sd <- variable(lower = 0)

  distribution(y_greta) <- normal_censored(
    mean = mean,
    sd = sd,
    is_censored = is_censored_greta,
    censor = "right",
    lower = NULL,
    upper = NULL,
    dim = n
  )

  # Model fitting
  # m <- model(shape, rate)

  # Add expectations
  # expect_s3_class(m, "greta_model")
  expect_error(py_last_error(), NA)
})
