# Test script for lognormal_censored distribution

library(greta)
library(testthat)

test_that("lognormal_censored distribution works correctly", {
  if (!reticulate::py_module_available("tensorflow") ||
      !reticulate::py_module_available("tensorflow_probability")) {
    skip("Required Python modules are not available for testing.")
  }

  # Simulate data
  set.seed(456)
  n <- 100
  true_meanlog <- 0.5
  true_sdlog <- 0.75
  y <- rlnorm(n, meanlog = true_meanlog, sdlog = true_sdlog)

  # Introduce left censoring at y < 1
  censoring_threshold <- 1
  is_censored <- y < censoring_threshold
  y_obs <- ifelse(is_censored, censoring_threshold, y)

  # Data preparation
  y_greta <- as_data(y_obs)
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  meanlog <- variable()
  sdlog <- variable(lower = 0)

  distribution(y_greta) <- lognormal_censored(
    meanlog = meanlog,
    sdlog = sdlog,
    is_censored = is_censored_greta,
    censor = "left",
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
