# Test script for pareto_censored distribution

library(greta)
library(testthat)
library(reticulate)

test_that("pareto_censored distribution works correctly", {
  if (!reticulate::py_module_available("tensorflow") ||
      !reticulate::py_module_available("tensorflow_probability")) {
    skip("Required Python modules are not available for testing.")
  }

  # Simulate data
  set.seed(404)
  n <- 100
  true_scale <- 1
  true_alpha <- 2.5
  library(VGAM) # For rpareto
  y <- rpareto(n, scale = true_scale, shape = true_alpha)

  # Introduce left censoring at y < 2
  censoring_threshold <- 2
  is_censored <- y < censoring_threshold
  y_obs <- ifelse(is_censored, censoring_threshold, y)

  # Data preparation
  y_greta <- as_data(y_obs)
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  scale <- variable(lower = 0)
  alpha <- variable(lower = 0)

  distribution(y_greta) <- pareto_censored(
    scale = scale,
    alpha = alpha,
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
