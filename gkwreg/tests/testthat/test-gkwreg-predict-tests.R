# Test Suite for predict.gkwreg Method
# Author: Lopes, J. E.
# Description: Comprehensive tests for the predict() method for gkwreg objects
#              covering different prediction types, newdata handling, and special features

library(testthat)
library(gkwreg)
library(gkwdist)

# Setup: Generate test data and fit model
setup_predict_data <- function() {
  set.seed(54321)
  n <- 200
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)
  x3 <- factor(sample(c("A", "B"), n, replace = TRUE))

  # Generate Kumaraswamy response
  alpha <- exp(0.6 + 0.25 * x1)
  beta <- exp(1.1 - 0.18 * x2 + 0.3 * (x3 == "B"))
  y <- rkw(n, alpha = alpha, beta = beta)

  # Ensure strictly in (0, 1)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  list(
    data = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3),
    alpha_true = alpha,
    beta_true = beta
  )
}

# =============================================================================
# CORE PREDICT TESTS
# =============================================================================

test_that("Test 1: Basic prediction with newdata returns correct response type", {
  # Test predicting mean response on new data
  setup <- setup_predict_data()

  # Fit model on first 150 observations
  train <- setup$data[1:150, ]
  test <- setup$data[151:200, ]

  fit <- gkwreg(y ~ x1 | x2 + x3, data = train, family = "kw")

  # Predict on test data
  pred <- predict(fit, newdata = test, type = "response")

  expect_type(pred, "double")
  expect_length(pred, nrow(test))
  expect_true(all(pred > 0 & pred < 1))
  expect_true(all(is.finite(pred)))
})

test_that("Test 2: Prediction without newdata uses original data correctly", {
  # Test that omitting newdata produces fitted values
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1 + x2, data = setup$data, family = "kw")

  # Predict without newdata
  pred_no_newdata <- predict(fit, type = "response")

  # Should match fitted values
  expect_equal(pred_no_newdata, fitted(fit), tolerance = 1e-10)
  expect_length(pred_no_newdata, nrow(setup$data))
})


test_that("Test 6: Density calculation at specified points works correctly", {
  # Test type = "density" evaluates PDF at given points
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  # Evaluate density at specific points
  at_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  pred_dens <- predict(fit, type = "density", at = at_values, elementwise = FALSE)

  expect_true(is.matrix(pred_dens) || is.array(pred_dens))
  expect_equal(nrow(pred_dens), nrow(setup$data))
  expect_equal(ncol(pred_dens), length(at_values))
  expect_true(all(pred_dens >= 0))
  expect_true(all(is.finite(pred_dens)))
})

test_that("Test 7: Probability/CDF calculation returns values in [0,1]", {
  # Test type = "probability" evaluates CDF at given points
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1 + x2, data = setup$data, family = "kw")

  # Evaluate CDF at specific points
  at_values <- c(0.2, 0.4, 0.6, 0.8)
  pred_prob <- predict(fit, type = "probability", at = at_values, elementwise = FALSE)

  expect_true(is.matrix(pred_prob) || is.array(pred_prob))
  expect_equal(nrow(pred_prob), nrow(setup$data))
  expect_equal(ncol(pred_prob), length(at_values))

  # CDF values must be in [0, 1]
  expect_true(all(pred_prob >= 0 & pred_prob <= 1))

  # CDF should be monotonically increasing
  for (i in 1:nrow(pred_prob)) {
    expect_true(all(diff(pred_prob[i, ]) >= 0))
  }
})

test_that("Test 8: Quantile calculation returns values in (0,1) for valid probabilities", {
  # Test type = "quantile" computes inverse CDF
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  # Compute quantiles for specific probabilities
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  pred_quant <- predict(fit, type = "quantile", at = probs, elementwise = FALSE)

  expect_true(is.matrix(pred_quant) || is.array(pred_quant))
  expect_equal(nrow(pred_quant), nrow(setup$data))
  expect_equal(ncol(pred_quant), length(probs))

  # Quantiles must be in (0, 1) for bounded distribution
  expect_true(all(pred_quant > 0 & pred_quant < 1))

  # Quantiles should be monotonically increasing
  for (i in 1:nrow(pred_quant)) {
    expect_true(all(diff(pred_quant[i, ]) >= 0))
  }
})

test_that("Test 9: Elementwise parameter controls behavior correctly", {
  # Test elementwise = TRUE vs FALSE for density/probability/quantile
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  # Create observation-specific values
  n_obs <- 50
  test_data <- setup$data[1:n_obs, ]
  at_values <- runif(n_obs, 0.1, 0.9)

  # Elementwise = TRUE: each observation gets its own value
  pred_elem <- predict(fit,
    newdata = test_data, type = "density",
    at = at_values, elementwise = TRUE
  )

  expect_type(pred_elem, "double")
  expect_length(pred_elem, n_obs)

  # Elementwise = FALSE: all values applied to all observations
  pred_non_elem <- predict(fit,
    newdata = test_data[1:5, ], type = "density",
    at = c(0.3, 0.5, 0.7), elementwise = FALSE
  )

  expect_true(is.matrix(pred_non_elem))
  expect_equal(nrow(pred_non_elem), 5)
  expect_equal(ncol(pred_non_elem), 3)
})

# =============================================================================
# ADDITIONAL EDGE CASE AND VALIDATION TESTS
# =============================================================================

test_that("Predict handles different families correctly", {
  # Test predictions across different distribution families
  setup <- setup_predict_data()

  fit_kw <- gkwreg(y ~ x1, data = setup$data, family = "kw")
  fit_beta <- gkwreg(y ~ x1, data = setup$data, family = "beta")
  fit_ekw <- gkwreg(y ~ x1, data = setup$data, family = "ekw")

  pred_kw <- predict(fit_kw, type = "response")
  pred_beta <- predict(fit_beta, type = "response")
  pred_ekw <- predict(fit_ekw, type = "response")

  expect_true(all(is.finite(pred_kw)))
  expect_true(all(is.finite(pred_beta)))
  expect_true(all(is.finite(pred_ekw)))

  # All should be in (0, 1)
  expect_true(all(pred_kw > 0 & pred_kw < 1))
  expect_true(all(pred_beta > 0 & pred_beta < 1))
  expect_true(all(pred_ekw > 0 & pred_ekw < 1))
})

test_that("Predict type aliases work correctly", {
  # Test that "mean" = "response", "pdf" = "density", "cdf" = "probability"
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  at_vals <- c(0.3, 0.5, 0.7)
  pred_density <- predict(fit, type = "density", at = at_vals, elementwise = FALSE)
  pred_pdf <- predict(fit, type = "pdf", at = at_vals, elementwise = FALSE)
  expect_equal(pred_density, pred_pdf)

  pred_probability <- predict(fit, type = "probability", at = at_vals, elementwise = FALSE)
  pred_cdf <- predict(fit, type = "cdf", at = at_vals, elementwise = FALSE)
  expect_equal(pred_probability, pred_cdf)
})

test_that("Predict with factor variables in formula works", {
  # Test predictions with categorical predictors
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x3 | x3, data = setup$data, family = "kw")

  # Create newdata with both factor levels
  newdata <- data.frame(x3 = factor(c("A", "B", "A", "B")))

  pred <- predict(fit, newdata = newdata, type = "response")

  expect_length(pred, 4)
  expect_true(all(is.finite(pred)))

  # Predictions for same level should be identical
  expect_equal(pred[1], pred[3])
  expect_equal(pred[2], pred[4])
})

test_that("Predict with interaction terms works correctly", {
  # Test predictions with interaction effects
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1 * x2, data = setup$data, family = "kw")

  newdata <- data.frame(
    x1 = c(0, 1, 0, 1),
    x2 = c(0, 0, 1, 1)
  )

  pred <- predict(fit, newdata = newdata, type = "response")

  expect_length(pred, 4)
  expect_true(all(is.finite(pred)))
  expect_true(all(pred > 0 & pred < 1))
})

test_that("Predict CDF and quantile are inverse operations", {
  # Test that quantile(CDF(y)) ≈ y
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  # Take first 10 observations
  test_data <- setup$data[1:10, ]
  y_values <- test_data$y

  # Compute CDF at y values (elementwise)
  cdf_at_y <- predict(fit,
    newdata = test_data, type = "probability",
    at = y_values, elementwise = TRUE
  )

  # Compute quantiles at those CDF values (elementwise)
  quant_at_cdf <- predict(fit,
    newdata = test_data, type = "quantile",
    at = cdf_at_y, elementwise = TRUE
  )

  # Should approximately recover original y values
  expect_equal(quant_at_cdf, y_values, tolerance = 1e-4)
})

test_that("Predict density integrates to approximately 1", {
  # Test that PDF integrates to 1 (numerical check)
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ 1, data = setup$data[1:5, ], family = "kw")

  # Create fine grid
  y_grid <- seq(0.001, 0.999, length.out = 1000)

  # Get density for first observation
  dens <- predict(fit,
    newdata = setup$data[1, , drop = FALSE],
    type = "density", at = y_grid, elementwise = FALSE
  )

  # Approximate integral using trapezoidal rule
  dy <- diff(y_grid)
  integral <- sum((dens[1, -1] + dens[1, -length(dens[1, ])]) / 2 * dy)

  # Should be close to 1
  expect_equal(integral, 1, tolerance = 0.05)
})


test_that("Predict with default at value works for density", {
  # Test default at = 0.5 for density/probability
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  # Should use default at = 0.5
  pred_dens_default <- predict(fit, type = "density")
  pred_dens_explicit <- predict(fit, type = "density", at = 0.5, elementwise = FALSE)

  expect_equal(pred_dens_default[, 1], pred_dens_explicit[, 1])
})

test_that("Predict handles single observation correctly", {
  # Test prediction for single row of data
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1 + x2, data = setup$data, family = "kw")

  single_obs <- setup$data[1, , drop = FALSE]
  pred_single <- predict(fit, newdata = single_obs, type = "response")

  expect_length(pred_single, 1)
  expect_true(is.finite(pred_single))
  expect_true(pred_single > 0 & pred_single < 1)
})

test_that("Predict with very large newdata works efficiently", {
  # Test scalability with large prediction dataset
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1 + x2, data = setup$data[1:100, ], family = "kw")

  # Create large newdata
  large_newdata <- data.frame(
    x1 = rnorm(10000),
    x2 = runif(10000, -1, 1)
  )

  # Should complete without error
  expect_no_error(
    pred_large <- predict(fit, newdata = large_newdata, type = "response")
  )

  expect_length(pred_large, 10000)
  expect_true(all(is.finite(pred_large)))
})

test_that("Predict with missing required 'at' parameter errors appropriately", {
  # Test that density/probability/quantile require 'at' parameter
  setup <- setup_predict_data()

  fit <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  # These should work with default at = 0.5 or error if at is required
  expect_no_error(
    predict(fit, type = "density")
  )
})
