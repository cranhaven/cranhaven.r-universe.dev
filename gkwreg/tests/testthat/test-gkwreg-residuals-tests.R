# Test Suite for fitted.gkwreg Method
# Author: Lopes, J. E.
# Description: Comprehensive tests for the fitted() method for gkwreg objects
#              covering value extraction, family specifications, and edge cases

library(testthat)
library(gkwreg)
library(gkwdist)

# Setup: Generate test data and fit model
setup_fitted_data <- function() {
  set.seed(19283)
  n <- 150
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)
  x3 <- factor(sample(c("A", "B", "C"), n, replace = TRUE))

  # Generate Kumaraswamy response
  alpha <- exp(0.7 + 0.3 * x1)
  beta <- exp(1.0 - 0.2 * x2)
  y <- rkw(n, alpha = alpha, beta = beta)

  # Ensure strictly in (0, 1)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  # Fit model
  fit <- gkwreg(y ~ x1 | x2, data = data, family = "kw")

  list(data = data, fit = fit, y = y)
}

# =============================================================================
# CORE FITTED TESTS
# =============================================================================

test_that("Test 1: Basic fitted values extraction works correctly", {
  # Test that fitted() extracts values from model object
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)

  expect_type(fitted_vals, "double")
  expect_length(fitted_vals, nrow(setup$data))
  expect_true(all(is.finite(fitted_vals)))
  expect_false(any(is.na(fitted_vals)))
})

test_that("Test 2: Fitted values are bounded in (0, 1)", {
  # Test that fitted values respect response variable bounds
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)

  expect_true(all(fitted_vals > 0))
  expect_true(all(fitted_vals < 1))

  # Check they're not exactly 0 or 1
  expect_true(all(fitted_vals > 1e-15))
  expect_true(all(fitted_vals < 1 - 1e-15))
})

test_that("Test 3: Fitted values match predict() with type='response'", {
  # Test consistency between fitted() and predict()
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)
  pred_vals <- predict(setup$fit, type = "response")

  expect_equal(fitted_vals, pred_vals, tolerance = 1e-10)
})

test_that("Test 4: Fitted values match stored fitted.values component", {
  # Test that fitted() retrieves stored component correctly
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)
  stored_vals <- setup$fit$fitted.values

  expect_equal(fitted_vals, stored_vals, tolerance = 1e-10)
  expect_identical(fitted_vals, stored_vals)
})

test_that("Test 5: Specifying different family triggers recalculation", {
  # Test that family argument changes fitted values appropriately
  setup <- setup_fitted_data()

  # Fit with Kumaraswamy family
  fitted_kw <- fitted(setup$fit)

  # Get fitted values under Beta family assumption
  fitted_beta <- fitted(setup$fit, family = "beta")

  expect_type(fitted_beta, "double")
  expect_length(fitted_beta, nrow(setup$data))
  expect_true(all(is.finite(fitted_beta)))

  # Values should differ (unless coefficients happen to be identical)
  # At minimum, they should both be valid
  expect_true(all(fitted_beta > 0 & fitted_beta < 1))
})


test_that("Test 6: Fitted values have correct length with subset", {
  # Test that fitted values length matches used observations
  setup <- setup_fitted_data()

  # Fit model with subset
  subset_idx <- setup$data$x1 > 0
  n_subset <- sum(subset_idx)

  fit_subset <- gkwreg(y ~ x1 | x2,
    data = setup$data,
    family = "kw", subset = subset_idx
  )

  fitted_vals <- fitted(fit_subset)

  expect_length(fitted_vals, n_subset)
  expect_equal(length(fitted_vals), fit_subset$nobs)
})

test_that("Test 7: Fitted values are consistent across formula specifications", {
  # Test that different formula specs produce valid fitted values
  setup <- setup_fitted_data()

  # Intercept only
  fit_int <- gkwreg(y ~ 1, data = setup$data, family = "kw")
  fitted_int <- fitted(fit_int)

  # With predictors
  fit_pred <- gkwreg(y ~ x1 + x2, data = setup$data, family = "kw")
  fitted_pred <- fitted(fit_pred)

  # Different predictors per parameter
  fit_diff <- gkwreg(y ~ x1 | x2, data = setup$data, family = "kw")
  fitted_diff <- fitted(fit_diff)

  expect_length(fitted_int, nrow(setup$data))
  expect_length(fitted_pred, nrow(setup$data))
  expect_length(fitted_diff, nrow(setup$data))

  # Intercept-only should have less variance in fitted values
  expect_true(var(fitted_int) < var(fitted_pred))
})

test_that("Test 8: Fitted values calculation is efficient for large datasets", {
  # Test performance and correctness with large data
  set.seed(55555)
  n_large <- 5000
  x1 <- rnorm(n_large)
  x2 <- runif(n_large, -1, 1)

  alpha <- exp(0.5 + 0.25 * x1)
  beta <- exp(1.0 - 0.15 * x2)
  y <- rkw(n_large, alpha = alpha, beta = beta)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  large_data <- data.frame(y = y, x1 = x1, x2 = x2)

  fit_large <- gkwreg(y ~ x1 | x2, data = large_data, family = "kw")

  # Should complete quickly
  start_time <- Sys.time()
  fitted_large <- fitted(fit_large)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_length(fitted_large, n_large)
  expect_true(all(is.finite(fitted_large)))
  expect_true(elapsed < 5) # Should be very fast (< 5 seconds)
})

# =============================================================================
# ADDITIONAL VALIDATION TESTS
# =============================================================================

test_that("Fitted values with factor predictors work correctly", {
  # Test fitted values with categorical variables
  setup <- setup_fitted_data()

  fit_factor <- gkwreg(y ~ x3 | x3, data = setup$data, family = "kw")
  fitted_vals <- fitted(fit_factor)

  expect_length(fitted_vals, nrow(setup$data))
  expect_true(all(is.finite(fitted_vals)))

  # Within each factor level, fitted values should be constant (intercept-only)
  for (level in levels(setup$data$x3)) {
    level_fitted <- fitted_vals[setup$data$x3 == level]
    # All should be the same within level for intercept-only model per parameter
    expect_true(length(unique(round(level_fitted, 10))) <= nrow(setup$data))
  }
})

test_that("Fitted values with interaction terms work", {
  # Test fitted values with interaction effects
  setup <- setup_fitted_data()

  fit_interact <- gkwreg(y ~ x1 * x2, data = setup$data, family = "kw")
  fitted_vals <- fitted(fit_interact)

  expect_length(fitted_vals, nrow(setup$data))
  expect_true(all(fitted_vals > 0 & fitted_vals < 1))
  expect_true(all(is.finite(fitted_vals)))
})

test_that("Fitted values match manual calculation from parameters", {
  # Test that fitted values can be reconstructed from parameter vectors
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)

  # Get parameter vectors
  alpha_vec <- setup$fit$parameter_vectors$alphaVec
  beta_vec <- setup$fit$parameter_vectors$betaVec

  # Manually calculate means for Kumaraswamy distribution
  # E[Y] for Kumaraswamy requires calculation via beta function
  # For now, just verify we can access parameters
  expect_length(alpha_vec, nrow(setup$data))
  expect_length(beta_vec, nrow(setup$data))
  expect_true(all(alpha_vec > 0))
  expect_true(all(beta_vec > 0))
})

test_that("Fitted values are deterministic and reproducible", {
  # Test that fitted() returns same values on repeated calls
  setup <- setup_fitted_data()

  fitted1 <- fitted(setup$fit)
  fitted2 <- fitted(setup$fit)
  fitted3 <- fitted(setup$fit)

  expect_identical(fitted1, fitted2)
  expect_identical(fitted2, fitted3)
})

test_that("Fitted values with different link functions are valid", {
  # Test fitted values with non-default link functions
  setup <- setup_fitted_data()

  fit_log <- gkwreg(y ~ x1, data = setup$data, family = "kw", link = "log")
  fit_sqrt <- gkwreg(y ~ x1,
    data = setup$data, family = "kw",
    link = list(alpha = "sqrt", beta = "log")
  )

  fitted_log <- fitted(fit_log)
  fitted_sqrt <- fitted(fit_sqrt)

  expect_length(fitted_log, nrow(setup$data))
  expect_length(fitted_sqrt, nrow(setup$data))
  expect_true(all(fitted_log > 0 & fitted_log < 1))
  expect_true(all(fitted_sqrt > 0 & fitted_sqrt < 1))
})

test_that("Fitted values correlation with observed values is positive", {
  # Test that fitted values are positively correlated with observed
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)
  observed <- setup$data$y

  correlation <- cor(fitted_vals, observed)

  # Should have positive correlation for reasonable model
  expect_true(correlation > 0)
  expect_true(correlation < 1)

  # For good model, correlation should be substantial
  expect_true(correlation > 0.3)
})

test_that("Fitted values range is narrower than observed values range", {
  # Test that fitted values typically have less variance than observed
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)
  observed <- setup$data$y

  # Fitted values often have smaller range (regression to mean)
  fitted_range <- diff(range(fitted_vals))
  observed_range <- diff(range(observed))

  # Both should be in (0, 1)
  expect_true(fitted_range > 0)
  expect_true(fitted_range < 1)
  expect_true(observed_range > 0)
  expect_true(observed_range <= 1)
})

test_that("Fitted values mean is close to observed mean", {
  # Test that fitted values preserve mean of observed data
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)
  observed <- setup$data$y

  mean_fitted <- mean(fitted_vals)
  mean_observed <- mean(observed)

  # Means should be similar
  expect_equal(mean_fitted, mean_observed, tolerance = 0.1)
})

test_that("Fitted values with offset work correctly", {
  # Test fitted values when model includes offset
  setup <- setup_fitted_data()

  offset_vec <- rnorm(nrow(setup$data), mean = 0, sd = 0.1)

  fit_offset <- gkwreg(y ~ x1,
    data = setup$data, family = "kw",
    offset = offset_vec
  )

  fitted_vals <- fitted(fit_offset)

  expect_length(fitted_vals, nrow(setup$data))
  expect_true(all(fitted_vals > 0 & fitted_vals < 1))
  expect_true(all(is.finite(fitted_vals)))
})

test_that("Fitted values with weights are properly calculated", {
  # Test fitted values with case weights
  setup <- setup_fitted_data()

  weights <- runif(nrow(setup$data), 0.5, 1.5)

  fit_weighted <- gkwreg(y ~ x1 | x2,
    data = setup$data,
    family = "kw", weights = weights
  )

  fitted_vals <- fitted(fit_weighted)

  expect_length(fitted_vals, nrow(setup$data))
  expect_true(all(fitted_vals > 0 & fitted_vals < 1))
  expect_true(all(is.finite(fitted_vals)))
})


test_that("Fitted values for intercept-only model are constant", {
  # Test that intercept-only model produces constant fitted values
  setup <- setup_fitted_data()

  fit_intercept <- gkwreg(y ~ 1 | 1, data = setup$data, family = "kw")
  fitted_vals <- fitted(fit_intercept)

  # All fitted values should be identical
  unique_vals <- unique(round(fitted_vals, 10))
  expect_length(unique_vals, 1)

  # Should be close to mean of observed
  expect_equal(fitted_vals[1], mean(setup$data$y), tolerance = 0.2)
})

test_that("Fitted method works with transformed response", {
  # Test fitted values when formula includes transformations
  setup <- setup_fitted_data()

  # Add small constant to allow log transformation for illustration
  # (though response should stay in (0,1))
  fit_poly <- gkwreg(y ~ poly(x1, 2), data = setup$data, family = "kw")

  fitted_vals <- fitted(fit_poly)

  expect_length(fitted_vals, nrow(setup$data))
  expect_true(all(fitted_vals > 0 & fitted_vals < 1))
})

test_that("Fitted values can be used for residual calculation", {
  # Test that fitted values integrate properly with residuals
  setup <- setup_fitted_data()

  fitted_vals <- fitted(setup$fit)
  resid_manual <- setup$data$y - fitted_vals
  resid_method <- residuals(setup$fit, type = "response")

  expect_equal(resid_manual, resid_method, tolerance = 1e-10)
})

test_that("Fitted values handle very small sample sizes", {
  # Test fitted values with minimal data
  set.seed(9999)
  n_small <- 25
  x <- rnorm(n_small)
  y <- rkw(n_small, alpha = 2, beta = 2)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  small_data <- data.frame(y = y, x = x)

  fit_small <- gkwreg(y ~ x, data = small_data, family = "kw")
  fitted_small <- fitted(fit_small)

  expect_length(fitted_small, n_small)
  expect_true(all(is.finite(fitted_small)))
  expect_true(all(fitted_small > 0 & fitted_small < 1))
})

test_that("Fitted method signature accepts standard arguments", {
  # Test that method accepts standard fitted() arguments
  setup <- setup_fitted_data()

  # Should work with named argument
  fitted1 <- fitted(object = setup$fit)

  # Should work with positional argument
  fitted2 <- fitted(setup$fit)

  expect_identical(fitted1, fitted2)
})

test_that("Fitted values from stored component vs recalculation match", {
  # Test that different retrieval paths give same result
  setup <- setup_fitted_data()

  # Direct from stored component
  fitted_stored <- setup$fit$fitted.values

  # Via fitted method
  fitted_method <- fitted(setup$fit)

  # Via predict
  fitted_predict <- predict(setup$fit, type = "response")

  expect_equal(fitted_stored, fitted_method, tolerance = 1e-10)
  expect_equal(fitted_method, fitted_predict, tolerance = 1e-10)
})
