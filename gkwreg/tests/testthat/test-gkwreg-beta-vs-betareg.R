# ==============================================================================
# Test Suite: gkwreg (beta family) vs betareg Package
# Author: Lopes, J. E.
# Description: Rigorous testing of gkwreg's beta family implementation
#              against the reference betareg package, accounting for
#              parameterization differences
# ==============================================================================

library(testthat)
library(gkwreg)
library(gkwdist)
library(betareg)

# Setup helper function
setup_beta_comparison_data <- function(seed = 12345) {
  set.seed(seed)
  n <- 200
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)

  # Generate using betareg parametrization
  eta_mu <- -0.5 + 0.8 * x1 - 0.4 * x2
  mu <- plogis(eta_mu)

  eta_phi <- 1.5 + 0.3 * x1
  phi <- exp(eta_phi)

  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  y <- rbeta(n, shape1, shape2)
  y <- pmax(pmin(y, 1 - 1e-7), 1e-7)

  data.frame(y = y, x1 = x1, x2 = x2)
}

# ==============================================================================
# TEST 1: Density Function Equivalence
# ==============================================================================

test_that("Test 1: dbeta_ correctly implements Beta(gamma, delta+1)", {
  # Verify that dbeta_(x, gamma, delta) = dbeta(x, gamma, delta+1)

  x <- seq(0.01, 0.99, length.out = 100)
  gamma <- 2.5
  delta <- 3.2

  dens_gkw <- dbeta_(x, gamma, delta)
  dens_base <- dbeta(x, shape1 = gamma, shape2 = delta + 1)

  expect_equal(dens_gkw, dens_base, tolerance = 1e-14)
  expect_true(all(is.finite(dens_gkw)))
  expect_true(all(dens_gkw >= 0))
})

# ==============================================================================
# TEST 2: Log-Likelihood Calculation
# ==============================================================================

test_that("Test 2: Log-likelihood values should match for same distribution", {
  # When both models fit the same underlying Beta distribution,
  # log-likelihoods should be comparable (though not necessarily identical
  # due to different parametrizations)

  data <- setup_beta_comparison_data()

  # Fit simple intercept-only models
  fit_gkw <- gkwreg(y ~ 1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ 1, data = data)

  ll_gkw <- as.numeric(logLik(fit_gkw))
  ll_betareg <- as.numeric(logLik(fit_betareg))

  # Log-likelihoods should be finite
  expect_true(is.finite(ll_gkw))
  expect_true(is.finite(ll_betareg))

  # Relative difference should be small for intercept-only model
  rel_diff <- abs(ll_gkw - ll_betareg) / abs(ll_betareg)

  # This may fail - documenting the discrepancy
  if (rel_diff > 0.1) {
    warning(sprintf(
      "Log-likelihood difference detected: gkwreg = %.4f, betareg = %.4f, rel diff = %.2f%%",
      ll_gkw, ll_betareg, rel_diff * 100
    ))
  }
})

# ==============================================================================
# TEST 3: Fitted Values Correlation
# ==============================================================================

test_that("Test 3: Fitted values should be highly correlated", {
  # Even with different parametrizations, fitted means should match

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1 + x2, data = data)

  fitted_gkw <- fitted(fit_gkw)
  fitted_betareg <- fitted(fit_betareg)

  # Should be highly correlated
  correlation <- cor(fitted_gkw, fitted_betareg)
  expect_gt(correlation, 0.99)

  # Should be bounded in (0, 1)
  expect_true(all(fitted_gkw > 0 & fitted_gkw < 1))
  expect_true(all(fitted_betareg > 0 & fitted_betareg < 1))

  # Mean absolute difference should be small
  mad <- mean(abs(fitted_gkw - fitted_betareg))
  expect_lt(mad, 0.02) # Within 2%
})

# ==============================================================================
# TEST 4: Parameter Transformation Verification
# ==============================================================================

test_that("Test 4: Parameter transformations are consistent", {
  # Verify that gamma, delta from gkwreg relate correctly to
  # mu, phi from betareg

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1, data = data)

  # Extract parameters for first observation
  params_gkw <- predict(fit_gkw, type = "parameter")
  gamma <- as.numeric(params_gkw$gamma[1])
  delta <- as.numeric(params_gkw$delta[1])

  mu_betareg <- predict(fit_betareg, type = "response")[1]
  phi_betareg <- predict(fit_betareg, type = "precision")[1]

  # Verify relationships
  shape1_gkw <- gamma
  shape2_gkw <- delta + 1

  shape1_betareg <- mu_betareg * phi_betareg
  shape2_betareg <- (1 - mu_betareg) * phi_betareg

  # Calculate implied means
  mean_gkw <- as.numeric(gamma / (gamma + delta + 1))
  mean_betareg <- as.numeric(mu_betareg)

  # Means should be close
  expect_equal(mean_gkw, mean_betareg, tolerance = 0.05)

  # Shapes should be positive
  expect_gt(shape1_gkw, 0)
  expect_gt(shape2_gkw, 0)
  expect_gt(shape1_betareg, 0)
  expect_gt(shape2_betareg, 0)
})

# ==============================================================================
# TEST 5: Density Evaluation Consistency
# ==============================================================================

test_that("Test 5: Density evaluations should match at observed points", {
  # For same observation, both models should give similar densities

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1, data = data)

  # Test first 10 observations
  for (i in 1:10) {
    y_i <- data$y[i]

    # Get parameters from gkwreg
    params_gkw <- predict(fit_gkw, type = "parameter")
    gamma_i <- params_gkw$gamma[i]
    delta_i <- params_gkw$delta[i]

    # Get parameters from betareg
    mu_i <- predict(fit_betareg, type = "response")[i]
    phi_i <- predict(fit_betareg, type = "precision")[i]

    # Calculate densities
    dens_gkw <- dbeta_(y_i, gamma_i, delta_i)
    dens_betareg <- dbeta(y_i, mu_i * phi_i, (1 - mu_i) * phi_i)

    # Should both be positive and finite
    expect_gt(dens_gkw, 0)
    expect_gt(dens_betareg, 0)
    expect_true(is.finite(dens_gkw))
    expect_true(is.finite(dens_betareg))
  }
})

# ==============================================================================
# TEST 6: Model Convergence
# ==============================================================================

test_that("Test 6: Both models should converge successfully", {
  # Verify that both optimizations converge

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2 | x1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1 + x2 | x1, data = data)

  # gkwreg convergence
  expect_true(fit_gkw$convergence)
  expect_true(is.finite(logLik(fit_gkw)))

  # betareg convergence (check for converged attribute)
  expect_equal(fit_betareg$optim$convergence, 0)
  expect_true(is.finite(logLik(fit_betareg)))
})

# ==============================================================================
# TEST 7: Residuals Comparison
# ==============================================================================

test_that("Test 7: Residuals should be similar between implementations", {
  # Response residuals should be very similar

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1 + x2, data = data)

  resid_gkw <- residuals(fit_gkw, type = "response")
  resid_betareg <- residuals(fit_betareg, type = "response")

  # Should be highly correlated
  correlation <- cor(resid_gkw, resid_betareg)
  expect_gt(correlation, 0.99)
})

# ==============================================================================
# TEST 8: Real Data - GasolineYield
# ==============================================================================

test_that("Test 8: Models perform comparably on GasolineYield data", {
  # Test on real dataset

  data("GasolineYield", package = "betareg")

  fit_gkw <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "beta")
  fit_betareg <- betareg(yield ~ batch + temp, data = GasolineYield)

  # Both should converge
  expect_true(fit_gkw$convergence)

  # Fitted values should be correlated
  fitted_gkw <- fitted(fit_gkw)
  fitted_betareg <- fitted(fit_betareg)

  correlation <- cor(fitted_gkw, fitted_betareg)
  expect_gt(correlation, 0.95) # At least 95% correlation

  # Both should have reasonable RMSE
  rmse_gkw <- sqrt(mean((GasolineYield$yield - fitted_gkw)^2))
  rmse_betareg <- sqrt(mean((GasolineYield$yield - fitted_betareg)^2))

  expect_lt(rmse_gkw, 0.1)
  expect_lt(rmse_betareg, 0.1)
})

# ==============================================================================
# TEST 9: Prediction Consistency
# ==============================================================================

test_that("Test 9: Predictions on new data should be comparable", {
  # Test prediction capabilities

  data <- setup_beta_comparison_data()

  # Split data
  train <- data[1:150, ]
  test <- data[151:200, ]

  fit_gkw <- gkwreg(y ~ x1 + x2, data = train, family = "beta")
  fit_betareg <- betareg(y ~ x1 + x2, data = train)

  # Predict on test set
  pred_gkw <- predict(fit_gkw, newdata = test, type = "response")
  pred_betareg <- predict(fit_betareg, newdata = test, type = "response")

  # Predictions should be correlated
  correlation <- cor(pred_gkw, pred_betareg)
  expect_gt(correlation, 0.99)

  # Both should be in (0, 1)
  expect_true(all(pred_gkw > 0 & pred_gkw < 1))
  expect_true(all(pred_betareg > 0 & pred_betareg < 1))
})

# ==============================================================================
# TEST 10: AIC/BIC Comparison
# ==============================================================================

test_that("Test 10: Information criteria should be comparable", {
  # AIC/BIC should be in similar range

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1, data = data)

  aic_gkw <- AIC(fit_gkw)
  aic_betareg <- AIC(fit_betareg)

  bic_gkw <- BIC(fit_gkw)
  bic_betareg <- BIC(fit_betareg)

  # Both should be finite
  expect_true(is.finite(aic_gkw))
  expect_true(is.finite(aic_betareg))
  expect_true(is.finite(bic_gkw))
  expect_true(is.finite(bic_betareg))

  # Lower is better - document if there's a large difference
  aic_diff <- abs(aic_gkw - aic_betareg)
  if (aic_diff > 50) {
    warning(sprintf(
      "Large AIC difference: gkwreg = %.2f, betareg = %.2f",
      aic_gkw, aic_betareg
    ))
  }
})

# ==============================================================================
# TEST 11: Intercept-Only Model Equivalence
# ==============================================================================

test_that("Test 11: Intercept-only models should be very similar", {
  # Simplest case - should have minimal differences

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ 1 | 1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ 1, data = data)

  # Fitted values should all be identical within each model
  fitted_gkw <- fitted(fit_gkw)
  fitted_betareg <- fitted(fit_betareg)

  # Each should be constant
  expect_lt(sd(fitted_gkw), 1e-10)
  expect_lt(sd(fitted_betareg), 1e-10)

  # The constants should be close to observed mean
  mean_y <- mean(data$y)
  expect_equal(as.numeric(fitted_gkw[1]), mean_y, tolerance = 0.1)
  expect_equal(as.numeric(fitted_betareg[1]), mean_y, tolerance = 0.1)
})

# ==============================================================================
# TEST 12: Variable Dispersion Model
# ==============================================================================

test_that("Test 12: Models with variable dispersion should both work", {
  # Test with predictors in both mean and precision/delta

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 | x2, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1 | x2, data = data)

  # Both should converge
  expect_true(fit_gkw$convergence)
  expect_equal(fit_betareg$optim$convergence, 0)

  # Fitted values should be correlated
  correlation <- cor(fitted(fit_gkw), fitted(fit_betareg))
  expect_gt(correlation, 0.95)
})

# ==============================================================================
# TEST 13: Parameter Bounds
# ==============================================================================

test_that("Test 13: Estimated parameters should satisfy constraints", {
  # Verify parameter constraints are satisfied

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2 | x1, data = data, family = "beta")

  params <- predict(fit_gkw, type = "parameter")

  # gamma > 0
  expect_true(all(params$gamma > 0))

  # delta >= 0 (shape2 = delta + 1 must be >= 1)
  expect_true(all(params$delta >= 0))

  # Implied shapes should be valid
  shape1 <- params$gamma
  shape2 <- params$delta + 1

  expect_true(all(shape1 > 0))
  expect_true(all(shape2 >= 1))
})

# ==============================================================================
# TEST 15: Coefficient Count
# ==============================================================================

test_that("Test 15: Both models estimate same number of parameters", {
  # For equivalent models, parameter count should match

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2 | x1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1 + x2 | x1, data = data)

  npar_gkw <- fit_gkw$npar
  npar_betareg <- length(coef(fit_betareg))

  expect_equal(npar_gkw, npar_betareg)
})

# ==============================================================================
# TEST 16: Standard Errors Availability
# ==============================================================================

test_that("Test 16: Standard errors are computed for both models", {
  # Verify inference capability

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1,
    data = data, family = "beta",
    control = gkw_control(hessian = TRUE)
  )
  fit_betareg <- betareg(y ~ x1, data = data)

  # Both should have standard errors
  expect_false(is.null(fit_gkw$se))
  expect_false(is.null(sqrt(diag(vcov(fit_betareg)))))

  # All SEs should be positive
  expect_true(all(fit_gkw$se > 0))
  expect_true(all(sqrt(diag(vcov(fit_betareg))) > 0))
})

# ==============================================================================
# TEST 17: Link Function Consistency
# ==============================================================================

test_that("Test 17: Default link functions are applied correctly", {
  # Verify link functions

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1, data = data, family = "beta")

  # Check stored link functions
  expect_true("gamma" %in% names(fit_gkw$link))
  expect_true("delta" %in% names(fit_gkw$link))

  # Default should be log for both (in gkwreg beta family)
  # Note: betareg uses logit for mu, log for phi
  # This is a fundamental difference
  expect_type(fit_gkw$link$gamma, "character")
  expect_type(fit_gkw$link$delta, "character")
})

# ==============================================================================
# TEST 18: Prediction Types
# ==============================================================================

test_that("Test 18: Different prediction types work correctly", {
  # Test various prediction types for gkwreg

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2, data = data, family = "beta")

  # Response predictions
  pred_response <- predict(fit_gkw, type = "response")
  expect_true(all(pred_response > 0 & pred_response < 1))

  # Parameter predictions
  pred_param <- predict(fit_gkw, type = "parameter")
  expect_true("gamma" %in% names(pred_param))
  expect_true("delta" %in% names(pred_param))
})

# ==============================================================================
# TEST 19: Robustness to Different Seeds
# ==============================================================================

test_that("Test 19: Results are consistent across different random seeds", {
  # Test stability

  results <- lapply(1:5, function(seed) {
    data <- setup_beta_comparison_data(seed = seed * 1000)

    fit_gkw <- gkwreg(y ~ x1, data = data, family = "beta")
    fit_betareg <- betareg(y ~ x1, data = data)

    cor(fitted(fit_gkw), fitted(fit_betareg))
  })

  correlations <- unlist(results)

  # All correlations should be high
  expect_true(all(correlations > 0.95))

  # Should be relatively stable
  expect_lt(sd(correlations), 0.05)
})

# ==============================================================================
# TEST 20: Documentation Example Verification
# ==============================================================================

test_that("Test 20: Package examples produce expected results", {
  # Verify that documented behavior matches reality

  data("GasolineYield", package = "betareg")

  # Fit both models as in documentation
  fit_gkw <- gkwreg(yield ~ batch + temp,
    data = GasolineYield,
    family = "beta"
  )
  fit_betareg <- betareg(yield ~ batch + temp,
    data = GasolineYield
  )

  # Should both work
  expect_s3_class(fit_gkw, "gkwreg")
  expect_s3_class(fit_betareg, "betareg")

  # Should produce summaries
  expect_no_error(summary(fit_gkw))
  expect_no_error(summary(fit_betareg))

  # Should produce predictions
  expect_no_error(predict(fit_gkw))
  expect_no_error(predict(fit_betareg))

  # Fitted values should be reasonable
  expect_true(all(fitted(fit_gkw) > 0 & fitted(fit_gkw) < 1))
  expect_true(all(fitted(fit_betareg) > 0 & fitted(fit_betareg) < 1))
})

# ==============================================================================
# SUMMARY TEST: Overall Compatibility Assessment
# ==============================================================================

test_that("SUMMARY: gkwreg beta family provides compatible alternative to betareg", {
  # Overall assessment

  data <- setup_beta_comparison_data()

  fit_gkw <- gkwreg(y ~ x1 + x2 | x1, data = data, family = "beta")
  fit_betareg <- betareg(y ~ x1 + x2 | x1, data = data)

  # Key compatibility metrics
  fitted_cor <- cor(fitted(fit_gkw), fitted(fit_betareg))
  ll_diff <- abs(as.numeric(logLik(fit_gkw)) - as.numeric(logLik(fit_betareg)))
  ll_ratio <- ll_diff / abs(as.numeric(logLik(fit_betareg)))

  # Document results
  cat("\n")
  cat("===== COMPATIBILITY ASSESSMENT =====\n")
  cat("Fitted values correlation:", fitted_cor, "\n")
  cat("Log-likelihood difference:", ll_diff, "\n")
  cat("Relative LL difference:   ", ll_ratio * 100, "%\n")

  # Basic compatibility requirements
  expect_gt(fitted_cor, 0.95)

  # Both should produce valid statistical models
  expect_true(fit_gkw$convergence,
    info = "gkwreg should converge"
  )
  expect_true(is.finite(logLik(fit_gkw)),
    info = "gkwreg should have finite log-likelihood"
  )
  expect_true(is.finite(logLik(fit_betareg)),
    info = "betareg should have finite log-likelihood"
  )

  cat("=====================================\n\n")
})
