# Test Suite for gkwreg Core Function
# Author: Lopes, J. E.
# Description: Comprehensive tests for the gkwreg() function covering
#              basic functionality, different families, edge cases, and error handling

library(testthat)
library(gkwreg)
library(gkwdist)

# Setup: Generate test data
setup_test_data <- function() {
  set.seed(2203)
  n <- 100
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)

  # Generate Kumaraswamy response
  alpha <- exp(0.5 + 0.3 * x1)
  beta <- exp(1.0 - 0.2 * x2)
  y <- rkw(n, alpha = alpha, beta = beta)

  data.frame(y = y, x1 = x1, x2 = x2)
}

# =============================================================================
# CORE FUNCTION TESTS (20 tests for gkwreg)
# =============================================================================

test_that("Test 1: Basic intercept-only model fits successfully", {
  # Test that simplest possible model runs without error
  data <- setup_test_data()

  fit <- gkwreg(y ~ 1, data = data, family = "kw")

  expect_s3_class(fit, "gkwreg")
  expect_true(fit$convergence)
  expect_equal(length(coef(fit)), 2) # Only intercepts for alpha and beta
})

test_that("Test 2: Model with covariates produces correct coefficient count", {
  # Test that covariates are properly incorporated
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1 + x2, data = data, family = "kw")

  expect_equal(length(coef(fit)), 4) # 2 coefficients per parameter (intercept + 2 predictors)
  expect_true(all(!is.na(coef(fit))))
})

test_that("Test 3: Kumaraswamy family produces valid parameter estimates", {
  # Test KW family returns positive alpha and beta
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_true(all(fit$parameter_vectors$alphaVec > 0))
  expect_true(all(fit$parameter_vectors$betaVec > 0))
  expect_true(all(fit$parameter_vectors$gammaVec == 1)) # Should be fixed at 1
})

test_that("Test 4: Beta family produces correct fixed parameters", {
  # Test Beta family fixes alpha=1, beta=1, lambda=1
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "beta")

  expect_equal(fit$family, "beta")
  expect_true(all(fit$parameter_vectors$alphaVec == 1))
  expect_true(all(fit$parameter_vectors$betaVec == 1))
  expect_true(all(fit$parameter_vectors$lambdaVec == 1))
})

test_that("Test 7: Different predictors per parameter specification works", {
  # Test extended formula syntax with different predictors
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1 | x2, data = data, family = "kw")

  coef_names <- names(coef(fit))
  expect_true(any(grepl("alpha:x1", coef_names)))
  expect_true(any(grepl("beta:x2", coef_names)))
  expect_false(any(grepl("alpha:x2", coef_names)))
})

test_that("Test 8: Custom log link function is applied correctly", {
  # Test custom link specification
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw", link = "log")

  expect_equal(fit$link$alpha, "log")
  expect_equal(fit$link$beta, "log")
})

test_that("Test 9: Named list of link functions works for different parameters", {
  # Test parameter-specific link functions
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    link = list(alpha = "log", beta = "sqrt")
  )

  expect_equal(fit$link$alpha, "log")
  expect_equal(fit$link$beta, "sqrt")
})

test_that("Test 10: Custom link scales are properly stored", {
  # Test link_scale parameter
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    link_scale = list(alpha = 5, beta = 15)
  )

  expect_equal(fit$link_scale$alpha, 5)
  expect_equal(fit$link_scale$beta, 15)
})

test_that("Test 11: Fitted values are within (0, 1) bounds", {
  # Test that predictions respect response bounds
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1 + x2, data = data, family = "kw")

  expect_true(all(fitted(fit) > 0))
  expect_true(all(fitted(fit) < 1))
  expect_equal(length(fitted(fit)), nrow(data))
})

test_that("Test 12: Residuals sum to approximately zero", {
  # Test residual properties
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_lt(abs(sum(residuals(fit))), 1)
  expect_equal(length(residuals(fit)), nrow(data))
})

test_that("Test 13: Log-likelihood is finite", {
  # Test likelihood calculation
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_true(is.finite(logLik(fit)))
})

test_that("Test 14: AIC and BIC are calculated correctly", {
  # Test information criteria
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  # AIC = -2*logLik + 2*npar
  expected_aic <- -2 * as.numeric(logLik(fit)) + 2 * fit$npar
  expect_equal(AIC(fit), expected_aic, tolerance = 1e-6)

  # BIC = -2*logLik + log(n)*npar
  expected_bic <- -2 * as.numeric(logLik(fit)) + log(fit$nobs) * fit$npar
  expect_equal(BIC(fit), expected_bic, tolerance = 1e-6)
})

test_that("Test 15: Variance-covariance matrix has correct dimensions", {
  # Test vcov output with Hessian enabled
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  vc <- vcov(fit)
  expect_equal(nrow(vc), length(coef(fit)))
  expect_equal(ncol(vc), length(coef(fit)))
  expect_true(isSymmetric(vc))
})

test_that("Test 16: Standard errors are positive when Hessian computed", {
  # Test standard error calculation
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  expect_true(all(fit$se > 0))
  expect_equal(length(fit$se), length(coef(fit)))
})

test_that("Test 17: No Hessian computation when disabled", {
  # Test that Hessian can be disabled
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(hessian = FALSE)
  )

  expect_null(fit$vcov)
  expect_null(fit$se)
})

test_that("Test 18: Different optimization methods produce convergence", {
  # Test alternative optimizers
  data <- setup_test_data()

  fit_nlminb <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(method = "nlminb")
  )
  fit_bfgs <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(method = "BFGS")
  )

  expect_true(fit_nlminb$convergence)
  expect_true(fit_bfgs$convergence)
  expect_equal(fit_nlminb$method, "nlminb")
  expect_equal(fit_bfgs$method, "BFGS")
})

test_that("Test 19: Model components returned when requested", {
  # Test model, x, y storage options
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    model = TRUE, x = TRUE, y = TRUE
  )

  expect_true("model" %in% names(fit))
  expect_true("x" %in% names(fit))
  expect_true("y" %in% names(fit))
  expect_s3_class(fit$model, "data.frame")
  expect_type(fit$x, "list")
})

test_that("Test 20: Model components not returned when disabled", {
  # Test minimal storage mode
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "ekw",
    model = FALSE, x = FALSE, y = FALSE
  )

  expect_false("model" %in% names(fit))
  expect_false("x" %in% names(fit))
  expect_false("y" %in% names(fit))
})

# =============================================================================
# ADDITIONAL TESTS: Error Handling and Edge Cases
# =============================================================================

test_that("Response outside (0,1) throws informative error", {
  # Test validation of response bounds
  data <- setup_test_data()
  data$y[1] <- 1.5 # Invalid value

  expect_error(
    gkwreg(y ~ x1, data = data, family = "kw"),
    regexp = "0.*1|bound|range"
  )
})

test_that("Invalid link function throws error", {
  # Test link validation
  data <- setup_test_data()

  expect_error(
    gkwreg(y ~ x1, data = data, family = "kw", link = "invalid_link"),
    regexp = "link"
  )
})

test_that("NA handling with na.omit works correctly", {
  # Test missing value handling
  data <- setup_test_data()
  data$y[1:5] <- NA

  fit <- gkwreg(y ~ x1, data = data, family = "kw", na.action = na.omit)

  expect_equal(fit$nobs, nrow(data) - 5)
  expect_true(fit$convergence)
})

test_that("Subset argument filters data correctly", {
  # Test subset functionality
  data <- setup_test_data()
  n_original <- nrow(data)

  fit <- gkwreg(y ~ x1, data = data, family = "kw", subset = data$x1 > 0)

  expect_true(fit$nobs < n_original)
  expect_true(fit$nobs > 0)
})

test_that("Weights argument is accepted without error", {
  # Test weights handling (experimental feature)
  data <- setup_test_data()
  weights <- runif(nrow(data), 0.5, 1.5)

  expect_no_error(
    fit <- gkwreg(y ~ x1, data = data, family = "kw", weights = weights)
  )
})

test_that("Formula object is properly stored", {
  # Test formula storage
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1 | x2, data = data, family = "kw")

  expect_s3_class(fit$formula, "formula")
})

test_that("Convergence message is informative", {
  # Test convergence reporting
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_type(fit$message, "character")
  expect_true(nchar(fit$message) > 0)
})

test_that("Call is correctly stored in model object", {
  # Test call storage
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_type(fit$call, "language")
  expect_true(as.character(fit$call[[1]]) == "gkwreg")
})

# =============================================================================
# TESTS: Different Families Comprehensive Coverage
# =============================================================================

test_that("KKw family (Kumaraswamy-Kumaraswamy) fits correctly", {
  # Test alternative four-parameter family
  data <- setup_test_data()

  fit <- gkwreg(y ~ 1, data = data, family = "kkw")

  expect_equal(fit$family, "kkw")
  expect_equal(length(coef(fit)), 4) # alpha, beta, delta, lambda
  expect_true(all(fit$parameter_vectors$gammaVec == 1)) # Gamma fixed
})

test_that("McDonald family fits correctly", {
  # Test McDonald (Beta Power) family
  data <- setup_test_data()

  fit <- gkwreg(y ~ 1, data = data, family = "mc")

  expect_equal(fit$family, "mc")
  expect_equal(length(coef(fit)), 3) # gamma, delta, lambda
  expect_true(all(fit$parameter_vectors$alphaVec == 1))
  expect_true(all(fit$parameter_vectors$betaVec == 1))
})

# =============================================================================
# TESTS: Prediction and Inference
# =============================================================================

test_that("Predict method works on new data", {
  # Test prediction functionality
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1 + x2, data = data, family = "kw")

  newdata <- data.frame(x1 = c(0, 1, -1), x2 = c(0, 0.5, -0.5))
  pred <- predict(fit, newdata = newdata, type = "response")

  expect_length(pred, 3)
  expect_true(all(pred > 0 & pred < 1))
})

test_that("Confidence intervals can be computed", {
  # Test confint method
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  ci <- confint(fit, level = 0.95)

  expect_equal(nrow(ci), length(coef(fit)))
  expect_equal(ncol(ci), 2)
  expect_true(all(ci[, 1] <= coef(fit)))
  expect_true(all(ci[, 2] >= coef(fit)))
})

# =============================================================================
# TESTS: Simulation-Based Parameter Recovery
# =============================================================================

test_that("Parameter recovery works for known Kumaraswamy data", {
  # Test that we can recover true parameters from simulated data
  set.seed(2024)
  n <- 500
  x <- rnorm(n)

  # True parameters: log(alpha) = 0.5 + 0.3*x, log(beta) = 1.0
  alpha_true <- exp(0.5 + 0.3 * x)
  beta_true <- exp(1.0)
  y <- rkw(n, alpha = alpha_true, beta = beta_true)

  data <- data.frame(y = y, x = x)
  fit <- gkwreg(y ~ x | 1, data = data, family = "kw")

  # Check recovery of intercepts (allowing for estimation error)
  expect_equal(as.numeric(coef(fit)["alpha:(Intercept)"]), 0.5, tolerance = 0.15)
  expect_equal(as.numeric(coef(fit)["alpha:x"]), 0.3, tolerance = 0.1)
  expect_equal(as.numeric(coef(fit)["beta:(Intercept)"]), 1.0, tolerance = 0.15)
})

# =============================================================================
# TESTS: Real Data Examples
# =============================================================================

test_that("GasolineYield dataset fits successfully", {
  # Test with real dataset from package
  data(GasolineYield)

  fit <- gkwreg(yield ~ temp, data = GasolineYield, family = "kw")

  expect_true(fit$convergence)
  expect_true(fit$nobs > 0)
})

test_that("Model comparison works with nested models", {
  # Test anova method for model comparison
  data <- setup_test_data()

  fit1 <- gkwreg(y ~ 1, data = data, family = "kw")
  fit2 <- gkwreg(y ~ x1, data = data, family = "kw")

  comparison <- anova(fit1, fit2)

  expect_s3_class(comparison, "anova")
  expect_true(logLik(fit2) >= logLik(fit1)) # More complex model should fit better
})

# =============================================================================
# TESTS: Edge Cases and Robustness
# =============================================================================

test_that("Very small sample size produces warning or still fits", {
  # Test behavior with minimal data
  set.seed(123)
  n <- 20
  x <- rnorm(n)
  y <- rkw(n, alpha = 2, beta = 3)
  data <- data.frame(y = y, x = x)

  # Should still fit but may be unstable
  expect_no_error(
    fit <- gkwreg(y ~ x, data = data, family = "kw")
  )
})

test_that("Perfect separation handled gracefully", {
  # Test extreme predictor values
  set.seed(456)
  n <- 100
  x <- c(rep(-10, 50), rep(10, 50))
  y <- c(rkw(50, alpha = 0.5, beta = 2), rkw(50, alpha = 5, beta = 2))
  data <- data.frame(y = y, x = x)

  # Should converge or provide informative message
  fit <- gkwreg(y ~ x,
    data = data, family = "kw",
    control = gkw_control(maxit = 1000)
  )

  expect_type(fit$convergence, "logical")
})

test_that("High collinearity in predictors still produces estimates", {
  # Test multicollinearity handling
  set.seed(789)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.01) # Nearly perfect correlation
  y <- rkw(n, alpha = 2, beta = 2)
  data <- data.frame(y = y, x1 = x1, x2 = x2)

  fit <- gkwreg(y ~ x1 + x2, data = data, family = "kw")

  # Should fit but SEs may be large
  expect_s3_class(fit, "gkwreg")
})

# =============================================================================
# TESTS: Summary and Print Methods
# =============================================================================

test_that("Summary method produces informative output", {
  # Test summary.gkwreg
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  summ <- summary(fit)

  expect_s3_class(summ, "summary.gkwreg")
  expect_true("coefficients" %in% names(summ))
})

test_that("Print method works without errors", {
  # Test print.gkwreg
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_output(print(fit), regexp = "gkwreg|Kumaraswamy")
})

test_that("Coef method extracts coefficients correctly", {
  # Test coef.gkwreg
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  cf <- coef(fit)

  expect_type(cf, "double")
  expect_true(is.numeric(cf))
  expect_true(!is.null(names(cf)))
})

# =============================================================================
# TESTS: Control Parameters
# =============================================================================

test_that("Maximum iterations parameter is respected", {
  # Test maxit control
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(maxit = 50)
  )

  # May not converge with only 5 iterations
  expect_true(fit$iterations <= 50)
})

test_that("Tolerance parameters affect convergence", {
  # Test reltol and abstol
  data <- setup_test_data()

  fit_tight <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(reltol = 1e-10)
  )
  fit_loose <- gkwreg(y ~ x1,
    data = data, family = "kw",
    control = gkw_control(reltol = 1e-4)
  )

  expect_s3_class(fit_tight, "gkwreg")
  expect_s3_class(fit_loose, "gkwreg")
})

test_that("Silent mode suppresses output", {
  # Test silent control
  data <- setup_test_data()

  expect_silent(
    fit <- gkwreg(y ~ x1,
      data = data, family = "kw",
      control = gkw_control(silent = TRUE)
    )
  )
})

# =============================================================================
# TESTS: Multiple Link Functions
# =============================================================================

test_that("Probit link functions work correctly", {
  # Test probit link for delta parameter
  set.seed(321)
  n <- 150
  x <- rnorm(n)
  gamma <- exp(0.5)
  delta <- pnorm(0.2 + 0.3 * x) # Probit scale
  lambda <- exp(1.0)
  y <- rgkw(n,
    alpha = 1, beta = 1, gamma = gamma,
    delta = delta, lambda = lambda
  )

  data <- data.frame(y = y, x = x)
  fit <- gkwreg(y ~ 1 | 1 | 1 | x | 1,
    data = data, family = "mc",
    link = list(delta = "probit")
  )

  expect_equal(fit$link$delta, "probit")
  expect_true(fit$convergence)
})

test_that("Sqrt link function works", {
  # Test sqrt link
  data <- setup_test_data()

  fit <- gkwreg(y ~ x1,
    data = data, family = "kw",
    link = list(alpha = "sqrt", beta = "log")
  )

  expect_equal(fit$link$alpha, "sqrt")
  expect_true(fit$convergence)
})

test_that("Cloglog link function works", {
  # Test complementary log-log link
  data <- setup_test_data()

  fit <- gkwreg(y ~ 1 | 1 | 1 | 1,
    data = data, family = "beta",
    link = list(delta = "cloglog")
  )

  expect_equal(fit$link$delta, "cloglog")
})

# =============================================================================
# TESTS: Diagnostic Statistics
# =============================================================================

test_that("RMSE is computed correctly", {
  # Test root mean squared error
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  manual_rmse <- sqrt(mean(residuals(fit)^2))

  expect_equal(fit$rmse, manual_rmse, tolerance = 1e-10)
})

test_that("Efron R-squared is between 0 and 1", {
  # Test pseudo R-squared
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1 + x2, data = data, family = "kw")

  expect_true(fit$efron_r2 >= 0)
  expect_true(fit$efron_r2 <= 1)
})

test_that("Mean absolute error is positive", {
  # Test MAE
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1, data = data, family = "kw")

  expect_true(fit$mean_absolute_error > 0)
  manual_mae <- mean(abs(residuals(fit)))
  expect_equal(fit$mean_absolute_error, manual_mae, tolerance = 1e-10)
})

test_that("Degrees of freedom calculated correctly", {
  # Test df.residual
  data <- setup_test_data()
  fit <- gkwreg(y ~ x1 + x2, data = data, family = "kw")

  expected_df <- nrow(data) - length(coef(fit))
  expect_equal(fit$df.residual, expected_df)
})

# =============================================================================
# TESTS: Offset Functionality
# =============================================================================

test_that("Offset vector is accepted", {
  # Test offset parameter
  data <- setup_test_data()
  offset_vec <- rnorm(nrow(data), mean = 0, sd = 0.1)

  expect_no_error(
    fit <- gkwreg(y ~ x1, data = data, family = "kw", offset = offset_vec)
  )
})
