# Test Suite for S3 Methods of gkwreg Objects
# Author: Lopes, J. E.
# Description: Comprehensive tests covering all S3 methods: print, coef, summary,
#              print.summary, vcov, nobs, and confint for gkwreg objects

library(testthat)
library(gkwreg)
library(gkwdist)

# Setup: Generate test data and fit model
setup_methods_data <- function() {
  set.seed(47856)
  n <- 120
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)
  x3 <- factor(sample(c("A", "B"), n, replace = TRUE))

  # Generate Kumaraswamy response
  alpha <- exp(0.65 + 0.28 * x1)
  beta <- exp(1.15 - 0.18 * x2)
  y <- rkw(n, alpha = alpha, beta = beta)

  # Ensure strictly in (0, 1)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  # Fit model with Hessian
  fit <- gkwreg(y ~ x1 | x2,
    data = data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  list(data = data, fit = fit, y = y)
}

# =============================================================================
# TESTS FOR print.gkwreg (3 tests)
# =============================================================================

test_that("Test 1: print.gkwreg produces output without errors", {
  # Test that print method executes successfully
  setup <- setup_methods_data()

  expect_output(print(setup$fit), regexp = "Call")
  expect_output(print(setup$fit), regexp = "Coefficients")
})

test_that("Test 2: print.gkwreg respects digits argument", {
  # Test that digits parameter controls output precision
  setup <- setup_methods_data()

  # Should work with different digit specifications
  expect_no_error(print(setup$fit, digits = 2))
  expect_no_error(print(setup$fit, digits = 5))
  expect_no_error(print(setup$fit, digits = 8))
})

test_that("Test 3: print.gkwreg returns object invisibly", {
  # Test that print returns the object invisibly
  setup <- setup_methods_data()

  result <- withVisible(print(setup$fit))

  expect_false(result$visible)
  expect_identical(result$value, setup$fit)
  expect_s3_class(result$value, "gkwreg")
})

# =============================================================================
# TESTS FOR coef.gkwreg (2 tests)
# =============================================================================

test_that("Test 4: coef.gkwreg extracts coefficients correctly", {
  # Test that coef method returns named numeric vector
  setup <- setup_methods_data()

  coefs <- coef(setup$fit)

  expect_type(coefs, "double")
  expect_true(is.numeric(coefs))
  expect_true(!is.null(names(coefs)))
  expect_true(all(is.finite(coefs)))

  # Should have coefficients for both parameters
  expect_true(any(grepl("alpha", names(coefs))))
  expect_true(any(grepl("beta", names(coefs))))
})

test_that("Test 5: coef.gkwreg matches stored coefficients component", {
  # Test that coef() returns same as direct access
  setup <- setup_methods_data()

  coefs_method <- coef(setup$fit)
  coefs_direct <- setup$fit$coefficients

  expect_identical(coefs_method, coefs_direct)
  expect_equal(length(coefs_method), setup$fit$npar)
})

# =============================================================================
# TESTS FOR summary.gkwreg (4 tests)
# =============================================================================

test_that("Test 6: summary.gkwreg returns proper summary object", {
  # Test that summary method creates summary.gkwreg object
  setup <- setup_methods_data()

  summ <- summary(setup$fit)

  expect_s3_class(summ, "summary.gkwreg")
  expect_type(summ, "list")

  # Check essential components
  expect_true("call" %in% names(summ))
  expect_true("family" %in% names(summ))
  expect_true("coefficients" %in% names(summ))
  expect_true("link" %in% names(summ))
})

test_that("Test 7: summary.gkwreg computes z-values and p-values", {
  # Test that summary calculates test statistics
  setup <- setup_methods_data()

  summ <- summary(setup$fit)
  coef_table <- summ$coefficients

  expect_true("Estimate" %in% colnames(coef_table))
  expect_true("Std. Error" %in% colnames(coef_table))
  expect_true("z value" %in% colnames(coef_table))
  expect_true("Pr(>|z|)" %in% colnames(coef_table))

  # All p-values should be between 0 and 1
  p_vals <- coef_table[, "Pr(>|z|)"]
  expect_true(all(p_vals >= 0 & p_vals <= 1))
})

test_that("Test 8: summary.gkwreg computes confidence intervals", {
  # Test that confidence intervals are calculated
  setup <- setup_methods_data()

  summ_95 <- summary(setup$fit, conf.level = 0.95)
  summ_90 <- summary(setup$fit, conf.level = 0.90)

  expect_true(!is.null(summ_95$conf.int))
  expect_true(!is.null(summ_90$conf.int))

  # 95% CI should be wider than 90% CI
  width_95 <- summ_95$conf.int[1, 2] - summ_95$conf.int[1, 1]
  width_90 <- summ_90$conf.int[1, 2] - summ_90$conf.int[1, 1]
  expect_true(width_95 > width_90)
})

test_that("Test 9: summary.gkwreg includes residual summary", {
  # Test that summary includes residual statistics
  setup <- setup_methods_data()

  summ <- summary(setup$fit)

  expect_true(!is.null(summ$residuals))
  expect_true("Min" %in% names(summ$residuals))
  expect_true("Max" %in% names(summ$residuals))
  expect_true("Median" %in% names(summ$residuals))
  expect_true("Mean" %in% names(summ$residuals))
})

# =============================================================================
# TESTS FOR print.summary.gkwreg (2 tests)
# =============================================================================

test_that("Test 10: print.summary.gkwreg displays formatted output", {
  # Test that summary print method produces readable output
  setup <- setup_methods_data()

  summ <- summary(setup$fit)

  expect_output(print(summ), regexp = "Generalized Kumaraswamy")
  expect_output(print(summ), regexp = "Family")
  expect_output(print(summ), regexp = "Coefficients")
  expect_output(print(summ), regexp = "Estimate")
})

test_that("Test 11: print.summary.gkwreg respects signif.stars argument", {
  # Test that significance stars can be toggled
  setup <- setup_methods_data()

  summ <- summary(setup$fit)

  # Should work with stars
  expect_no_error(print(summ, signif.stars = TRUE))

  # Should work without stars
  expect_no_error(print(summ, signif.stars = FALSE))
})

# =============================================================================
# TESTS FOR vcov.gkwreg (3 tests)
# =============================================================================

test_that("Test 12: vcov.gkwreg returns variance-covariance matrix", {
  # Test that vcov extracts matrix correctly
  setup <- setup_methods_data()

  vc <- vcov(setup$fit)

  expect_true(is.matrix(vc))
  expect_equal(nrow(vc), length(coef(setup$fit)))
  expect_equal(ncol(vc), length(coef(setup$fit)))
  expect_true(isSymmetric(vc))
})

test_that("Test 13: vcov.gkwreg diagonal matches squared standard errors", {
  # Test that variance matrix diagonal equals SE^2
  setup <- setup_methods_data()

  vc <- vcov(setup$fit)
  se <- setup$fit$se

  expect_equal(sqrt(diag(vc)), se, tolerance = 1e-10)
  expect_equal(diag(vc), se^2, tolerance = 1e-10)
})

test_that("Test 14: vcov.gkwreg warns when Hessian not computed", {
  # Test behavior when variance-covariance not available
  setup <- setup_methods_data()

  # Fit without Hessian
  fit_no_hess <- gkwreg(y ~ x1,
    data = setup$data, family = "kw",
    control = gkw_control(hessian = FALSE)
  )

  expect_warning(vcov(fit_no_hess), regexp = "Variance-covariance matrix not found")
  expect_null(suppressWarnings(vcov(fit_no_hess)))
})

# =============================================================================
# TESTS FOR nobs.gkwreg (2 tests)
# =============================================================================

test_that("Test 15: nobs.gkwreg returns correct number of observations", {
  # Test that nobs returns observation count
  setup <- setup_methods_data()

  n <- nobs(setup$fit)

  expect_type(n, "integer")
  expect_equal(n, nrow(setup$data))
  expect_equal(n, setup$fit$nobs)
})

test_that("Test 16: nobs.gkwreg accounts for subset and NA handling", {
  # Test nobs with subset and missing data
  setup <- setup_methods_data()

  # Fit with subset
  subset_idx <- setup$data$x1 > 0
  fit_subset <- gkwreg(y ~ x1,
    data = setup$data, family = "kw",
    subset = subset_idx
  )

  n_subset <- nobs(fit_subset)
  expect_equal(n_subset, sum(subset_idx))

  # Fit with NA (after removing some observations)
  data_na <- setup$data
  data_na$y[1:5] <- NA
  fit_na <- gkwreg(y ~ x1,
    data = data_na, family = "kw",
    na.action = na.omit
  )

  n_na <- nobs(fit_na)
  expect_equal(n_na, nrow(setup$data) - 5)
})

# =============================================================================
# TESTS FOR confint.gkwreg (4 tests)
# =============================================================================

test_that("Test 17: confint.gkwreg returns confidence intervals", {
  # Test that confint computes intervals correctly
  setup <- setup_methods_data()

  ci <- confint(setup$fit)

  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), length(coef(setup$fit)))
  expect_equal(ncol(ci), 2)

  # Lower bound should be less than upper bound
  expect_true(all(ci[, 1] < ci[, 2]))
})

test_that("Test 18: confint.gkwreg respects level argument", {
  # Test that confidence level controls interval width
  setup <- setup_methods_data()

  ci_95 <- confint(setup$fit, level = 0.95)
  ci_90 <- confint(setup$fit, level = 0.90)
  ci_99 <- confint(setup$fit, level = 0.99)

  # Wider confidence level = wider intervals
  width_90 <- ci_90[1, 2] - ci_90[1, 1]
  width_95 <- ci_95[1, 2] - ci_95[1, 1]
  width_99 <- ci_99[1, 2] - ci_99[1, 1]

  expect_true(width_90 < width_95)
  expect_true(width_95 < width_99)
})

test_that("Test 19: confint.gkwreg allows parameter selection", {
  # Test selecting specific parameters
  setup <- setup_methods_data()

  coef_names <- names(coef(setup$fit))

  # Select by name
  ci_name <- confint(setup$fit, parm = coef_names[1])
  expect_equal(nrow(ci_name), 1)
  expect_equal(rownames(ci_name), coef_names[1])

  # Select by index
  ci_idx <- confint(setup$fit, parm = 1:2)
  expect_equal(nrow(ci_idx), 2)

  # Select multiple by name
  ci_multi <- confint(setup$fit, parm = coef_names[1:3])
  expect_equal(nrow(ci_multi), 3)
})

test_that("Test 20: confint.gkwreg intervals contain point estimates", {
  # Test that confidence intervals contain the coefficient estimates
  setup <- setup_methods_data()

  ci <- confint(setup$fit, level = 0.95)
  coefs <- coef(setup$fit)

  # Each coefficient should fall within its CI
  for (i in seq_along(coefs)) {
    expect_true(coefs[i] >= ci[i, 1])
    expect_true(coefs[i] <= ci[i, 2])
  }
})

# =============================================================================
# ADDITIONAL INTEGRATION TESTS
# =============================================================================

test_that("All methods work together coherently", {
  # Integration test: all methods work on same object
  setup <- setup_methods_data()

  # All methods should work without error
  expect_no_error(print(setup$fit))
  expect_no_error(coef(setup$fit))
  expect_no_error(summary(setup$fit))
  expect_no_error(vcov(setup$fit))
  expect_no_error(nobs(setup$fit))
  expect_no_error(confint(setup$fit))
})

test_that("Methods work across different families", {
  # Test that all methods work for different distribution families
  setup <- setup_methods_data()

  families <- c("kw", "beta", "ekw")

  for (fam in families) {
    fit <- gkwreg(y ~ x1,
      data = setup$data, family = fam,
      control = gkw_control(hessian = TRUE)
    )

    expect_no_error(coef(fit))
    expect_no_error(nobs(fit))
    expect_no_error(vcov(fit))
    expect_no_error(summary(fit))
    expect_no_error(confint(fit))
  }
})

test_that("Methods handle edge cases gracefully", {
  # Test methods with minimal model
  setup <- setup_methods_data()

  fit_minimal <- gkwreg(y ~ 1,
    data = setup$data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  expect_equal(nobs(fit_minimal), nrow(setup$data))
  expect_equal(length(coef(fit_minimal)), 2) # Two intercepts
  expect_no_error(summary(fit_minimal))
  expect_no_error(confint(fit_minimal))
})


test_that("summary handles models without Hessian", {
  # Test summary when SE not available
  setup <- setup_methods_data()

  fit_no_hess <- gkwreg(y ~ x1,
    data = setup$data, family = "kw",
    control = gkw_control(hessian = FALSE)
  )

  summ <- summary(fit_no_hess)

  # Should still work but with limited info
  expect_s3_class(summ, "summary.gkwreg")
  expect_true("Estimate" %in% colnames(summ$coefficients))

  # Should not have SE, z, p-value columns
  expect_false("Std. Error" %in% colnames(summ$coefficients))
})

test_that("Methods handle factor predictors correctly", {
  # Test methods with categorical variables
  setup <- setup_methods_data()

  fit_factor <- gkwreg(y ~ x3,
    data = setup$data, family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  coefs <- coef(fit_factor)
  expect_true(any(grepl("x3", names(coefs))))

  ci <- confint(fit_factor)
  expect_equal(nrow(ci), length(coefs))

  summ <- summary(fit_factor)
  expect_s3_class(summ, "summary.gkwreg")
})

test_that("confint parameter selection errors for invalid input", {
  # Test error handling in confint
  setup <- setup_methods_data()

  # Invalid parameter name
  expect_error(confint(setup$fit, parm = "nonexistent"),
    regexp = "parameter.*not found"
  )

  # Out of range index
  expect_error(confint(setup$fit, parm = 999),
    regexp = "out of range"
  )

  # Invalid level
  expect_error(confint(setup$fit, level = 1.5),
    regexp = "level.*between 0 and 1"
  )
  expect_error(confint(setup$fit, level = -0.1),
    regexp = "level.*between 0 and 1"
  )
})

test_that("print methods handle large coefficient names properly", {
  # Test display with complex formulas
  setup <- setup_methods_data()

  fit_complex <- gkwreg(y ~ x1 * x2 + I(x1^2),
    data = setup$data,
    family = "kw"
  )

  expect_output(print(fit_complex), regexp = "Coefficients")

  summ <- summary(fit_complex)
  expect_output(print(summ), regexp = "Estimate")
})

test_that("Summary statistics are mathematically consistent", {
  # Test internal consistency of summary output
  setup <- setup_methods_data()

  summ <- summary(setup$fit)

  # nobs should match
  expect_equal(summ$nobs, nobs(setup$fit))

  # Coefficients should match
  expect_equal(summ$coefficients[, "Estimate"], as.numeric(coef(setup$fit)))

  # Standard errors should match vcov diagonal
  vc <- vcov(setup$fit)
  expect_equal(summ$coefficients[, "Std. Error"], as.numeric(sqrt(diag(vc))))
})
