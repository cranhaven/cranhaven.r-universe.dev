# Test Suite for anova.gkwreg Method
# Author: Lopes, J. E.
# Description: Comprehensive tests for the anova() method for gkwreg objects
#              covering single models, nested model comparisons, likelihood ratio tests,
#              and edge cases

library(testthat)
library(gkwreg)
library(gkwdist)

# Setup: Generate test data and fit models
setup_anova_models <- function() {
  set.seed(98765)
  n <- 1500
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)
  x3 <- rnorm(n, mean = 1, sd = 0.5)

  # Generate Kumaraswamy response
  alpha <- exp(0.5 + 0.2 * x1)
  beta <- exp(1.2 - 0.15 * x2)
  y <- rkw(n, alpha = alpha, beta = beta)

  data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  # Fit nested sequence of models
  fit0 <- gkwreg(y ~ 1, data = data, family = "kw")
  fit1 <- gkwreg(y ~ x1, data = data, family = "kw")
  fit2 <- gkwreg(y ~ x1 + x2, data = data, family = "kw")
  fit3 <- gkwreg(y ~ x1 + x2 + x3, data = data, family = "kw")

  list(data = data, fit0 = fit0, fit1 = fit1, fit2 = fit2, fit3 = fit3)
}

models <- setup_anova_models()

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Test 1: Single model anova returns correct structure", {
  # Test anova on single model produces proper output
  # models <- setup_anova_models()

  result <- anova(models$fit1)

  expect_s3_class(result, c("anova.gkwreg", "anova", "data.frame"))
  expect_true("Resid. Df" %in% names(result))
  expect_true("Resid. Dev" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("Test 2: Single model shows correct degrees of freedom", {
  # Test that residual df matches model specification
  # models <- setup_anova_models()

  result <- anova(models$fit1)

  expected_df <- models$fit1$df.residual
  expect_equal(result[1, "Resid. Df"], expected_df)
})

test_that("Test 3: Single model shows correct deviance", {
  # Test deviance calculation for single model
  # models <- setup_anova_models()

  result <- anova(models$fit1)

  expected_dev <- -2 * as.numeric(logLik(models$fit1))
  expect_equal(result[1, "Resid. Dev"], expected_dev, tolerance = 1e-6)
})

test_that("Test 4: Two nested models produce comparison", {
  # Test basic two-model comparison
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1)

  expect_equal(nrow(result), 2)
  expect_true("Df" %in% names(result))
  expect_true("Deviance" %in% names(result))
  expect_true(!is.na(result[2, "Df"]))
})

test_that("Test 5: Multiple nested models all appear in output", {
  # Test three-model comparison
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2)

  expect_equal(nrow(result), 3)
  expect_equal(sum(!is.na(result$Df)), 2) # Two comparisons
})

# =============================================================================
# LIKELIHOOD RATIO TEST STATISTICS
# =============================================================================

test_that("Test 6: LRT statistic calculated correctly", {
  # Test likelihood ratio test statistic computation
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, test = "Chisq")

  # Manual calculation
  ll0 <- as.numeric(logLik(models$fit0))
  ll1 <- as.numeric(logLik(models$fit1))
  expected_lrt <- -2 * (ll0 - ll1)

  expect_equal(result[2, "Deviance"], expected_lrt, tolerance = 1e-6)
})

test_that("Test 7: LRT statistic is positive for better fitting model", {
  # Test that more complex model has better fit (positive LRT)
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit2, test = "Chisq")

  expect_true(result[2, "Deviance"] > 0)
})

test_that("Test 8: Degrees of freedom difference calculated correctly", {
  # Test df calculation between models
  # models <- setup_anova_models()

  result <- anova(models$fit1, models$fit2)

  df_diff <- models$fit1$npar - models$fit2$npar
  expect_equal(result[2, "Df"], -df_diff)
})

test_that("Test 9: P-values are between 0 and 1", {
  # Test p-value validity
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2, test = "Chisq")

  p_values <- result$`Pr(>Chi)`[!is.na(result$`Pr(>Chi)`)]
  expect_true(all(p_values >= 0 & p_values <= 1))
})

test_that("Test 10: P-values match manual chi-squared calculation", {
  # Test p-value computation accuracy
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, test = "Chisq")

  # Manual p-value
  lrt_stat <- result[2, "Deviance"]
  df_diff <- abs(result[2, "Df"])
  expected_pval <- pchisq(lrt_stat, df = df_diff, lower.tail = FALSE)

  expect_equal(result[2, "Pr(>Chi)"], expected_pval, tolerance = 1e-8)
})

# =============================================================================
# TEST ARGUMENT VARIATIONS
# =============================================================================

test_that("Test 11: test='none' suppresses p-values", {
  # Test that test='none' removes hypothesis testing
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, test = "none")

  expect_false("Pr(>Chi)" %in% names(result))
})

test_that("Test 12: test='Chisq' includes p-values", {
  # Test that test='Chisq' includes chi-squared test
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, test = "Chisq")

  expect_true("Pr(>Chi)" %in% names(result))
  expect_false(is.na(result[2, "Pr(>Chi)"]))
})

test_that("Test 13: Default test is Chisq", {
  # Test default behavior includes chi-squared test
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1)

  expect_true("Pr(>Chi)" %in% names(result))
})

# =============================================================================
# MODEL ORDERING
# =============================================================================

test_that("Test 14: Models automatically ordered by complexity", {
  # Test that models are sorted by degrees of freedom
  # models <- setup_anova_models()

  # Provide models in reverse order
  result <- anova(models$fit3, models$fit0, models$fit2)

  # Should be ordered from simplest to most complex
  expect_true(all(diff(result$`Resid. Df`) <= 0))
})

test_that("Test 15: Reversed model order produces same results", {
  # Test that order of input doesn't matter
  # models <- setup_anova_models()

  result1 <- anova(models$fit0, models$fit1, models$fit2)
  result2 <- anova(models$fit2, models$fit1, models$fit0)

  expect_equal(result1$`Resid. Df`, result2$`Resid. Df`)
  expect_equal(result1$`Resid. Dev`, result2$`Resid. Dev`)
})

# =============================================================================
# DIFFERENT FAMILIES
# =============================================================================

test_that("Test 16: Nested families comparison works (kw vs ekw)", {
  # Test comparison between Kumaraswamy and Exponentiated Kumaraswamy
  # models <- setup_anova_models()

  fit_kw <- gkwreg(y ~ x1, data = models$data, family = "kw")
  fit_ekw <- gkwreg(y ~ x1, data = models$data, family = "ekw")

  result <- anova(fit_kw, fit_ekw)

  expect_equal(nrow(result), 2)
  expect_true(result[2, "Deviance"] >= 0) # ekw should fit better or equal
})

test_that("Test 17: Nested families comparison (beta vs mc)", {
  # Test Beta vs McDonald comparison
  # models <- setup_anova_models()

  fit_beta <- gkwreg(y ~ x1, data = models$data, family = "beta")
  fit_mc <- gkwreg(y ~ x1, data = models$data, family = "mc")

  result <- anova(fit_beta, fit_mc)

  expect_s3_class(result, "anova.gkwreg")
  expect_equal(nrow(result), 2)
})

# =============================================================================
# DIFFERENT FORMULA SPECIFICATIONS
# =============================================================================

test_that("Test 19: Different predictors per parameter are compared", {
  # Test models with different parameter specifications
  # models <- setup_anova_models()

  fit_same <- gkwreg(y ~ x1, data = models$data, family = "kw")
  fit_diff <- gkwreg(y ~ x1 | x2, data = models$data, family = "kw")

  result <- anova(fit_same, fit_diff)

  expect_equal(nrow(result), 2)
  expect_true(abs(result[2, "Df"]) > 0)
})

test_that("Test 20: Intercept-only vs predictors comparison", {
  # Test adding predictors to intercept-only model
  # models <- setup_anova_models()

  fit_int <- gkwreg(y ~ 1 | 1, data = models$data, family = "kw")
  fit_pred <- gkwreg(y ~ x1 | x2, data = models$data, family = "kw")

  result <- anova(fit_int, fit_pred, test = "Chisq")

  expect_true(result[2, "Pr(>Chi)"] >= 0)
  expect_true(result[2, "Deviance"] > 0)
})

test_that("Test 21: Interaction terms comparison", {
  # Test models with and without interactions
  # models <- setup_anova_models()

  fit_main <- gkwreg(y ~ x1 + x2, data = models$data, family = "kw")
  fit_inter <- gkwreg(y ~ x1 * x2, data = models$data, family = "kw")

  result <- anova(fit_main, fit_inter)

  expect_equal(nrow(result), 2)
  # Interaction model should have more parameters
  expect_true(result[2, "Resid. Df"] < result[1, "Resid. Df"])
})

# =============================================================================
# OUTPUT STRUCTURE VALIDATION
# =============================================================================

test_that("Test 22: Output has correct class attributes", {
  # Test return object class
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1)

  expect_s3_class(result, "anova.gkwreg")
  expect_s3_class(result, "anova")
  expect_s3_class(result, "data.frame")
})

test_that("Test 23: All required columns present", {
  # Test that all expected columns exist
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, test = "Chisq")

  required_cols <- c("Resid. Df", "Resid. Dev", "Df", "Deviance", "Pr(>Chi)")
  expect_true(all(required_cols %in% names(result)))
})

test_that("Test 24: Row names are informative", {
  # Test that rows have meaningful names
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2)

  expect_true(!is.null(rownames(result)))
  expect_equal(length(rownames(result)), 3)
})

test_that("Test 25: First row has NA for comparison columns", {
  # Test that baseline model has NA for Df and Deviance
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1)

  expect_true(is.na(result[1, "Df"]))
  expect_true(is.na(result[1, "Deviance"]))
})

# =============================================================================
# EDGE CASES AND ERROR HANDLING
# =============================================================================

test_that("Test 26: Identical models produce zero deviance difference", {
  # Test comparing identical models
  # models <- setup_anova_models()

  # Fit same model twice
  fit_a <- gkwreg(y ~ x1, data = models$data, family = "kw")
  fit_b <- gkwreg(y ~ x1, data = models$data, family = "kw")

  result <- anova(fit_a, fit_b)

  expect_equal(result[2, "Deviance"], 0, tolerance = 1e-10)
  expect_equal(result[2, "Df"], 0)
})

test_that("Test 27: Single model with test='Chisq' doesn't error", {
  # Test that test argument doesn't break single model anova
  # models <- setup_anova_models()

  expect_no_error(
    result <- anova(models$fit1, test = "Chisq")
  )
})

test_that("Test 28: Non-nested models still produce output", {
  # Test non-nested models (e.g., different predictors)
  # models <- setup_anova_models()

  fit_x1 <- gkwreg(y ~ x1, data = models$data, family = "kw")
  fit_x2 <- gkwreg(y ~ x2, data = models$data, family = "kw")

  # Should work but interpretation may be questionable
  expect_no_error(
    result <- anova(fit_x1, fit_x2)
  )
})

test_that("Test 29: Very similar models produce small deviance difference", {
  # Test numerical stability with nearly identical models
  # models <- setup_anova_models()

  # Add tiny predictor effect
  models$data$x_tiny <- rnorm(nrow(models$data), sd = 0.0001)

  fit_base <- gkwreg(y ~ x1, data = models$data, family = "kw")
  fit_tiny <- gkwreg(y ~ x1 + x_tiny, data = models$data, family = "kw")

  result <- anova(fit_base, fit_tiny)

  expect_true(is.finite(result[2, "Deviance"]))
  expect_true(result[2, "Deviance"] >= 0)
})

# =============================================================================
# STATISTICAL VALIDITY TESTS
# =============================================================================

test_that("Test 30: Significant predictor produces small p-value", {
  # Test that truly important predictor is detected
  set.seed(2468)
  n <- 200
  x_important <- rnorm(n)
  alpha <- exp(0.5 + 0.5 * x_important) # Strong effect
  beta <- exp(1.0)
  y <- rkw(n, alpha = alpha, beta = beta)
  data <- data.frame(y = y, x_important = x_important)

  fit0 <- gkwreg(y ~ 1, data = data, family = "kw")
  fit1 <- gkwreg(y ~ x_important, data = data, family = "kw")

  result <- anova(fit0, fit1, test = "Chisq")

  expect_true(result[2, "Pr(>Chi)"] < 0.05)
})

test_that("Test 31: Irrelevant predictor produces large p-value", {
  # Test that noise predictor is not significant
  set.seed(3690)
  n <- 150
  x_noise <- rnorm(n)
  y <- rkw(n, alpha = 2, beta = 2) # No relationship with x
  data <- data.frame(y = y, x_noise = x_noise)

  fit0 <- gkwreg(y ~ 1, data = data, family = "kw")
  fit1 <- gkwreg(y ~ x_noise, data = data, family = "kw")

  result <- anova(fit0, fit1, test = "Chisq")

  expect_true(result[2, "Pr(>Chi)"] > 0.10)
})

test_that("Test 32: Deviance decreases with model complexity", {
  # Test that more complex models have lower (more negative) deviance
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2, models$fit3)

  # Residual deviance should decrease (or stay same) with complexity
  expect_true(all(diff(result$`Resid. Dev`) <= 0))
})

test_that("Test 33: Sequential tests all have correct df", {
  # Test degrees of freedom in sequential comparisons
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2)

  df_changes <- result$Df[!is.na(result$Df)]
  expect_true(all(df_changes != 0))
})

# =============================================================================
# REAL DATA EXAMPLES
# =============================================================================

test_that("Test 34: GasolineYield dataset produces valid anova", {
  # Test with real dataset from betareg package
  data(GasolineYield)

  fit1 <- gkwreg(yield ~ 1, data = GasolineYield, family = "kw")
  fit2 <- gkwreg(yield ~ temp, data = GasolineYield, family = "kw")

  result <- anova(fit1, fit2, test = "Chisq")

  expect_s3_class(result, "anova.gkwreg")
  expect_equal(nrow(result), 2)
  expect_true(all(is.finite(result$`Resid. Dev`)))
})

# =============================================================================
# MULTIPLE MODEL COMPARISONS
# =============================================================================

test_that("Test 35: Four nested models compared correctly", {
  # Test with four models in sequence
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2, models$fit3)

  expect_equal(nrow(result), 4)
  expect_equal(sum(!is.na(result$Df)), 3) # Three pairwise comparisons
})

test_that("Test 36: All pairwise comparisons have positive LRT", {
  # Test that each step improves fit
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2, test = "Chisq")

  deviance_changes <- result$Deviance[!is.na(result$Deviance)]
  expect_true(all(deviance_changes >= 0))
})

# =============================================================================
# PRINT AND DISPLAY TESTS
# =============================================================================

test_that("Test 37: Print method works without error", {
  # Test that anova object can be printed
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1)

  expect_output(print(result), regexp = "Resid")
})

test_that("Test 38: Summary statistics are all finite", {
  # Test numerical validity of all output
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, models$fit2)

  numeric_cols <- sapply(result, is.numeric)
  numeric_data <- as.matrix(result[, numeric_cols])

  expect_true(all(is.finite(numeric_data[!is.na(numeric_data)])))
})

# =============================================================================
# COMPARISON WITH EXTERNAL FUNCTIONS
# =============================================================================

test_that("Test 39: Results consistent with manual LRT calculation", {
  # Test that anova results match manual likelihood ratio test
  # models <- setup_anova_models()

  result <- anova(models$fit0, models$fit1, test = "Chisq")

  # Manual calculation
  ll0 <- as.numeric(logLik(models$fit0))
  ll1 <- as.numeric(logLik(models$fit1))
  manual_lrt <- 2 * (ll1 - ll0)
  df_diff <- models$fit1$npar - models$fit0$npar
  manual_pval <- pchisq(manual_lrt, df = df_diff, lower.tail = FALSE)

  expect_equal(result[2, "Deviance"], manual_lrt, tolerance = 1e-6)
  expect_equal(result[2, "Pr(>Chi)"], manual_pval, tolerance = 1e-8)
})

# =============================================================================
# SPECIAL CASES WITH DIFFERENT LINK FUNCTIONS
# =============================================================================

test_that("Test 40: Models with different link functions compared", {
  # Test anova with different link specifications
  # models <- setup_anova_models()

  fit_log <- gkwreg(y ~ x1,
    data = models$data, family = "kw",
    link = "log"
  )
  fit_sqrt <- gkwreg(y ~ x1,
    data = models$data, family = "kw",
    link = list(alpha = "sqrt", beta = "log")
  )

  # Should work (though models may not be nested in strict sense)
  expect_no_error(
    result <- anova(fit_log, fit_sqrt)
  )
})

test_that("Test 41: Same family, different complexity levels", {
  # Test systematic comparison within same family
  # models <- setup_anova_models()

  beta0 <- gkwreg(y ~ 1, data = models$data, family = "beta")
  beta1 <- gkwreg(y ~ x1, data = models$data, family = "beta")
  beta2 <- gkwreg(y ~ x1 + x2, data = models$data, family = "beta")

  result <- anova(beta0, beta1, beta2, test = "Chisq")

  expect_equal(nrow(result), 3)
  expect_true(all(result$`Resid. Dev`[1] >= result$`Resid. Dev`[3]))
})

# =============================================================================
# NEGATIVE TESTS (Expected Failures)
# =============================================================================

test_that("Test 42: Non-gkwreg object produces error", {
  # Test that function requires gkwreg objects
  # models <- setup_anova_models()

  fake_model <- list(loglik = -100, npar = 3)

  expect_error(
    anova(models$fit0, fake_model),
    regexp = "gkwreg|class"
  )
})

test_that("Test 43: Empty model list handled gracefully", {
  # Test edge case of no models
  expect_error(
    anova(),
    regexp = ".*"
  )
})
