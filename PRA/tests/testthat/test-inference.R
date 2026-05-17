#' @srrstats {G5.2} *Error and warning behaviour is explicitly demonstrated through tests.*
#' @srrstats {G5.2a} *Every error message is unique and tested.*
#' @srrstats {G5.2b} *Tests trigger every error message and compare with expected values.*
#' @srrstats {G5.3} *Return objects tested for absence of NA, NaN, Inf.*
#' @srrstats {G5.6} *Parameter recovery tests verify implementations produce expected results given data with known properties.*
#' @srrstats {G5.6a} *Parameter recovery tests succeed within defined tolerance rather than exact values.*
#' @srrstats {G5.6b} *Parameter recovery tests run with multiple random seeds when randomness is involved.*
#' @srrstats {G5.7} *Algorithm performance tests verify implementations perform correctly as data properties change.*
#' @srrstats {G5.8} *Edge condition tests verify appropriate behavior with extreme data properties.*
#' @srrstats {G5.8a} *Zero-length data tests trigger clear errors.*
#' @srrstats {G5.8b} *Unsupported data type tests trigger clear errors.*
#' @srrstats {G5.8c} *All-NA and all-identical data tests trigger clear errors or warnings.*
#' @srrstats {G5.8d} *Out-of-scope data tests verify appropriate behavior.*
#' @srrstats {G5.9} *Noise susceptibility tests verify stochastic behavior stability.*
#' @srrstats {G5.9a} *Trivial noise tests show results are stable at machine epsilon scale.*
#' @srrstats {G5.9b} *Random seed stability tests show consistent behavior across different seeds.*

# Unit tests for risk_prob function
test_that("risk_prob calculates correct risk probabilities", {
  cause_probs <- c(0.3, 0.2)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)

  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
  expected <- (0.3 * 0.8) + ((1 - 0.3) * 0.2) + (0.2 * 0.6) + ((1 - 0.2) * 0.4)
  expect_equal(result, expected)
})

test_that("risk_prob handles input validation correctly", {
  expect_error(risk_prob(c(0.3, 1.2), c(0.8, 0.6), c(0.2, 0.4)), "All values in cause_probs must be between 0 and 1.")
  expect_error(risk_prob(c(0.3, 0.2), c(0.8, 1.6), c(0.2, 0.4)), "All values in risks_given_causes must be between 0 and 1.")
  expect_error(risk_prob(c(0.3, 0.2), c(0.8, 0.6), c(0.2, -0.4)), "All values in risks_given_not_causes must be between 0 and 1.")
  expect_error(risk_prob(c(0.3, 0.2), c(0.8), c(0.2, 0.4)), "All input vectors must have the same length.")
})

# Unit tests for cost_pdf function
test_that("cost_pdf generates correct sample sizes", {
  num_sims <- 1000
  risk_probs <- c(0.3, 0.5)
  means_given_risks <- c(10000, 15000)
  sds_given_risks <- c(2000, 1000)
  base_cost <- 2000

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)
  expect_equal(length(samples), num_sims)
})

test_that("cost_pdf handles input validation correctly", {
  expect_error(cost_pdf(-1000, c(0.3, 0.5), c(10000, 15000), c(2000, 1000), 2000), "num_sims must be a positive integer.")
  expect_error(cost_pdf(1000, c(0.3, 1.5), c(10000, 15000), c(2000, 1000), 2000), "All risk_probs must be between 0 and 1.")
  expect_error(cost_pdf(1000, c(0.3, 0.5), c(10000), c(2000, 1000), 2000), "risk_probs, means_given_risks, and sds_given_risks must have the same length.")
  expect_error(cost_pdf(1000, c(0.3, 0.5), c(10000, 15000), c(2000, -1000), 2000), "Standard deviations must be non-negative.")
})

test_that("cost_pdf generates realistic samples", {
  num_sims <- 1000
  risk_probs <- c(0.3)
  means_given_risks <- c(10000)
  sds_given_risks <- c(2000)
  base_cost <- 2000

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)
  expect_true(all(samples >= base_cost))
  expect_gt(mean(samples), base_cost)
})

# Additional risk_prob tests
test_that("risk_prob handles single cause correctly", {
  cause_probs <- c(0.5)
  risks_given_causes <- c(0.8)
  risks_given_not_causes <- c(0.2)

  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
  expected <- (0.5 * 0.8) + (0.5 * 0.2)
  expect_equal(result, expected)
})

test_that("risk_prob handles extreme probabilities correctly", {
  # All causes certain
  cause_probs <- c(1.0, 1.0)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)

  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
  expect_true(is.numeric(result))

  # No causes
  cause_probs <- c(0.0, 0.0)
  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
  expect_true(is.numeric(result))
})

# Additional cost_pdf tests
test_that("cost_pdf handles zero risk probabilities correctly", {
  num_sims <- 1000
  risk_probs <- c(0, 0)
  means_given_risks <- c(10000, 15000)
  sds_given_risks <- c(2000, 1000)
  base_cost <- 2000

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)
  expect_equal(length(samples), num_sims)
  expect_true(all(samples == base_cost))
})

test_that("cost_pdf validates sum of risk_probs", {
  expect_error(
    cost_pdf(1000, c(0.6, 0.6), c(10000, 15000), c(2000, 1000), 2000),
    "Sum of risk_probs must not exceed 1."
  )
})

test_that("cost_pdf handles default base_cost correctly", {
  num_sims <- 100
  risk_probs <- c(0)
  means_given_risks <- c(10000)
  sds_given_risks <- c(2000)

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks)
  expect_true(all(samples == 0))
})

# ============================================================================
# NaN/NA/Inf Error Tests (G5.2, G5.2b)
# ============================================================================
test_that("risk_prob rejects NaN inputs", {
  expect_error(risk_prob(c(NaN, 0.2), c(0.8, 0.6), c(0.2, 0.4)), "Input vectors must not contain NaN values.")
  expect_error(risk_prob(c(0.3, 0.2), c(NaN, 0.6), c(0.2, 0.4)), "Input vectors must not contain NaN values.")
  expect_error(risk_prob(c(0.3, 0.2), c(0.8, 0.6), c(NaN, 0.4)), "Input vectors must not contain NaN values.")
})

test_that("risk_prob rejects NA inputs", {
  expect_error(risk_prob(c(NA_real_, 0.2), c(0.8, 0.6), c(0.2, 0.4)), "Input vectors must not contain NA values.")
  expect_error(risk_prob(c(0.3, 0.2), c(NA_real_, 0.6), c(0.2, 0.4)), "Input vectors must not contain NA values.")
  expect_error(risk_prob(c(0.3, 0.2), c(0.8, 0.6), c(NA_real_, 0.4)), "Input vectors must not contain NA values.")
})

test_that("risk_prob rejects Inf inputs", {
  expect_error(risk_prob(c(Inf, 0.2), c(0.8, 0.6), c(0.2, 0.4)), "Input vectors must not contain infinite values.")
  expect_error(risk_prob(c(0.3, 0.2), c(Inf, 0.6), c(0.2, 0.4)), "Input vectors must not contain infinite values.")
  expect_error(risk_prob(c(0.3, 0.2), c(0.8, 0.6), c(Inf, 0.4)), "Input vectors must not contain infinite values.")
})

test_that("cost_pdf rejects NaN in risk params", {
  expect_error(
    cost_pdf(1000, c(NaN, 0.5), c(10000, 15000), c(2000, 1000), 2000),
    "risk_probs, means_given_risks, and sds_given_risks must not contain NaN values."
  )
  expect_error(
    cost_pdf(1000, c(0.3, 0.5), c(NaN, 15000), c(2000, 1000), 2000),
    "risk_probs, means_given_risks, and sds_given_risks must not contain NaN values."
  )
  expect_error(
    cost_pdf(1000, c(0.3, 0.5), c(10000, 15000), c(NaN, 1000), 2000),
    "risk_probs, means_given_risks, and sds_given_risks must not contain NaN values."
  )
})

test_that("cost_pdf rejects NA in risk params", {
  expect_error(
    cost_pdf(1000, c(NA_real_, 0.5), c(10000, 15000), c(2000, 1000), 2000),
    "risk_probs, means_given_risks, and sds_given_risks must not contain NA values."
  )
})

test_that("cost_pdf rejects Inf in risk params", {
  expect_error(
    cost_pdf(1000, c(Inf, 0.5), c(10000, 15000), c(2000, 1000), 2000),
    "risk_probs, means_given_risks, and sds_given_risks must not contain infinite values."
  )
})

test_that("cost_pdf rejects NaN base_cost", {
  expect_error(
    cost_pdf(1000, c(0.3, 0.5), c(10000, 15000), c(2000, 1000), NaN),
    "base_cost must not be NaN."
  )
})

test_that("cost_pdf rejects NA base_cost", {
  expect_error(
    cost_pdf(1000, c(0.3, 0.5), c(10000, 15000), c(2000, 1000), NA_real_),
    "base_cost must not be NA."
  )
})

test_that("cost_pdf rejects Inf base_cost", {
  expect_error(
    cost_pdf(1000, c(0.3, 0.5), c(10000, 15000), c(2000, 1000), Inf),
    "base_cost must not be infinite."
  )
})

# ============================================================================
# G5.3: Return value tests
# ============================================================================
test_that("risk_prob result contains no NA, NaN, or Inf", {
  result <- risk_prob(c(0.3, 0.2), c(0.8, 0.6), c(0.2, 0.4))
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("cost_pdf result contains no NA, NaN, or Inf", {
  set.seed(42)
  samples <- cost_pdf(100, c(0.3, 0.5), c(10000, 15000), c(2000, 1000), 2000)
  expect_false(anyNA(samples))
  expect_false(any(is.nan(samples)))
  expect_false(any(is.infinite(samples)))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a, G5.6b)
# ============================================================================

test_that("risk_prob recovers known probability with certain cause", {
  # If cause is certain (prob=1) and risk given cause is certain (prob=1)
  cause_probs <- c(1.0)
  risks_given_causes <- c(1.0)
  risks_given_not_causes <- c(0.0)

  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)

  # Expected: P(R) = 1.0 * 1.0 + 0.0 * 0.0 = 1.0
  expect_equal(result, 1.0, tolerance = 1e-10)
})

test_that("risk_prob recovers known probability with no risk", {
  # If all conditional probabilities are zero
  cause_probs <- c(0.5, 0.3)
  risks_given_causes <- c(0.0, 0.0)
  risks_given_not_causes <- c(0.0, 0.0)

  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)

  # Expected: P(R) = 0
  expect_equal(result, 0.0, tolerance = 1e-10)
})

test_that("risk_prob recovers known probability from manual calculation", {
  cause_probs <- c(0.3, 0.2)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)

  result <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)

  # Manual calculation using law of total probability:
  # P(R) = P(R|C1)*P(C1) + P(R|¬C1)*P(¬C1) + P(R|C2)*P(C2) + P(R|¬C2)*P(¬C2)
  #      = 0.8*0.3 + 0.2*0.7 + 0.6*0.2 + 0.4*0.8
  #      = 0.24 + 0.14 + 0.12 + 0.32 = 0.82
  expected <- 0.8 * 0.3 + 0.2 * 0.7 + 0.6 * 0.2 + 0.4 * 0.8

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("cost_pdf converges to theoretical mean with single certain risk (seed 123)", {
  set.seed(123)

  num_sims <- 100000
  risk_probs <- c(1.0) # Certain risk
  means_given_risks <- c(10000)
  sds_given_risks <- c(0) # No variance
  base_cost <- 5000

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)

  # Expected: all samples = base_cost + mean_risk = 15000
  expected_mean <- 5000 + 10000

  expect_equal(mean(samples), expected_mean, tolerance = 1)
})

test_that("cost_pdf converges to theoretical mean with uncertain risk (seed 42)", {
  set.seed(42)

  num_sims <- 100000
  risk_probs <- c(0.5)
  means_given_risks <- c(10000)
  sds_given_risks <- c(1000)
  base_cost <- 5000

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)

  # Expected mean: base_cost + risk_prob * mean_risk
  expected_mean <- 5000 + 0.5 * 10000 # 10000

  expect_equal(mean(samples), expected_mean, tolerance = 100)
})

test_that("cost_pdf with zero risk probability equals base cost (seed 123)", {
  set.seed(123)

  num_sims <- 1000
  risk_probs <- c(0.0)
  means_given_risks <- c(10000)
  sds_given_risks <- c(1000)
  base_cost <- 5000

  samples <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)

  # All samples should equal base_cost
  expect_true(all(samples == base_cost))
})

# ============================================================================
# Algorithm Performance Tests (G5.7)
# ============================================================================

test_that("cost_pdf mean estimate improves with sample size", {
  risk_probs <- c(0.5)
  means_given_risks <- c(10000)
  sds_given_risks <- c(1000)
  base_cost <- 5000

  # Expected mean: base_cost + risk_prob * mean_risk = 5000 + 0.5*10000 = 10000
  expected_mean <- 10000

  # Test with increasing sample sizes
  set.seed(123)
  result_1k <- mean(cost_pdf(1000, risk_probs, means_given_risks, sds_given_risks, base_cost))

  set.seed(123)
  result_10k <- mean(cost_pdf(10000, risk_probs, means_given_risks, sds_given_risks, base_cost))

  set.seed(123)
  result_100k <- mean(cost_pdf(100000, risk_probs, means_given_risks, sds_given_risks, base_cost))

  # Error should decrease with sample size
  error_1k <- abs(result_1k - expected_mean)
  error_10k <- abs(result_10k - expected_mean)
  error_100k <- abs(result_100k - expected_mean)

  expect_true(error_100k < error_10k)
  expect_true(error_10k <= error_1k)
})

# ============================================================================
# Noise Susceptibility Tests (G5.9a) - Trivial Noise
# ============================================================================

test_that("risk_prob is stable to trivial noise in probabilities", {
  cause_probs <- c(0.3, 0.2)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)

  result_clean <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)

  # Add machine epsilon noise
  cause_probs_noisy <- cause_probs + runif(2, -.Machine$double.eps, .Machine$double.eps)

  result_noisy <- risk_prob(cause_probs_noisy, risks_given_causes, risks_given_not_causes)

  # Results should be essentially identical
  expect_equal(result_clean, result_noisy, tolerance = 10 * .Machine$double.eps)
})

# ============================================================================
# Noise Susceptibility Tests (G5.9b) - Random Seed Stability
# ============================================================================

test_that("cost_pdf produces consistent distributions across seeds", {
  num_sims <- 10000
  risk_probs <- c(0.5)
  means_given_risks <- c(10000)
  sds_given_risks <- c(1000)
  base_cost <- 5000

  # Run with different seeds
  set.seed(111)
  samples_1 <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)

  set.seed(222)
  samples_2 <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)

  set.seed(333)
  samples_3 <- cost_pdf(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost)

  # Means should be similar
  expect_equal(mean(samples_1), mean(samples_2), tolerance = 100)
  expect_equal(mean(samples_2), mean(samples_3), tolerance = 100)

  # Standard deviations should be similar
  expect_equal(sd(samples_1), sd(samples_2), tolerance = 100)
  expect_equal(sd(samples_2), sd(samples_3), tolerance = 100)
})
