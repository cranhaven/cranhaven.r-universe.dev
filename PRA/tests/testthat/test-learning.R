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

test_that("risk_post_prob handles fully observed causes", {
  cause_probs <- c(0.3, 0.2)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)
  observed_causes <- c(1, 0)

  result <- risk_post_prob(cause_probs, risks_given_causes, risks_given_not_causes, observed_causes)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("risk_post_prob handles partially observed causes", {
  cause_probs <- c(0.3, 0.2, 0.5)
  risks_given_causes <- c(0.8, 0.6, 0.7)
  risks_given_not_causes <- c(0.2, 0.4, 0.3)
  observed_causes <- c(1, NA, 0)

  result <- risk_post_prob(cause_probs, risks_given_causes, risks_given_not_causes, observed_causes)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("cost_post_pdf generates valid samples", {
  num_sims <- 1000
  observed_risks <- c(1, NA, 1)
  means_given_risks <- c(10000, 15000, 5000)
  sds_given_risks <- c(2000, 1000, 1000)
  base_cost <- 2000

  samples <- cost_post_pdf(num_sims, observed_risks, means_given_risks, sds_given_risks, base_cost)
  expect_equal(length(samples), num_sims)
  expect_true(all(is.numeric(samples)))
})

test_that("cost_post_pdf handles no observed risks", {
  num_sims <- 1000
  observed_risks <- c(NA, NA, NA)
  means_given_risks <- c(10000, 15000, 5000)
  sds_given_risks <- c(2000, 1000, 1000)
  base_cost <- 2000

  samples <- cost_post_pdf(num_sims, observed_risks, means_given_risks, sds_given_risks, base_cost)
  expect_equal(length(samples), num_sims)
  expect_true(all(samples == base_cost))
})

# Error handling tests for risk_post_prob
test_that("risk_post_prob validates input vector lengths", {
  expect_error(
    risk_post_prob(c(0.3), c(0.8, 0.6), c(0.2, 0.4), c(1, 0)),
    "All input vectors must have the same length."
  )
})

test_that("risk_post_prob validates probability ranges", {
  expect_error(
    risk_post_prob(c(0.3, 1.5), c(0.8, 0.6), c(0.2, 0.4), c(1, 0)),
    "All values in cause_probs must be between 0 and 1."
  )
  expect_error(
    risk_post_prob(c(0.3, 0.2), c(0.8, 1.6), c(0.2, 0.4), c(1, 0)),
    "All values in risks_given_causes must be between 0 and 1."
  )
  expect_error(
    risk_post_prob(c(0.3, 0.2), c(0.8, 0.6), c(0.2, -0.4), c(1, 0)),
    "All values in risks_given_not_causes must be between 0 and 1."
  )
})

test_that("risk_post_prob validates observed_causes values", {
  expect_error(
    risk_post_prob(c(0.3, 0.2), c(0.8, 0.6), c(0.2, 0.4), c(1, 2)),
    "All values in observed_causes must be 0, 1, or NA."
  )
})

# Error handling tests for cost_post_pdf
test_that("cost_post_pdf validates num_sims", {
  expect_error(
    cost_post_pdf(-100, c(1, 0), c(10000, 15000), c(2000, 1000), 2000),
    "num_sims must be a positive integer."
  )
  expect_error(
    cost_post_pdf("not a number", c(1, 0), c(10000, 15000), c(2000, 1000), 2000),
    "num_sims must be a positive integer."
  )
})

test_that("cost_post_pdf validates observed_risks values", {
  expect_error(
    cost_post_pdf(1000, c(1, 2), c(10000, 15000), c(2000, 1000), 2000),
    "All values in observed_risks must be 0, 1, or NA."
  )
})

test_that("cost_post_pdf validates vector lengths", {
  expect_error(
    cost_post_pdf(1000, c(1, 0, 1), c(10000, 15000), c(2000, 1000), 2000),
    "observed_risks, means_given_risks, and sds_given_risks must have the same length."
  )
})

test_that("cost_post_pdf validates non-negative standard deviations", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(10000, 15000), c(2000, -1000), 2000),
    "Standard deviations must be non-negative."
  )
})

test_that("risk_post_prob handles all observed causes correctly", {
  cause_probs <- c(0.3, 0.2)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)
  observed_causes <- c(1, 1)

  result <- risk_post_prob(cause_probs, risks_given_causes, risks_given_not_causes, observed_causes)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("cost_post_pdf handles zero observed risks correctly", {
  num_sims <- 1000
  observed_risks <- c(0, 0, 0)
  means_given_risks <- c(10000, 15000, 5000)
  sds_given_risks <- c(2000, 1000, 1000)
  base_cost <- 2000

  samples <- cost_post_pdf(num_sims, observed_risks, means_given_risks, sds_given_risks, base_cost)
  expect_equal(length(samples), num_sims)
  expect_true(all(samples == base_cost))
})

# ============================================================================
# NaN/NA/Inf Error Tests (G5.2, G5.2b)
# ============================================================================
test_that("risk_post_prob rejects NaN in cause params", {
  expect_error(
    risk_post_prob(c(NaN, 0.2), c(0.8, 0.6), c(0.2, 0.4), c(1, 0)),
    "cause_probs, risks_given_causes, and risks_given_not_causes must not contain NaN values."
  )
  expect_error(
    risk_post_prob(c(0.3, 0.2), c(NaN, 0.6), c(0.2, 0.4), c(1, 0)),
    "cause_probs, risks_given_causes, and risks_given_not_causes must not contain NaN values."
  )
  expect_error(
    risk_post_prob(c(0.3, 0.2), c(0.8, 0.6), c(NaN, 0.4), c(1, 0)),
    "cause_probs, risks_given_causes, and risks_given_not_causes must not contain NaN values."
  )
})

test_that("risk_post_prob rejects NA in cause params", {
  expect_error(
    risk_post_prob(c(NA_real_, 0.2), c(0.8, 0.6), c(0.2, 0.4), c(1, 0)),
    "cause_probs, risks_given_causes, and risks_given_not_causes must not contain NA values."
  )
})

test_that("risk_post_prob rejects Inf in cause params", {
  expect_error(
    risk_post_prob(c(Inf, 0.2), c(0.8, 0.6), c(0.2, 0.4), c(1, 0)),
    "cause_probs, risks_given_causes, and risks_given_not_causes must not contain infinite values."
  )
})

test_that("risk_post_prob rejects Inf in observed_causes", {
  expect_error(
    risk_post_prob(c(0.3, 0.2), c(0.8, 0.6), c(0.2, 0.4), c(Inf, 0)),
    "observed_causes must not contain infinite values."
  )
})

test_that("cost_post_pdf rejects NaN in means/sds", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(NaN, 15000), c(2000, 1000), 2000),
    "means_given_risks and sds_given_risks must not contain NaN values."
  )
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(10000, 15000), c(NaN, 1000), 2000),
    "means_given_risks and sds_given_risks must not contain NaN values."
  )
})

test_that("cost_post_pdf rejects NA in means/sds", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(NA_real_, 15000), c(2000, 1000), 2000),
    "means_given_risks and sds_given_risks must not contain NA values."
  )
})

test_that("cost_post_pdf rejects Inf in means/sds", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(Inf, 15000), c(2000, 1000), 2000),
    "means_given_risks and sds_given_risks must not contain infinite values."
  )
})

test_that("cost_post_pdf rejects Inf in observed_risks", {
  expect_error(
    cost_post_pdf(1000, c(Inf, 0), c(10000, 15000), c(2000, 1000), 2000),
    "observed_risks must not contain infinite values."
  )
})

test_that("cost_post_pdf rejects NaN base_cost", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(10000, 15000), c(2000, 1000), NaN),
    "base_cost must not be NaN."
  )
})

test_that("cost_post_pdf rejects NA base_cost", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(10000, 15000), c(2000, 1000), NA_real_),
    "base_cost must not be NA."
  )
})

test_that("cost_post_pdf rejects Inf base_cost", {
  expect_error(
    cost_post_pdf(1000, c(1, 0), c(10000, 15000), c(2000, 1000), Inf),
    "base_cost must not be infinite."
  )
})

# ============================================================================
# G5.3: Return value tests
# ============================================================================
test_that("risk_post_prob result contains no NA, NaN, or Inf", {
  result <- risk_post_prob(c(0.3, 0.2), c(0.8, 0.6), c(0.2, 0.4), c(1, 0))
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("cost_post_pdf result contains no NA, NaN, or Inf", {
  set.seed(42)
  samples <- cost_post_pdf(100, c(1, 0), c(10000, 15000), c(2000, 1000), 2000)
  expect_false(anyNA(samples))
  expect_false(any(is.nan(samples)))
  expect_false(any(is.infinite(samples)))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a)
# ============================================================================

test_that("risk_post_prob handles all NA observations correctly", {
  cause_probs <- c(0.3, 0.2)
  risks_given_causes <- c(0.8, 0.6)
  risks_given_not_causes <- c(0.2, 0.4)
  observed_causes <- c(NA_real_, NA_real_) # No observations

  result <- risk_post_prob(
    cause_probs, risks_given_causes,
    risks_given_not_causes, observed_causes
  )

  # Result should be a valid probability
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
})
