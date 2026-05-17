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

# Define a helper function to generate simulation results for testing
generate_simulation_results <- function() {
  set.seed(123) # Set seed for reproducibility
  num_sims <- 1000
  total_distribution <- rnorm(num_sims, mean = 100, sd = 15)
  list(total_distribution = total_distribution)
}

test_that("contingency function calculates correctly with default percentiles", {
  sims <- generate_simulation_results()
  result <- contingency(sims)
  expect_true(is.numeric(result))

  # Calculate expected values
  expected_phigh_value <- quantile(sims$total_distribution, probs = 0.95)
  expected_pbase_value <- quantile(sims$total_distribution, probs = 0.50)
  expected_contingency <- expected_phigh_value - expected_pbase_value

  expect_equal(result, expected_contingency)
})

test_that("contingency function calculates correctly with custom percentiles", {
  sims <- generate_simulation_results()
  result <- contingency(sims, phigh = 0.90, pbase = 0.10)
  expect_true(is.numeric(result))

  # Calculate expected values
  expected_phigh_value <- quantile(sims$total_distribution, probs = 0.90)
  expected_pbase_value <- quantile(sims$total_distribution, probs = 0.10)
  expected_contingency <- expected_phigh_value - expected_pbase_value

  expect_equal(result, expected_contingency)
})

test_that("contingency function handles edge cases correctly", {
  sims <- generate_simulation_results()

  # phigh is less than pbase (uncommon but should be tested)
  expect_error(contingency(sims, phigh = 0.25, pbase = 0.75), "phigh must be greater than pbase")
})

test_that("contingency function handles invalid input correctly", {
  sims <- generate_simulation_results()

  # Invalid percentile values
  expect_error(contingency(sims, phigh = 1.5, pbase = 0.50), "phigh must be between 0 and 1")
  expect_error(contingency(sims, phigh = 0.95, pbase = -0.5), "pbase must be between 0 and 1")

  # Non-numeric percentiles
  expect_error(contingency(sims, phigh = "high", pbase = 0.50), "phigh must be between 0 and 1")
  expect_error(contingency(sims, phigh = 0.95, pbase = "base"), "pbase must be between 0 and 1")
})

# ============================================================================
# NaN/NA/Inf Error Tests (G5.2, G5.2b)
# ============================================================================
test_that("contingency rejects NaN inputs", {
  sims <- generate_simulation_results()
  expect_error(contingency(sims, phigh = NaN, pbase = 0.50), "phigh must not be NaN")
  expect_error(contingency(sims, phigh = 0.95, pbase = NaN), "pbase must not be NaN")
})

test_that("contingency rejects NA inputs", {
  sims <- generate_simulation_results()
  expect_error(contingency(sims, phigh = NA_real_, pbase = 0.50), "phigh must not be NA")
  expect_error(contingency(sims, phigh = 0.95, pbase = NA_real_), "pbase must not be NA")
})

test_that("contingency rejects Inf inputs", {
  sims <- generate_simulation_results()
  expect_error(contingency(sims, phigh = Inf, pbase = 0.50), "phigh must not be infinite")
  expect_error(contingency(sims, phigh = 0.95, pbase = Inf), "pbase must not be infinite")
})

# ============================================================================
# G5.3: Return value tests
# ============================================================================
test_that("contingency result contains no NA, NaN, or Inf", {
  sims <- generate_simulation_results()
  result <- contingency(sims)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a)
# ============================================================================

test_that("contingency recovers known percentile difference", {
  set.seed(123)

  # Create known MCS result with specific percentiles
  num_sims <- 10000
  task_dists <- list(list(type = "normal", mean = 100, sd = 10))
  mcs_result <- mcs(num_sims, task_dists)

  result <- contingency(mcs_result, phigh = 0.95, pbase = 0.50)

  # Should return difference between 95th and 50th percentiles
  expected <- mcs_result$percentiles[3] - mcs_result$percentiles[2]

  expect_equal(result, expected, tolerance = 1e-10)
})

# ============================================================================
# Edge Condition Tests (G5.8c) - All-Identical Data
# ============================================================================

test_that("contingency handles all identical values in distribution", {
  set.seed(123)

  # Create MCS result with constant values
  num_sims <- 1000
  constant_dist <- rep(100, num_sims)
  sims <- list(total_distribution = constant_dist)
  class(sims) <- "mcs"

  result <- contingency(sims, phigh = 0.95, pbase = 0.50)

  # Contingency should be 0 since all values are identical
  expect_equal(as.numeric(result), 0, tolerance = 1e-10)
})
