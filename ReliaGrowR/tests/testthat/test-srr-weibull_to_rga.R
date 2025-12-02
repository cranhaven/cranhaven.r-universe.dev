#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical
#' algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.

test_that("weibull_to_rga() input validation errors", {
  # failures
  expect_error(
    weibull_to_rga("a"),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, -2, 3)),
    "`failures` must be a numeric vector with positive values."
  )
  expect_error(
    weibull_to_rga(c(1, NA, 3)),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, NaN, 3)),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, Inf, 3)),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )

  # suspensions
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = "a"),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, -2)),
    "`suspensions` must be a numeric vector with positive values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, NA)),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, NaN)),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, Inf)),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval bounds missing
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2)),
    "Both `interval_starts` and `interval_ends` must be provided together."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_ends = c(1, 2)),
    "Both `interval_starts` and `interval_ends` must be provided together."
  )

  # interval bounds type
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = "a", interval_ends = 2),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = 1, interval_ends = "b"),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval bounds length mismatch
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = 3),
    "`interval_starts` and `interval_ends` must have the same length."
  )

  # interval bounds positivity
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, -2), interval_ends = c(2, 3)),
    "Interval bounds must be positive."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(-2, 3)),
    "Interval bounds must be positive."
  )

  # interval_starts bad values
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, NA), interval_ends = c(2, 3)),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, NaN), interval_ends = c(2, 3)),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, Inf), interval_ends = c(2, 3)),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval_ends bad values
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(NA, 3)),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(NaN, 3)),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(Inf, 3)),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval_starts >= interval_ends
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(5, 10), interval_ends = c(5, 8)),
    "Each interval start must be strictly less than its corresponding end."
  )
})

test_that("weibull_to_rga() works with failures only", {
  res <- weibull_to_rga(c(10, 20, 30))
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("CumulativeTime", "Failures"))
  expect_equal(nrow(res), 3)
  expect_true(all(res$Failures == 1))
})

test_that("weibull_to_rga() works with failures and suspensions", {
  res <- weibull_to_rga(c(10, 20), suspensions = c(15, 25))
  # suspensions should not contribute to Failures
  expect_equal(res$Failures, c(1, 1))
  expect_true(all(res$CumulativeTime > 0))
})

test_that("weibull_to_rga() works with interval-censored data", {
  res <- weibull_to_rga(
    failures = c(50, 100),
    interval_starts = c(30, 80),
    interval_ends = c(40, 120)
  )
  # Two failures + two interval failures
  expect_true(all(res$Failures >= 1))
  expect_true(any(diff(res$CumulativeTime) > 0))
})

test_that("weibull_to_rga() works with failures, suspensions, and interval data", {
  res <- weibull_to_rga(
    failures = c(100, 200, 200, 400),
    suspensions = c(250, 350, 450),
    interval_starts = c(150, 300),
    interval_ends = c(180, 320)
  )
  # There should be aggregated failures (since 200 repeats)
  expect_true(any(res$Failures > 1))
  expect_gt(nrow(res), 2)
  expect_equal(names(res), c("CumulativeTime", "Failures"))
})

test_that("weibull_to_rga() handles duplicate failures correctly", {
  res <- weibull_to_rga(c(10, 10, 20))
  # At time 10 there should be 2 failures aggregated
  expect_true(any(res$Failures == 2))
})
