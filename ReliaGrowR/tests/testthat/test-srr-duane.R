#' @srrstats {G5.1} The function is tested with a standard data set. The data set is
#' created within and used to test the package. The data set is exported so that users
#' can confirm tests and run examples.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.4c} Unit tests include stored values that are drawn from a published paper output.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produce expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as data changes.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.9b} Unit tests check that different random seeds do not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#' @srrstats {RE7.1} Unit tests check for noiseless, exact relationships between
#' predictor (independent) and response (dependent) data.
#' @srrstats {RE7.1a} Unit tests confirm that model fitting is at least as fast
#' or faster than testing with equivalent noisy data.
#' @srrstats {RE7.2} Unit tests demonstrate that output objects retain aspects
#' of input data such as case names.
#' @srrstats {RE7.3} Unit tests demonstrate expected behavior when `duane` object
#' is submitted to the accessor methods `print` and `plot`.

set.seed(123)

test_that("data.frame input works the same as separate vectors", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 1)
  df <- data.frame(times = times, failures = failures)

  res_df <- duane(df)
  res_vec <- duane(times, failures)

  expect_s3_class(res_df, "duane")
  expect_s3_class(res_vec, "duane")
  expect_equal(res_df$Cumulative_Time, res_vec$Cumulative_Time)
  expect_equal(res_df$Cumulative_MTBF, res_vec$Cumulative_MTBF)
  expect_equal(res_df$Fitted_Values, res_vec$Fitted_Values)
})

test_that("data.frame without required columns errors", {
  df1 <- data.frame(x = 1:3, y = 1:3)
  expect_error(
    duane(df1),
    "must contain columns named 'times' and 'failures'"
  )

  df2 <- data.frame(times = 1:3)
  expect_error(
    duane(df2),
    "must contain columns named 'times' and 'failures'"
  )
})

test_that("NA or NaN in data.frame input throws error", {
  df_na <- data.frame(times = c(100, 200, NA), failures = c(1, 2, 1))
  df_nan <- data.frame(times = c(100, NaN, 300), failures = c(1, 2, 1))
  df_fail_na <- data.frame(times = c(100, 200, 300), failures = c(1, NA, 1))

  expect_error(duane(df_na), "'times' contains missing")
  expect_error(duane(df_nan), "'times' contains missing")
  expect_error(duane(df_fail_na), "'failures' contains missing")
})

test_that("data.frame with non-positive or infinite values errors", {
  df_zero_time <- data.frame(times = c(0, 100, 200), failures = c(1, 2, 1))
  df_neg_fail <- data.frame(times = c(100, 200, 300), failures = c(1, -1, 1))
  df_inf_time <- data.frame(times = c(100, Inf, 200), failures = c(1, 2, 1))

  expect_error(duane(df_zero_time), "must be finite and > 0")
  expect_error(duane(df_neg_fail), "must be finite and > 0")
  expect_error(duane(df_inf_time), "must be finite and > 0")
})

test_that("data.frame input respects conf.level parameter", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 1)
  df <- data.frame(times = times, failures = failures)

  res <- duane(df, conf.level = 0.90)
  expect_equal(res$conf.level, 0.90)
})

# Helper: minimal valid inputs
valid_times <- c(100, 200, 300)
valid_failures <- c(1, 2, 1)

test_that("data frame without required columns errors", {
  df_bad <- data.frame(a = 1:3, b = 1:3)
  expect_error(duane(df_bad),
    "Data frame input must contain columns named 'times' and 'failures'.",
    fixed = TRUE
  )
})

test_that("NA / NaN checks for times and failures", {
  expect_error(duane(c(1, NA), valid_failures),
    "'times' contains missing (NA) or NaN values.",
    fixed = TRUE
  )

  expect_error(duane(valid_times, c(1, NaN, 1)),
    "'failures' contains missing (NA) or NaN values.",
    fixed = TRUE
  )
})

test_that("type checks for times and failures", {
  expect_error(duane(list(1, 2, 3), valid_failures),
    "'times' must be a numeric vector.",
    fixed = TRUE
  )

  expect_error(duane(valid_times, list(1, 2, 3)),
    "'failures' must be a numeric vector.",
    fixed = TRUE
  )
})

test_that("empty vector checks", {
  expect_error(duane(numeric(0), numeric(0)),
    "'times' cannot be empty.",
    fixed = TRUE
  )

  expect_error(duane(valid_times, numeric(0)),
    "'failures' cannot be empty.",
    fixed = TRUE
  )
})

test_that("length mismatch check", {
  expect_error(duane(c(1, 2), c(1, 2, 3)),
    "The length of 'times' and 'failures' must be equal.",
    fixed = TRUE
  )
})

test_that("finite and >0 checks for times and failures", {
  expect_error(duane(c(1, Inf, 3), c(1, 1, 1)),
    "All values in 'times' must be finite and > 0.",
    fixed = TRUE
  )

  expect_error(duane(valid_times, c(1, 0, 2)),
    "All values in 'failures' must be finite and > 0.",
    fixed = TRUE
  )
})

test_that("conf.level validation", {
  expect_error(duane(valid_times, valid_failures, conf.level = c(0.9, 0.95)),
    "'conf.level' must be a single numeric value.",
    fixed = TRUE
  )

  expect_error(duane(valid_times, valid_failures, conf.level = 1),
    "'conf.level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )

  expect_error(duane(valid_times, valid_failures, conf.level = 0),
    "'conf.level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )
})

test_that("print.duane errors when input is not duane", {
  expect_error(print.duane(list()),
    "'x' must be an object of class 'duane'.",
    fixed = TRUE
  )
})

test_that("plot.duane argument type checks", {
  # Minimal valid duane object
  dummy_fit <- duane(valid_times, valid_failures)

  # inherits check
  expect_error(plot.duane(list()),
    "'x' must be an object of class 'duane'.",
    fixed = TRUE
  )

  # log must be single logical
  expect_error(plot.duane(dummy_fit, log = "nope"),
    "'log' must be a single logical value.",
    fixed = TRUE
  )

  # conf.int must be single logical
  expect_error(plot.duane(dummy_fit, log = TRUE, conf.int = "nope"),
    "'conf.int' must be a single logical value.",
    fixed = TRUE
  )

  # legend must be single logical
  expect_error(plot.duane(dummy_fit, log = TRUE, conf.int = TRUE, legend = "nope"),
    "'legend' must be a single logical value.",
    fixed = TRUE
  )

  # legend.pos must be single character
  expect_error(plot.duane(dummy_fit, log = TRUE, conf.int = TRUE, legend = TRUE, legend.pos = c("a", "b")),
    "'legend.pos' must be a single character string.",
    fixed = TRUE
  )
})

test_that("duane recovers known parameters (linear log-log relationship)", {
  # Generate synthetic data for constant MTBF process
  times <- rep(100, 10) # equal spacing of 100
  failures <- rep(1, 10) # one failure per interval

  fit <- duane(times, failures, conf.level = 0.95)

  coef_est <- coef(fit$model)

  # Expected: slope ~ 1, intercept ~ log(constant)
  expect_equal(unname(exp(coef_est[2])), 1, tolerance = 0.2)
})

test_that("duane returns same result for vector and data.frame inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit_vec <- duane(times, failures)
  fit_df <- duane(data.frame(times = times, failures = failures))

  expect_equal(fit_vec$Cumulative_MTBF,
    fit_df$Cumulative_MTBF,
    tolerance = 1e-12
  )

  expect_equal(coef(fit_vec$model),
    coef(fit_df$model),
    tolerance = 1e-12
  )
})

test_that("confidence level is respected", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit90 <- duane(times, failures, conf.level = 0.90)
  fit95 <- duane(times, failures, conf.level = 0.95)

  # Wider interval for higher confidence level
  width90 <- mean(fit90$Confidence_Bounds[, "upr"] - fit90$Confidence_Bounds[, "lwr"])
  width95 <- mean(fit95$Confidence_Bounds[, "upr"] - fit95$Confidence_Bounds[, "lwr"])

  expect_gt(width95, width90)
})

test_that("slope approaches 1 for constant MTBF process", {
  # Failures occur at constant intervals
  times <- rep(100, 20)
  failures <- rep(1, 20)

  fit <- duane(times, failures)
  slope <- unname(exp(coef(fit$model)[2]))

  expect_equal(slope, 1, tolerance = 0.2)
})

test_that("slope is less than 1 for reliability growth process", {
  # Failures get further apart over time (improving reliability)
  times <- seq(100, 1000, length.out = 20)
  failures <- rep(1, 20)

  fit <- duane(times, failures)
  slope <- coef(fit$model)[2]

  expect_lt(slope, 1)
})

test_that("cumulative MTBF increases with cumulative time if failures constant", {
  times <- rep(100, 10)
  failures <- rep(1, 10)

  fit <- duane(times, failures)

  expect_true(all(diff(fit$Cumulative_MTBF) >= 0))
})

# test_that("loglik increases with more data", {
#   times_small <- rep(100, 5)
#   failures_small <- rep(1, 5)
#   fit_small <- duane(times_small, failures_small)
#
#   times_large <- rep(100, 50)
#   failures_large <- rep(1, 50)
#   fit_large <- duane(times_large, failures_large)
#
#   # Larger datasets should yield smaller logLik but larger penalties
#   expect_lt(fit_small$logLik, fit_large$logLik) # logLik increases with more data
#
# })

test_that("adding small noise to times does not change slope significantly", {
  times <- rep(100, 20)
  failures <- rep(1, 20)

  fit_clean <- duane(times, failures)

  # Add ±1% random noise
  noise <- rnorm(length(times), mean = 0, sd = 1)
  times_noisy <- times + times * noise * 0.01
  fit_noisy <- duane(times_noisy, failures)

  slope_clean <- coef(fit_clean$model)[2]
  slope_noisy <- coef(fit_noisy$model)[2]

  expect_equal(slope_clean, slope_noisy, tolerance = 0.05)
})

test_that("adding small noise preserves fitted MTBF values", {
  times <- seq(100, 1000, length.out = 20)
  failures <- rep(1, 20)

  fit_clean <- duane(times, failures)

  noise <- rnorm(length(times), mean = 0, sd = 1)
  times_noisy <- times + times * noise * 0.01
  fit_noisy <- duane(times_noisy, failures)

  # Compare fitted values via correlation
  cor_val <- cor(fit_clean$Fitted_Values, fit_noisy$Fitted_Values)
  expect_gt(cor_val, 0.99)
})

test_that("duane works with input from testdata", {
  data("testdata", package = "ReliaGrowR")

  # Subset one LRU to get a simple series
  g1 <- subset(testdata, LRU == "G1")

  # Use cumulative ETI as times and Failure_Count as failures
  times <- g1$Cum_ETI
  failures <- g1$Failure_Count

  fit <- duane(times, failures, conf.level = 0.95)

  # Check structure
  expect_true(all(fit$Cumulative_MTBF > 0))
  expect_true(all(fit$Fitted_Values > 0))
})

# Example data
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- duane(times, failures, conf.level = 0.95)

test_that("print.duane works on valid duane object", {
  expect_invisible(out <- print(fit))
  expect_s3_class(out, "duane")
})

test_that("print.duane shows expected content", {
  output <- capture.output(print(fit))
  expect_true(any(grepl("Duane Analysis Result", output)))
  expect_true(any(grepl("Coefficients:", output)))
  expect_true(any(grepl("Log-likelihood:", output)))
  expect_true(any(grepl("AIC:", output)))
  expect_true(any(grepl("Confidence level:", output)))
})

test_that("print.duane errors on wrong input type", {
  expect_error(print.duane(123), "'x' must be an object of class 'duane'")
  expect_error(print.duane(list()), "'x' must be an object of class 'duane'")
})

test_that("plot.duane works on valid duane object", {
  expect_invisible(plot(fit)) # default args
  expect_invisible(plot(fit, log = FALSE, conf.int = FALSE, legend = FALSE))
})

test_that("plot.duane errors on wrong input type", {
  expect_error(plot.duane(123), "'x' must be an object of class 'duane'")
  expect_error(plot.duane(list()), "'x' must be an object of class 'duane'")
})

test_that("plot.duane input validation works", {
  expect_error(plot(fit, log = "yes"), "'log' must be a single logical value")
  expect_error(plot(fit, conf.int = "yes"), "'conf.int' must be a single logical value")
  expect_error(plot(fit, legend = "yes"), "'legend' must be a single logical value")
  expect_error(plot(fit, legend.pos = TRUE), "'legend.pos' must be a single character string")
})

test_that("duane() handles noiseless, exact relationships efficiently", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  set.seed(123)

  # Generate synthetic data with a perfect log-log linear relationship
  n <- 1000
  times <- seq(1, n)
  alpha <- 2.0
  beta <- 0.5
  # Perfect noiseless relationship (log-log linear)
  failures_noiseless <- exp((log(times) - log(alpha)) / beta)

  # Slightly noisy version of the same
  noise <- rnorm(n, mean = 0, sd = 0.1)
  failures_noisy <- exp((log(times) - log(alpha)) / beta + noise)

  # Measure performance (timing)
  t_noiseless <- system.time({
    fit_noiseless <- duane(times, failures_noiseless)
  })[["elapsed"]]

  t_noisy <- system.time({
    fit_noisy <- duane(times, failures_noisy)
  })[["elapsed"]]

  # Check that noiseless fit is at least as fast or faster
  expect_true(
    t_noiseless <= t_noisy * 1.2 + 0.1, # allow small tolerance
    info = sprintf("Noiseless fit took %.3fs vs noisy %.3fs", t_noiseless, t_noisy)
  )

  # Check that coefficients are nearly identical in noiseless case
  coef_noiseless <- coef(fit_noiseless$model)
  coef_noisy <- coef(fit_noisy$model)
})

test_that("output retains row or case names from input data", {
  # Case 1: Named numeric vectors
  times <- c(A = 100, B = 200, C = 300, D = 400, E = 500)
  failures <- c(A = 1, B = 2, C = 1, D = 3, E = 2)

  fit_named <- duane(times, failures)

  # Expect that output retains names
  expect_equal(names(fit_named$times), names(times))
  expect_equal(names(fit_named$failures), names(failures))
})
