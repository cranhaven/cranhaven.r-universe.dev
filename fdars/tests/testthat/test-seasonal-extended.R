# Tests for seasonal analysis functions: lomb.scargle, matrix.profile,
# stl.fd, ssa.fd, cfd.autoperiod, autoperiod, sazed

# Helper: create a periodic fdata object with known period
make_periodic_fdata <- function(n = 5, m = 200, period = 20, noise_sd = 0.1, seed = 42) {
  set.seed(seed)
  t <- seq(0, 1, length.out = m)
  X <- matrix(0, n, m)
  for (i in seq_len(n)) {
    X[i, ] <- sin(2 * pi * t * (m / period)) + rnorm(m, sd = noise_sd)
  }
  fdata(X, argvals = t)
}

# =============================================================================
# Lomb-Scargle Periodogram
# =============================================================================

test_that("lomb.scargle returns correct class and structure", {
  fd <- make_periodic_fdata()
  result <- lomb.scargle(fd)

  expect_s3_class(result, "lomb_scargle_result")
  expect_true("peak_period" %in% names(result))
  expect_true("peak_frequency" %in% names(result))
  expect_true("peak_power" %in% names(result))
  expect_true("frequencies" %in% names(result))
  expect_true("power" %in% names(result))
  expect_true("false_alarm_probability" %in% names(result))
  expect_true("significance" %in% names(result))
})

test_that("lomb.scargle detects correct period", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- lomb.scargle(fd)

  # Peak period should be close to true period (20 out of 200 = 0.1 in [0,1])
  expect_true(result$peak_period > 0)
  expect_true(result$peak_power > 0)
  expect_true(length(result$frequencies) > 0)
  expect_true(length(result$power) == length(result$frequencies))
})

test_that("lomb.scargle validates input", {
  expect_error(lomb.scargle(matrix(1:10, 2, 5)), "fdata")
  # Too few time points
  fd_short <- fdata(matrix(1:2, 1, 2), argvals = 1:2)
  expect_error(lomb.scargle(fd_short), "at least 3")
})

test_that("lomb.scargle print method works", {
  fd <- make_periodic_fdata()
  result <- lomb.scargle(fd)
  expect_output(print(result), "Lomb-Scargle")
})

test_that("lomb.scargle plot method works", {
  fd <- make_periodic_fdata()
  result <- lomb.scargle(fd)
  expect_no_error(plot(result))
})

# =============================================================================
# Matrix Profile
# =============================================================================

test_that("matrix.profile returns correct class and structure", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- matrix.profile(fd)

  expect_s3_class(result, "matrix_profile_result")
  expect_true("profile" %in% names(result))
  expect_true("primary_period" %in% names(result))
  expect_true("subsequence_length" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_true(length(result$profile) > 0)
})

test_that("matrix.profile with explicit subsequence_length", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- matrix.profile(fd, subsequence_length = 10)

  expect_s3_class(result, "matrix_profile_result")
  expect_equal(result$subsequence_length, 10)
})

test_that("matrix.profile validates input", {
  expect_error(matrix.profile(matrix(1:10, 2, 5)), "fdata")
  fd_short <- fdata(matrix(1:5, 1, 5), argvals = 1:5)
  expect_error(matrix.profile(fd_short), "at least 8")
})

test_that("matrix.profile print method works", {
  fd <- make_periodic_fdata(n = 1, m = 100)
  result <- matrix.profile(fd)
  expect_output(print(result), "Matrix Profile")
})

test_that("matrix.profile plot method works", {
  fd <- make_periodic_fdata(n = 1, m = 100)
  result <- matrix.profile(fd)
  expect_no_error(plot(result))
})

# =============================================================================
# STL Decomposition
# =============================================================================

test_that("stl.fd returns correct class and structure", {
  fd <- make_periodic_fdata(n = 3, m = 200, period = 20)
  result <- stl.fd(fd, period = 20)

  expect_s3_class(result, "stl_result")
  expect_s3_class(result$trend, "fdata")
  expect_s3_class(result$seasonal, "fdata")
  expect_s3_class(result$remainder, "fdata")
  expect_equal(nrow(result$trend$data), 3)
  expect_equal(ncol(result$trend$data), 200)
})

test_that("stl.fd components sum to original", {
  fd <- make_periodic_fdata(n = 2, m = 200, period = 20, noise_sd = 0.5)
  result <- stl.fd(fd, period = 20)

  reconstructed <- result$trend$data + result$seasonal$data + result$remainder$data
  expect_equal(reconstructed, fd$data, tolerance = 1e-6)
})

test_that("stl.fd with robust=FALSE", {
  fd <- make_periodic_fdata(n = 2, m = 200, period = 20)
  result <- stl.fd(fd, period = 20, robust = FALSE)

  expect_s3_class(result, "stl_result")
  reconstructed <- result$trend$data + result$seasonal$data + result$remainder$data
  expect_equal(reconstructed, fd$data, tolerance = 1e-6)
})

test_that("stl.fd validates input", {
  expect_error(stl.fd(matrix(1:10, 2, 5), period = 2), "fdata")

  # Series too short for period
  fd_short <- fdata(matrix(rnorm(20), 1, 20), argvals = 1:20)
  expect_error(stl.fd(fd_short, period = 15), "at least 2 complete")

  # Period too small
  fd <- make_periodic_fdata()
  expect_error(stl.fd(fd, period = 1), "at least 2")
})

test_that("stl.fd print method works", {
  fd <- make_periodic_fdata(n = 2, m = 200, period = 20)
  result <- stl.fd(fd, period = 20)
  expect_output(print(result), "STL Decomposition")
})

test_that("stl.fd plot method works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20)
  result <- stl.fd(fd, period = 20)
  expect_no_error(plot(result))
})

# =============================================================================
# SSA (Singular Spectrum Analysis)
# =============================================================================

test_that("ssa.fd returns correct class and structure", {
  fd <- make_periodic_fdata(n = 2, m = 100, period = 20)
  result <- ssa.fd(fd)

  expect_s3_class(result, "ssa_result")
  expect_s3_class(result$trend, "fdata")
  expect_s3_class(result$seasonal, "fdata")
  expect_s3_class(result$noise, "fdata")
  expect_true(length(result$singular.values) > 0)
  expect_true(length(result$contributions) > 0)
  expect_true(result$window.length > 0)
})

test_that("ssa.fd with custom window.length", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- ssa.fd(fd, window.length = 25)

  expect_s3_class(result, "ssa_result")
  expect_equal(result$window.length, 25)
})

test_that("ssa.fd validates input", {
  expect_error(ssa.fd(matrix(1:10, 2, 5)), "fdata")
  fd_short <- fdata(matrix(1:3, 1, 3), argvals = 1:3)
  expect_error(ssa.fd(fd_short), "at least 4")
})

test_that("ssa.fd contributions sum to approximately 1", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- ssa.fd(fd)

  expect_true(sum(result$contributions) > 0.9)
  expect_true(sum(result$contributions) <= 1.01)
})

test_that("ssa.fd print method works", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- ssa.fd(fd)
  expect_output(print(result), "Singular Spectrum Analysis")
})

test_that("ssa.fd plot method works", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- ssa.fd(fd)
  expect_no_error(plot(result))
})

# =============================================================================
# CFDAutoperiod
# =============================================================================

test_that("cfd.autoperiod returns correct class and structure", {
  fd <- make_periodic_fdata(n = 3, m = 200, period = 20, noise_sd = 0.05)
  result <- cfd.autoperiod(fd)

  expect_s3_class(result, "cfd_autoperiod_result")
  expect_true("period" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_true("n_periods" %in% names(result))
  expect_true(result$period > 0)
})

test_that("cfd.autoperiod validates input", {
  expect_error(cfd.autoperiod(matrix(1:10, 2, 5)), "fdata")
})

test_that("cfd.autoperiod print method works", {
  fd <- make_periodic_fdata(n = 3, m = 200, period = 20)
  result <- cfd.autoperiod(fd)
  expect_output(print(result), "CFDAutoperiod")
})

# =============================================================================
# Autoperiod
# =============================================================================

test_that("autoperiod returns correct class and structure", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- autoperiod(fd)

  expect_s3_class(result, "autoperiod_result")
  expect_true("period" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_true("candidates" %in% names(result))
  expect_true(is.data.frame(result$candidates))
  expect_true(result$period > 0)
})

test_that("autoperiod with detrend_method", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20)
  result <- autoperiod(fd, detrend_method = "linear")
  expect_s3_class(result, "autoperiod_result")
})

test_that("autoperiod validates input", {
  expect_error(autoperiod(matrix(1:10, 2, 5)), "fdata")
})

test_that("autoperiod print method works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20)
  result <- autoperiod(fd)
  expect_output(print(result), "Autoperiod")
})

# =============================================================================
# SAZED
# =============================================================================

test_that("sazed returns correct class and structure", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- sazed(fd)

  expect_s3_class(result, "sazed_result")
  expect_true("period" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_true("components" %in% names(result))
  expect_true("agreeing_components" %in% names(result))
  expect_true(result$period > 0)
})

test_that("sazed with detrend_method", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20)
  result <- sazed(fd, detrend_method = "linear")
  expect_s3_class(result, "sazed_result")
})

test_that("sazed components are named correctly", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20)
  result <- sazed(fd)

  expect_true("spectral" %in% names(result$components))
  expect_true("acf_peak" %in% names(result$components))
  expect_true("acf_average" %in% names(result$components))
  expect_true("zero_crossing" %in% names(result$components))
  expect_true("spectral_diff" %in% names(result$components))
})

test_that("sazed validates input", {
  expect_error(sazed(matrix(1:10, 2, 5)), "fdata")
})

test_that("sazed print method works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20)
  result <- sazed(fd)
  expect_output(print(result), "SAZED")
})
