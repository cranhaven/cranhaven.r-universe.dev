# Tests for uncovered basis, seasonal, and outlier code paths

# Helper
make_fd2d <- function(n = 5, m1 = 4, m2 = 10) {
  fdata(array(rnorm(n * m1 * m2), c(n, m1, m2)),
        argvals = list(seq(0, 1, length.out = m1), seq(0, 1, length.out = m2)),
        fdata2d = TRUE)
}

make_periodic_fdata <- function(n = 3, m = 200, period = 20, noise_sd = 0.1, seed = 42) {
  set.seed(seed)
  t <- seq(0, 1, length.out = m)
  X <- matrix(0, n, m)
  for (i in seq_len(n)) {
    X[i, ] <- sin(2 * pi * t * (m / period)) + rnorm(m, sd = noise_sd)
  }
  fdata(X, argvals = t)
}

# =============================================================================
# Basis function validation
# =============================================================================

test_that("basis.gcv validates input", {
  expect_error(basis.gcv(list(), nbasis = 5), "fdata")
  fd2d <- make_fd2d()
  expect_error(basis.gcv(fd2d, nbasis = 5), "2D")
})

test_that("basis.aic validates input", {
  expect_error(basis.aic(list(), nbasis = 5), "fdata")
  fd2d <- make_fd2d()
  expect_error(basis.aic(fd2d, nbasis = 5), "2D")
})

test_that("basis.bic validates input", {
  expect_error(basis.bic(list(), nbasis = 5), "fdata")
  fd2d <- make_fd2d()
  expect_error(basis.bic(fd2d, nbasis = 5), "2D")
})

test_that("fdata2basis_cv validates input", {
  expect_error(fdata2basis_cv(list(), nbasis.range = 4:20), "fdata")
  fd2d <- make_fd2d()
  expect_error(fdata2basis_cv(fd2d, nbasis.range = 4:20), "2D")
})

test_that("pspline validates input", {
  expect_error(pspline(list(), nbasis = 20), "fdata")
  fd2d <- make_fd2d()
  expect_error(pspline(fd2d, nbasis = 20), "2D")
})

test_that("pspline with lambda selection criteria", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(200), 10, 20), argvals = seq(0, 1, length.out = 20))
  ps_aic <- pspline(fd, lambda.select = TRUE, criterion = "AIC")
  expect_s3_class(ps_aic, "pspline")
  ps_bic <- pspline(fd, lambda.select = TRUE, criterion = "BIC")
  expect_s3_class(ps_bic, "pspline")
})

test_that("pspline preserves metadata", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(200), 10, 20), argvals = seq(0, 1, length.out = 20),
              metadata = data.frame(g = rep(c("A", "B"), 5)))
  ps <- pspline(fd, nbasis = 15)
  expect_equal(ps$fdata$metadata, fd$metadata)
})

test_that("pspline.2d validates input", {
  expect_error(pspline.2d(list()), "fdata")
  fd <- fdata(matrix(rnorm(100), 10, 10))
  expect_error(pspline.2d(fd), "2D")
})

test_that("pspline.2d with lambda selection", {
  set.seed(42)
  fd2d <- fdata(array(rnorm(500), c(5, 10, 10)),
                argvals = list(seq(0, 1, length.out = 10), seq(0, 1, length.out = 10)),
                fdata2d = TRUE)
  ps <- pspline.2d(fd2d, lambda.select = TRUE)
  expect_s3_class(ps, "pspline.2d")
  expect_output(print(ps), "P-spline")
  expect_no_error(plot(ps))
})

test_that("select.basis.auto validates input", {
  expect_error(select.basis.auto(list()), "fdata")
  fd2d <- make_fd2d()
  expect_error(select.basis.auto(fd2d), "2D")
})

test_that("fdata2basis_2d validates input", {
  expect_error(fdata2basis_2d(list()), "fdata")
  fd <- fdata(matrix(rnorm(100), 10, 10))
  expect_error(fdata2basis_2d(fd), "2D")
})

test_that("plot.basis.auto with pspline selection", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50), argvals = seq(0, 1, length.out = 50))
  ba <- select.basis.auto(fd)
  expect_no_error(plot(ba, which = "pspline"))
  expect_no_error(plot(ba, which = "fourier"))
})

# =============================================================================
# Seasonal analysis - detect.period method dispatch
# =============================================================================

test_that("detect.period with sazed method", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.period(fd, method = "sazed")
  # Returns sazed_result class
  expect_true(result$period > 0)
})

test_that("detect.period with autoperiod method", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.period(fd, method = "autoperiod")
  expect_true(result$period > 0)
})

test_that("detect.period with cfd method", {
  fd <- make_periodic_fdata(n = 3, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.period(fd, method = "cfd")
  expect_true(result$period > 0)
})

test_that("detect.period with fft method", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.period(fd, method = "fft")
  expect_s3_class(result, "period_estimate")
})

test_that("detect.period with acf method", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.period(fd, method = "acf")
  expect_s3_class(result, "period_estimate")
})

test_that("detect.period print method works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.period(fd, method = "fft")
  expect_output(print(result), "Period")
})

test_that("estimate.period validates input", {
  expect_error(estimate.period(list()), "fdata")
})

test_that("plot.matrix_profile_result with type=both", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- matrix.profile(fd)
  expect_no_error(plot(result, type = "both"))
})

test_that("plot.ssa_result with type=spectrum", {
  fd <- make_periodic_fdata(n = 1, m = 100, period = 20)
  result <- ssa.fd(fd)
  expect_no_error(plot(result, type = "spectrum"))
})

# Additional seasonal functions
test_that("detect.periods works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.periods(fd)
  expect_s3_class(result, "multiple_periods")
  expect_output(print(result), "Period")
})

test_that("detect.peaks works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.peaks(fd)
  expect_s3_class(result, "peak_detection")
  expect_output(print(result), "Peak")
})

test_that("seasonal.strength works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- seasonal.strength(fd, period = 20)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("classify.seasonality works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- classify.seasonality(fd, period = 20)
  expect_s3_class(result, "seasonality_classification")
  expect_output(print(result), "Seasonality")
})

test_that("detect.seasonality.changes works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.seasonality.changes(fd, period = 20)
  expect_output(print(result))
})

test_that("analyze.peak.timing works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- analyze.peak.timing(fd, period = 20)
  expect_s3_class(result, "peak_timing")
  expect_output(print(result), "Peak")
})

test_that("detrend works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detrend(fd, method = "linear")
  expect_s3_class(result, "fdata")
})

test_that("decompose works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- decompose(fd, period = 20)
  expect_true(is.list(result))
  expect_true("trend" %in% names(result))
  expect_true("seasonal" %in% names(result))
  expect_true("remainder" %in% names(result))
  expect_s3_class(result$trend, "fdata")
})

test_that("detect_amplitude_modulation works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect_amplitude_modulation(fd, period = 20)
  expect_s3_class(result, "amplitude_modulation")
  expect_output(print(result), "Amplitude")
  expect_no_error(plot(result))
})

test_that("detect.seasonality.changes.auto works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- detect.seasonality.changes.auto(fd, period = 20)
  expect_s3_class(result, "seasonality_changes_auto")
  expect_output(print(result))
})

test_that("instantaneous.period works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- instantaneous.period(fd)
  expect_true(is.list(result) || is.numeric(result))
})

test_that("seasonal.strength.curve works", {
  fd <- make_periodic_fdata(n = 1, m = 200, period = 20, noise_sd = 0.05)
  result <- seasonal.strength.curve(fd, period = 20)
  expect_true(!is.null(result))
})

# =============================================================================
# Outlier detection edge cases
# =============================================================================

test_that("outliers.lrt with seed=NULL works", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50))
  result <- outliers.lrt(fd, seed = NULL)
  expect_s3_class(result, "outliers.fdata")
})

test_that("outliers.lrt print shows threshold", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50))
  result <- outliers.lrt(fd, seed = 123)
  expect_output(print(result))
})

test_that("outliers.thres.lrt rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(outliers.thres.lrt(fd2d), "2D")
})

test_that("outliers.thres.lrt rejects invalid percentile", {
  fd <- fdata(matrix(rnorm(500), 10, 50))
  expect_error(outliers.thres.lrt(fd, percentile = 1.5))
})

test_that("outliers.lrt rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(outliers.lrt(fd2d), "2D")
})

test_that("magnitudeshape with label=index", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50))
  result <- magnitudeshape(fd, label = "index")
  expect_s3_class(result, "magnitudeshape")
})

test_that("magnitudeshape with label=id", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50), id = paste0("obs_", 1:10))
  result <- magnitudeshape(fd, label = "id")
  expect_s3_class(result, "magnitudeshape")
})

test_that("magnitudeshape with metadata label", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50),
              metadata = data.frame(patient_id = paste0("P", 1:10)))
  result <- magnitudeshape(fd, label = "patient_id")
  expect_s3_class(result, "magnitudeshape")
  expect_output(print(result), "Magnitude")
})

test_that("outliergram print with outlier types", {
  set.seed(42)
  X <- matrix(rnorm(500), 10, 50)
  X[1, ] <- X[1, ] + 10  # Create obvious outlier
  fd <- fdata(X)
  result <- outliergram(fd)
  expect_output(print(result), "Outliergram")
})

test_that("plot.outliergram works", {
  set.seed(42)
  X <- matrix(rnorm(500), 10, 50)
  X[1, ] <- X[1, ] + 10
  fd <- fdata(X)
  result <- outliergram(fd)
  expect_no_error(plot(result))
})

test_that("plot.outliers.fdata works", {
  set.seed(42)
  fd <- fdata(matrix(rnorm(500), 10, 50))
  result <- outliers.depth.pond(fd)
  expect_no_error(plot(result))
})

# =============================================================================
# Regression validation
# =============================================================================

test_that("fregre.pc rejects 2D fdata", {
  fd2d <- make_fd2d()
  expect_error(fregre.pc(fd2d, rnorm(5)), "2D")
})
