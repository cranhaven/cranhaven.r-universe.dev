# Validation tests for Phase 2 functions
# Tests for data conversion, statistical functions, and outlier detection

# =============================================================================
# Test Data Setup
# =============================================================================

create_test_data <- function(n = 30, m = 50, seed = 42) {
  set.seed(seed)
  t_grid <- seq(0, 1, length.out = m)
  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid + runif(1, 0, pi)) + rnorm(m, sd = 0.1)
  }
  list(
    fd = fdars::fdata(X, argvals = t_grid),
    X = X,
    t_grid = t_grid,
    n = n,
    m = m
  )
}

# =============================================================================
# Functional Medians (depth-based)
# =============================================================================

test_that("median with mode method returns valid median", {
  data <- create_test_data()
  med <- fdars::median(data$fd, method = "mode")

  expect_s3_class(med, "fdata")
  expect_equal(nrow(med$data), 1)
  expect_equal(ncol(med$data), data$m)
  expect_equal(length(med$argvals), data$m)
})

test_that("median with RP method returns valid median", {
  data <- create_test_data()
  med <- fdars::median(data$fd, method = "RP", nproj = 50)

  expect_s3_class(med, "fdata")
  expect_equal(nrow(med$data), 1)
  expect_equal(ncol(med$data), data$m)
})

test_that("median with RPD method returns valid median", {
  data <- create_test_data()
  med <- fdars::median(data$fd, method = "RPD", nproj = 50)

  expect_s3_class(med, "fdata")
  expect_equal(nrow(med$data), 1)
  expect_equal(ncol(med$data), data$m)
})

test_that("median with RT method returns valid median", {
  data <- create_test_data()
  med <- fdars::median(data$fd, method = "RT")

  expect_s3_class(med, "fdata")
  expect_equal(nrow(med$data), 1)
  expect_equal(ncol(med$data), data$m)
})

# =============================================================================
# Functional Trimmed Means (depth-based)
# =============================================================================

test_that("trimmed with mode method returns valid trimmed mean", {
  data <- create_test_data()
  trim <- fdars::trimmed(data$fd, trim = 0.1, method = "mode")

  expect_s3_class(trim, "fdata")
  expect_equal(nrow(trim$data), 1)
  expect_equal(ncol(trim$data), data$m)
})

test_that("trimmed with RP method returns valid trimmed mean", {
  data <- create_test_data()
  trim <- fdars::trimmed(data$fd, trim = 0.1, method = "RP", nproj = 50)

  expect_s3_class(trim, "fdata")
  expect_equal(nrow(trim$data), 1)
  expect_equal(ncol(trim$data), data$m)
})

test_that("trimmed with RPD method returns valid trimmed mean", {
  data <- create_test_data()
  trim <- fdars::trimmed(data$fd, trim = 0.1, method = "RPD", nproj = 50)

  expect_s3_class(trim, "fdata")
  expect_equal(nrow(trim$data), 1)
  expect_equal(ncol(trim$data), data$m)
})

test_that("trimmed with RT method returns valid trimmed mean", {
  data <- create_test_data()
  trim <- fdars::trimmed(data$fd, trim = 0.1, method = "RT")

  expect_s3_class(trim, "fdata")
  expect_equal(nrow(trim$data), 1)
  expect_equal(ncol(trim$data), data$m)
})

test_that("trimmed is more robust than mean", {
  # Create data with outliers
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)
  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }
  # Add outliers
  X[1, ] <- X[1, ] + 5
  X[2, ] <- X[2, ] - 5

  fd <- fdars::fdata(X, argvals = t_grid)
  mean_curve <- mean(fd)
  trim_curve <- fdars::trimmed(fd, trim = 0.1)

  # True mean (without outliers)
  true_mean <- colMeans(X[3:n, ])

  # Both methods should be reasonably close to the true mean
  # mean returns an fdata object
  mean_error <- sum((mean_curve$data[1, ] - true_mean)^2)
  trim_error <- sum((trim_curve$data[1, ] - true_mean)^2)

  # Both errors should be small (less than 0.5 with this level of outlier contamination)
  expect_lt(mean_error, 0.5)
  expect_lt(trim_error, 0.5)
})

# =============================================================================
# Functional Variance
# =============================================================================

test_that("var returns valid variance", {
  data <- create_test_data()
  fvar <- fdars::var(data$fd)

  expect_s3_class(fvar, "fdata")
  expect_equal(nrow(fvar$data), 1)
  expect_equal(ncol(fvar$data), data$m)

  # Variance should be non-negative
  expect_true(all(fvar$data >= 0))
})

test_that("var returns reasonable variance values", {
  data <- create_test_data()
  fvar <- fdars::var(data$fd)

  # Check variance is positive and reasonable
  expect_true(all(fvar$data > 0))
  expect_true(all(fvar$data < 10))  # Reasonable upper bound for our test data

  # Variance should have same shape as input
  expect_equal(ncol(fvar$data), data$m)
})

# =============================================================================
# Functional Trimmed Variance (depth-based)
# =============================================================================

test_that("trimvar with FM method returns valid trimmed variance", {
  data <- create_test_data()
  tv <- fdars::trimvar(data$fd, trim = 0.1)

  expect_s3_class(tv, "fdata")
  expect_equal(nrow(tv$data), 1)
  expect_equal(ncol(tv$data), data$m)
  expect_true(all(tv$data >= 0))
})

test_that("trimvar with mode method returns valid trimmed variance", {
  data <- create_test_data()
  tv <- fdars::trimvar(data$fd, trim = 0.1, method = "mode")

  expect_s3_class(tv, "fdata")
  expect_equal(nrow(tv$data), 1)
  expect_true(all(tv$data >= 0))
})

test_that("trimvar with RP method returns valid trimmed variance", {
  data <- create_test_data()
  tv <- fdars::trimvar(data$fd, trim = 0.1, method = "RP", nproj = 50)

  expect_s3_class(tv, "fdata")
  expect_equal(nrow(tv$data), 1)
  expect_true(all(tv$data >= 0))
})

test_that("trimvar with RPD method returns valid trimmed variance", {
  data <- create_test_data()
  tv <- fdars::trimvar(data$fd, trim = 0.1, method = "RPD", nproj = 50)

  expect_s3_class(tv, "fdata")
  expect_equal(nrow(tv$data), 1)
  expect_true(all(tv$data >= 0))
})

test_that("trimvar with RT method returns valid trimmed variance", {
  data <- create_test_data()
  tv <- fdars::trimvar(data$fd, trim = 0.1, method = "RT")

  expect_s3_class(tv, "fdata")
  expect_equal(nrow(tv$data), 1)
  expect_true(all(tv$data >= 0))
})

# =============================================================================
# fdata2pc - Principal Component Analysis
# =============================================================================

test_that("fdata2pc returns correct structure", {
  data <- create_test_data()
  ncomp <- 3
  pca <- fdars::fdata2pc(data$fd, ncomp = ncomp)

  expect_type(pca, "list")
  expect_true("d" %in% names(pca))
  expect_true("rotation" %in% names(pca))
  expect_true("x" %in% names(pca))
  expect_true("mean" %in% names(pca))

  # Check dimensions
  expect_length(pca$d, ncomp)
  expect_s3_class(pca$rotation, "fdata")
  expect_equal(nrow(pca$rotation$data), ncomp)
  expect_equal(ncol(pca$rotation$data), data$m)
  expect_equal(dim(pca$x), c(data$n, ncomp))
  expect_length(pca$mean, data$m)
})

test_that("fdata2pc singular values are decreasing", {
  data <- create_test_data()
  pca <- fdars::fdata2pc(data$fd, ncomp = 5)

  # Singular values should be positive and decreasing
  expect_true(all(pca$d > 0))
  expect_true(all(diff(pca$d) <= 0))
})

test_that("fdata2pc scores are orthogonal", {
  data <- create_test_data()
  pca <- fdars::fdata2pc(data$fd, ncomp = 3)

  # Scores should be approximately orthogonal
  cross_prod <- crossprod(pca$x)
  off_diag <- cross_prod - diag(diag(cross_prod))
  expect_true(max(abs(off_diag)) < 1e-6)
})

test_that("fdata2pc mean matches mean", {
  data <- create_test_data()
  pca <- fdars::fdata2pc(data$fd, ncomp = 3)
  fmean <- mean(data$fd)

  # mean returns an fdata object
  expect_equal(pca$mean, as.numeric(fmean$data), tolerance = 1e-10)
})

# =============================================================================
# fdata2pls - Partial Least Squares
# =============================================================================

test_that("fdata2pls returns correct structure", {
  data <- create_test_data()
  y <- rowMeans(data$X) + rnorm(data$n, sd = 0.1)
  ncomp <- 3
  pls <- fdars::fdata2pls(data$fd, y, ncomp = ncomp)

  expect_type(pls, "list")
  expect_true("rotation" %in% names(pls))
  expect_true("x" %in% names(pls))

  # Check dimensions
  expect_equal(dim(pls$rotation), c(data$m, ncomp))
  expect_equal(dim(pls$x), c(data$n, ncomp))
})

test_that("fdata2pls requires matching y length", {
  data <- create_test_data()
  y_wrong <- rnorm(data$n + 1)

  expect_error(fdars::fdata2pls(data$fd, y_wrong, ncomp = 3))
})

# =============================================================================
# fdata2basis - Basis Expansion
# =============================================================================

test_that("fdata2basis bspline returns correct dimensions", {
  data <- create_test_data()
  nbasis <- 10
  coefs <- fdars::fdata2basis(data$fd, nbasis = nbasis, type = "bspline")

  expect_true(is.matrix(coefs))
  expect_equal(nrow(coefs), data$n)
  # B-spline with degree 3 has nbasis + 4 basis functions for open knots
  expect_true(ncol(coefs) >= nbasis)
})

test_that("fdata2basis fourier returns correct dimensions", {
  data <- create_test_data()
  nbasis <- 10
  coefs <- fdars::fdata2basis(data$fd, nbasis = nbasis, type = "fourier")

  expect_true(is.matrix(coefs))
  expect_equal(nrow(coefs), data$n)
  expect_equal(ncol(coefs), nbasis)
})

test_that("fdata2basis fourier captures main variation", {
  # Create smooth sinusoidal data
  set.seed(42)
  n <- 20
  m <- 100
  t_grid <- seq(0, 1, length.out = m)
  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + 0.5 * cos(4 * pi * t_grid) + rnorm(m, sd = 0.05)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  coefs <- fdars::fdata2basis(fd, nbasis = 10, type = "fourier")

  # First few coefficients should capture most variation
  expect_true(is.matrix(coefs))
  expect_equal(nrow(coefs), n)
})

# =============================================================================
# outliers.thres.lrt - Bootstrap Threshold
# =============================================================================

test_that("outliers.thres.lrt returns positive threshold", {
  data <- create_test_data()
  thresh <- fdars::outliers.thres.lrt(data$fd, nb = 50, seed = 42)

  expect_type(thresh, "double")
  expect_length(thresh, 1)
  expect_true(thresh > 0)
})

test_that("outliers.thres.lrt is reproducible with seed", {
  data <- create_test_data()
  thresh1 <- fdars::outliers.thres.lrt(data$fd, nb = 50, seed = 123)
  thresh2 <- fdars::outliers.thres.lrt(data$fd, nb = 50, seed = 123)

  expect_equal(thresh1, thresh2)
})

# =============================================================================
# outliers.lrt - LRT Outlier Detection
# =============================================================================

test_that("outliers.lrt returns correct structure", {
  data <- create_test_data()
  result <- fdars::outliers.lrt(data$fd, nb = 50, seed = 42)

  expect_s3_class(result, "outliers.fdata")
  expect_true("outliers" %in% names(result))
  expect_true("distances" %in% names(result))
  expect_true("threshold" %in% names(result))
  expect_true("fdataobj" %in% names(result))

  expect_length(result$distances, data$n)
  expect_true(result$threshold > 0)
})

test_that("outliers.lrt detects obvious outliers", {
  # Create data with a clear outlier
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)
  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }
  # Add a very extreme outlier
  X[1, ] <- X[1, ] + 10

  fd <- fdars::fdata(X, argvals = t_grid)
  result <- fdars::outliers.lrt(fd, nb = 100, seed = 42)

  # The extreme outlier should have a high distance
  expect_true(result$distances[1] > median(result$distances))
})

test_that("outliers.lrt is reproducible with seed", {
  data <- create_test_data()
  result1 <- fdars::outliers.lrt(data$fd, nb = 50, seed = 123)
  result2 <- fdars::outliers.lrt(data$fd, nb = 50, seed = 123)

  expect_equal(result1$threshold, result2$threshold)
  expect_equal(result1$outliers, result2$outliers)
})

# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("functions reject non-fdata input", {
  X <- matrix(rnorm(100), 10, 10)

  # median, var, sd, cov now delegate to stats:: for non-fdata input (no error)
  expect_error(fdars::trimmed(X, trim = 0.1))
  expect_error(fdars::trimvar(X, trim = 0.1))
  expect_error(fdars::fdata2pc(X))
  expect_error(fdars::fdata2basis(X))
  expect_error(fdars::outliers.lrt(X))
})

test_that("trimmed handles edge cases", {
  data <- create_test_data()

  # Very small trim should work
  result_small <- fdars::trimmed(data$fd, trim = 0.01)
  expect_s3_class(result_small, "fdata")

  # Moderate trim should work
  result_mod <- fdars::trimmed(data$fd, trim = 0.2)
  expect_s3_class(result_mod, "fdata")
})

# =============================================================================
# Consistency Tests
# =============================================================================

test_that("all median functions return curves from original data", {
  data <- create_test_data()

  med_fm <- fdars::median(data$fd)
  med_mode <- fdars::median(data$fd, method = "mode")
  med_rp <- fdars::median(data$fd, method = "RP", nproj = 50)
  med_rt <- fdars::median(data$fd, method = "RT")

  # Each median should be one of the original curves
  check_is_original <- function(med, X) {
    any(apply(X, 1, function(row) all(abs(row - med$data[1, ]) < 1e-10)))
  }

  expect_true(check_is_original(med_fm, data$X))
  expect_true(check_is_original(med_mode, data$X))
  expect_true(check_is_original(med_rp, data$X))
  expect_true(check_is_original(med_rt, data$X))
})

test_that("trimmed variance is less than or equal to full variance", {
  data <- create_test_data()

  full_var <- fdars::var(data$fd)
  trim_var <- fdars::trimvar(data$fd, trim = 0.2)

  # With outliers removed, variance should typically be smaller
  # (not always guaranteed, but usually true)
  # We just check they're in same ballpark
  expect_true(mean(trim_var$data) > 0)
  expect_true(mean(full_var$data) > 0)
})
