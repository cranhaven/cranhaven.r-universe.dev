# Tests for basis representation functions

test_that("basis2fdata reconstructs from coefficients", {
  t <- seq(0, 1, length.out = 100)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdata(X, argvals = t)

  # Test Fourier reconstruction
  coefs <- fdata2basis(fd, nbasis = 15, type = "fourier")
  fd_recon <- basis2fdata(coefs, argvals = t, type = "fourier")
  expect_lt(max(abs(fd$data - fd_recon$data)), 0.01)

  # Test B-spline reconstruction (higher tolerance due to boundary effects)
  coefs_bsp <- fdata2basis(fd, nbasis = 25, type = "bspline")
  fd_recon_bsp <- basis2fdata(coefs_bsp, argvals = t, type = "bspline")
  expect_lt(max(abs(fd$data - fd_recon_bsp$data)), 1.0)
})

test_that("basis.gcv returns finite values", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdata(X, argvals = t)

  gcv <- basis.gcv(fd, nbasis = 10, type = "fourier")
  expect_true(is.finite(gcv))
  expect_gte(gcv, 0)
})

test_that("basis.aic and basis.bic return finite values", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t) + rnorm(50, sd = 0.1), nrow = 1)
  fd <- fdata(X, argvals = t)

  aic <- basis.aic(fd, nbasis = 10, type = "fourier")
  bic <- basis.bic(fd, nbasis = 10, type = "fourier")

  expect_true(is.finite(aic))
  expect_true(is.finite(bic))
})

test_that("fdata2basis_cv selects optimal nbasis", {
  set.seed(42)
  t <- seq(0, 1, length.out = 50)
  X <- matrix(0, 10, 50)
  for (i in 1:10) X[i, ] <- sin(4 * pi * t) + rnorm(50, sd = 0.1)
  fd <- fdata(X, argvals = t)

  cv_result <- fdata2basis_cv(fd, nbasis.range = 5:15, type = "fourier")

  expect_s3_class(cv_result, "basis.cv")
  expect_true(cv_result$optimal.nbasis >= 5)
  expect_true(cv_result$optimal.nbasis <= 15)
  expect_equal(length(cv_result$scores), 11)
  expect_s3_class(cv_result$fitted, "fdata")
})

test_that("fdata2basis_cv works with different criteria", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(sin(2 * pi * t) + rnorm(30, sd = 0.1), nrow = 5, ncol = 30, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  cv_gcv <- fdata2basis_cv(fd, nbasis.range = 4:10, criterion = "GCV")
  cv_aic <- fdata2basis_cv(fd, nbasis.range = 4:10, criterion = "AIC")
  cv_bic <- fdata2basis_cv(fd, nbasis.range = 4:10, criterion = "BIC")

  expect_s3_class(cv_gcv, "basis.cv")
  expect_s3_class(cv_aic, "basis.cv")
  expect_s3_class(cv_bic, "basis.cv")
})

test_that("pspline smooths noisy data", {
  set.seed(42)
  t <- seq(0, 1, length.out = 100)
  true_signal <- sin(2 * pi * t)
  noisy <- true_signal + rnorm(100, sd = 0.3)
  fd <- fdata(matrix(noisy, nrow = 1), argvals = t)

  result <- pspline(fd, nbasis = 20, lambda = 10)

  expect_s3_class(result, "pspline")
  expect_s3_class(result$fdata, "fdata")
  expect_true(result$edf > 0)
  expect_true(result$edf < 20)
  expect_true(is.finite(result$gcv))

  # Check smoothing reduces noise (MSE to true signal should be lower)
  mse_noisy <- mean((fd$data - true_signal)^2)
  mse_smooth <- mean((result$fdata$data - true_signal)^2)
  expect_lt(mse_smooth, mse_noisy)
})

test_that("pspline with automatic lambda selection", {
  set.seed(42)
  t <- seq(0, 1, length.out = 50)
  noisy <- sin(2 * pi * t) + rnorm(50, sd = 0.2)
  fd <- fdata(matrix(noisy, nrow = 1), argvals = t)

  result <- pspline(fd, nbasis = 15, lambda.select = TRUE,
                    lambda.range = 10^seq(-2, 2, length.out = 20))

  expect_s3_class(result, "pspline")
  expect_true(result$lambda > 0)
})

test_that("print and plot methods work for basis.cv", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdata(X, argvals = t)

  cv_result <- fdata2basis_cv(fd, nbasis.range = 4:8)

  # Test print
  expect_output(print(cv_result), "Basis Cross-Validation")

  # Test plot (just check it doesn't error)
  expect_silent(suppressMessages(plot(cv_result)))
})

test_that("print method works for pspline", {
  t <- seq(0, 1, length.out = 50)
  fd <- fdata(matrix(sin(2 * pi * t), nrow = 1), argvals = t)
  result <- pspline(fd, nbasis = 10, lambda = 1)

  expect_output(print(result), "P-spline Smoothing")
})

test_that("basis functions handle multiple curves", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(0, 5, 50)
  for (i in 1:5) X[i, ] <- sin(2 * pi * i * t)
  fd <- fdata(X, argvals = t)

  # Test projection and reconstruction
  coefs <- fdata2basis(fd, nbasis = 15, type = "fourier")
  expect_equal(nrow(coefs), 5)

  fd_recon <- basis2fdata(coefs, argvals = t, type = "fourier")
  expect_equal(nrow(fd_recon$data), 5)

  # Test P-spline with multiple curves
  result <- pspline(fd, nbasis = 15, lambda = 1)
  expect_equal(nrow(result$fdata$data), 5)
})

test_that("basis.gcv with penalty parameter", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t) + rnorm(50, sd = 0.1), nrow = 1)
  fd <- fdata(X, argvals = t)

  # Small lambda instead of zero (zero can cause numerical issues with near-perfect fit)
  gcv_small_pen <- basis.gcv(fd, nbasis = 10, lambda = 0.001)
  gcv_pen <- basis.gcv(fd, nbasis = 10, lambda = 10)

  # Both should be finite
  expect_true(is.finite(gcv_small_pen))
  expect_true(is.finite(gcv_pen))
})

# ==============================================================================
# Tests for select.basis.auto
# ==============================================================================

test_that("select.basis.auto returns correct structure", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)

  # Create simple test data
  X <- matrix(0, 3, 100)
  for (i in 1:3) X[i, ] <- sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd)

  # Check class
  expect_s3_class(result, "basis.auto")

  # Check all components exist
  expect_true("basis.type" %in% names(result))
  expect_true("nbasis" %in% names(result))
  expect_true("score" %in% names(result))
  expect_true("coefficients" %in% names(result))
  expect_true("fitted" %in% names(result))
  expect_true("edf" %in% names(result))
  expect_true("seasonal.detected" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_true("criterion" %in% names(result))

  # Check lengths match number of curves
  expect_equal(length(result$basis.type), 3)
  expect_equal(length(result$nbasis), 3)
  expect_equal(length(result$score), 3)
  expect_equal(length(result$edf), 3)

  # Check fitted is fdata
  expect_s3_class(result$fitted, "fdata")
  expect_equal(nrow(result$fitted$data), 3)
})

test_that("select.basis.auto selects Fourier for sinusoidal curves", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)

  # Pure sinusoidal signal
  X <- matrix(0, 5, 100)
  for (i in 1:5) X[i, ] <- sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.1)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd, criterion = "GCV")

  # Most or all should be Fourier
  n_fourier <- sum(result$basis.type == "fourier")
  expect_gte(n_fourier, 3)  # At least 3 of 5 should be Fourier
})

test_that("select.basis.auto handles mixed data types", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)

  # 2 seasonal curves
  X_seasonal <- matrix(0, 2, 100)
  for (i in 1:2) X_seasonal[i, ] <- sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.1)

  # 2 polynomial curves
  X_poly <- matrix(0, 2, 100)
  for (i in 1:2) X_poly[i, ] <- 0.1 * t^2 - t + rnorm(100, sd = 0.5)

  fd <- fdata(rbind(X_seasonal, X_poly), argvals = t)

  result <- select.basis.auto(fd)

  # Should have a mix of types
  expect_equal(length(result$basis.type), 4)
  expect_true("fourier" %in% result$basis.type || "pspline" %in% result$basis.type)
})

test_that("select.basis.auto works with different criteria", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2), nrow = 2, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  result_gcv <- select.basis.auto(fd, criterion = "GCV")
  result_aic <- select.basis.auto(fd, criterion = "AIC")
  result_bic <- select.basis.auto(fd, criterion = "BIC")

  expect_equal(result_gcv$criterion, "GCV")
  expect_equal(result_aic$criterion, "AIC")
  expect_equal(result_bic$criterion, "BIC")

  # All scores should be finite
  expect_true(all(is.finite(result_gcv$score)))
  expect_true(all(is.finite(result_aic$score)))
  expect_true(all(is.finite(result_bic$score)))
})

test_that("select.basis.auto with fixed lambda", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(0.1 * t^2 + rnorm(100, sd = 0.5), nrow = 2, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd, lambda.pspline = 1.0)

  # Lambda values for P-spline curves should be 1.0
  pspline_idx <- result$basis.type == "pspline"
  if (any(pspline_idx)) {
    expect_true(all(result$lambda[pspline_idx] == 1.0))
  }
})

test_that("select.basis.auto with custom nbasis range", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2), nrow = 2, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd, nbasis.range = c(5, 15))

  # All nbasis should be in range
  expect_true(all(result$nbasis >= 5))
  expect_true(all(result$nbasis <= 15))
})

test_that("select.basis.auto seasonal detection", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)

  # Strongly seasonal curve
  X_seasonal <- sin(2 * pi * t / 2.5)
  # Non-seasonal curve
  X_nonseasonal <- t + rnorm(100, sd = 0.1)

  fd <- fdata(rbind(X_seasonal, X_nonseasonal), argvals = t)

  result <- select.basis.auto(fd, use.seasonal.hint = TRUE)

  # First curve should be detected as seasonal
  expect_true(result$seasonal.detected[1])
})

test_that("print method works for basis.auto", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2), nrow = 3, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd)

  expect_output(print(result), "Automatic Basis Selection")
  expect_output(print(result), "Curves:")
})

test_that("summary method works for basis.auto", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2), nrow = 3, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd)

  # Summary should return a data frame invisibly
  expect_output(summary_df <- summary(result), "Automatic Basis Selection Summary")
  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), 3)
})

test_that("plot method works for basis.auto", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2), nrow = 3, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  result <- select.basis.auto(fd)

  # Plot should return a ggplot object
  p <- plot(result)
  expect_s3_class(p, "ggplot")

  # Test plot with subset
  p_fourier <- plot(result, which = "fourier")
  if (!is.null(p_fourier)) {
    expect_s3_class(p_fourier, "ggplot")
  }
})

test_that("select.basis.auto preserves metadata", {
  set.seed(42)
  t <- seq(0, 10, length.out = 100)
  X <- matrix(sin(2 * pi * t / 2.5) + rnorm(100, sd = 0.2), nrow = 2, ncol = 100, byrow = TRUE)
  fd <- fdata(X, argvals = t)
  fd$id <- c("curve1", "curve2")
  fd$metadata <- list(source = "test")

  result <- select.basis.auto(fd)

  # Metadata should be preserved in fitted
  expect_equal(result$fitted$id, fd$id)
  expect_equal(result$fitted$metadata, fd$metadata)
})

# ==============================================================================
# Additional tests for basis.R coverage
# ==============================================================================

test_that("basis2fdata with vector input", {
  t <- seq(0, 1, length.out = 100)
  # Single curve as vector
  coefs <- rnorm(10)
  fd <- fdars::basis2fdata(coefs, argvals = t, type = "bspline")

  expect_s3_class(fd, "fdata")
  expect_equal(nrow(fd$data), 1)
})

test_that("basis.gcv with bspline", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t) + rnorm(50, sd = 0.1), nrow = 2, ncol = 50, byrow = TRUE)
  fd <- fdars::fdata(X, argvals = t)

  gcv <- fdars::basis.gcv(fd, nbasis = 10, type = "bspline")
  expect_true(is.finite(gcv))
})

test_that("basis.aic with bspline", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t) + rnorm(50, sd = 0.1), nrow = 2, ncol = 50, byrow = TRUE)
  fd <- fdars::fdata(X, argvals = t)

  aic <- fdars::basis.aic(fd, nbasis = 10, type = "bspline")
  expect_true(is.finite(aic))
})

test_that("basis.bic with bspline", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t) + rnorm(50, sd = 0.1), nrow = 2, ncol = 50, byrow = TRUE)
  fd <- fdars::fdata(X, argvals = t)

  bic <- fdars::basis.bic(fd, nbasis = 10, type = "bspline")
  expect_true(is.finite(bic))
})

test_that("fdata2basis_cv with CV criterion", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(0, 10, 30)
  for (i in 1:10) X[i, ] <- sin(2 * pi * t) + rnorm(30, sd = 0.1)
  fd <- fdars::fdata(X, argvals = t)

  cv_result <- fdars::fdata2basis_cv(fd, nbasis.range = 5:10, criterion = "CV", kfold = 5)
  expect_s3_class(cv_result, "basis.cv")
})

test_that("plot method works for pspline", {
  t <- seq(0, 1, length.out = 50)
  fd <- fdars::fdata(matrix(sin(2 * pi * t), nrow = 1), argvals = t)
  result <- fdars::pspline(fd, nbasis = 10, lambda = 1)

  expect_no_error(plot(result))
})

test_that("pspline with order 3", {
  set.seed(42)
  t <- seq(0, 1, length.out = 100)
  noisy <- sin(2 * pi * t) + rnorm(100, sd = 0.3)
  fd <- fdars::fdata(matrix(noisy, nrow = 1), argvals = t)

  result <- fdars::pspline(fd, nbasis = 20, lambda = 10, order = 3)
  expect_s3_class(result, "pspline")
})

test_that("fdata2basis with bspline type", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdars::fdata(X, argvals = t)

  coefs <- fdars::fdata2basis(fd, nbasis = 10, type = "bspline")
  expect_true(is.matrix(coefs))
  expect_equal(ncol(coefs), 10)
})

test_that("basis2fdata with custom rangeval", {
  t <- seq(0, 2, length.out = 100)
  coefs <- matrix(rnorm(10), nrow = 1)

  fd <- fdars::basis2fdata(coefs, argvals = t, type = "fourier", rangeval = c(0, 2))
  expect_s3_class(fd, "fdata")
  expect_equal(fd$rangeval, c(0, 2))
})

test_that("basis.gcv with pooled = FALSE", {
  set.seed(42)
  t <- seq(0, 1, length.out = 50)
  X <- matrix(0, 5, 50)
  for (i in 1:5) X[i, ] <- sin(2 * pi * t) + rnorm(50, sd = 0.1)
  fd <- fdars::fdata(X, argvals = t)

  gcv <- fdars::basis.gcv(fd, nbasis = 10, type = "fourier", pooled = FALSE)
  expect_true(is.finite(gcv))
})

test_that("basis.aic with pooled = FALSE", {
  set.seed(42)
  t <- seq(0, 1, length.out = 50)
  X <- matrix(0, 5, 50)
  for (i in 1:5) X[i, ] <- sin(2 * pi * t) + rnorm(50, sd = 0.1)
  fd <- fdars::fdata(X, argvals = t)

  aic <- fdars::basis.aic(fd, nbasis = 10, pooled = FALSE)
  expect_true(is.finite(aic))
})

test_that("pspline.2d for 2D functional data", {
  set.seed(42)
  # Create 2D functional data with double argvals
  X <- array(rnorm(500), dim = c(5, 10, 10))
  argvals_s <- as.double(seq(0, 1, length.out = 10))
  argvals_t <- as.double(seq(0, 1, length.out = 10))
  fd <- fdars::fdata(X, argvals = list(argvals_s, argvals_t), fdata2d = TRUE)

  # Check if function exists and handles 2D data
  if ("pspline.2d" %in% getNamespaceExports("fdars")) {
    result <- fdars::pspline.2d(fd)
    expect_s3_class(result, "pspline.2d")
  }
})
