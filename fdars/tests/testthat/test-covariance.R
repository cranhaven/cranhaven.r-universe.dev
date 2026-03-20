# Tests for covariance functions and Gaussian process generation

# =============================================================================
# Covariance Function Tests
# =============================================================================

test_that("kernel.gaussian produces valid covariance matrix", {
  cov_func <- kernel.gaussian(variance = 1, length_scale = 0.2)
  t <- seq(0, 1, length.out = 20)
  K <- cov_func(t)

  # Check dimensions

  expect_equal(dim(K), c(20, 20))

  # Symmetric
  expect_equal(K, t(K), tolerance = 1e-10)

  # Positive semi-definite (all eigenvalues >= 0)
  eig <- eigen(K, symmetric = TRUE)$values

expect_true(all(eig >= -1e-10))

  # Diagonal should equal variance
  expect_equal(diag(K)[1], 1, tolerance = 1e-10)

  # Off-diagonal should decay with distance
  expect_true(K[1, 2] > K[1, 10])
})

test_that("kernel.gaussian respects variance parameter", {
  cov_func1 <- kernel.gaussian(variance = 1, length_scale = 0.2)
  cov_func2 <- kernel.gaussian(variance = 2, length_scale = 0.2)
  t <- seq(0, 1, length.out = 10)

  K1 <- cov_func1(t)
  K2 <- cov_func2(t)

  # K2 should be 2x K1
  expect_equal(K2, 2 * K1, tolerance = 1e-10)
})

test_that("kernel.exponential produces valid covariance matrix", {
  cov_func <- kernel.exponential(variance = 1, length_scale = 0.2)
  t <- seq(0, 1, length.out = 20)
  K <- cov_func(t)

  # Check dimensions
  expect_equal(dim(K), c(20, 20))

  # Symmetric
  expect_equal(K, t(K), tolerance = 1e-10)

  # Positive semi-definite
  eig <- eigen(K, symmetric = TRUE)$values
  expect_true(all(eig >= -1e-10))
})

test_that("kernel.matern with nu=0.5 equals kernel.exponential", {
  t <- seq(0, 1, length.out = 20)

  K_matern <- kernel.matern(variance = 1, length_scale = 0.2, nu = 0.5)(t)
  K_exp <- kernel.exponential(variance = 1, length_scale = 0.2)(t)

  expect_equal(K_matern, K_exp, tolerance = 1e-10)
})

test_that("kernel.matern with nu=Inf equals kernel.gaussian", {
  t <- seq(0, 1, length.out = 20)

  K_matern <- kernel.matern(variance = 1, length_scale = 0.2, nu = Inf)(t)
  K_gauss <- kernel.gaussian(variance = 1, length_scale = 0.2)(t)

  expect_equal(K_matern, K_gauss, tolerance = 1e-10)
})

test_that("kernel.matern special cases work correctly", {
  t <- seq(0, 1, length.out = 20)

  # nu = 1.5 (Matern 3/2)
  K_15 <- kernel.matern(nu = 1.5)(t)
  expect_equal(dim(K_15), c(20, 20))
  expect_true(all(eigen(K_15, symmetric = TRUE)$values >= -1e-10))

  # nu = 2.5 (Matern 5/2)
  K_25 <- kernel.matern(nu = 2.5)(t)
  expect_equal(dim(K_25), c(20, 20))
  expect_true(all(eigen(K_25, symmetric = TRUE)$values >= -1e-10))
})

test_that("kernel.brownian produces valid covariance matrix", {
  cov_func <- kernel.brownian(variance = 1)
  t <- seq(0, 1, length.out = 20)
  K <- cov_func(t)

  # Check dimensions
  expect_equal(dim(K), c(20, 20))

  # Symmetric
  expect_equal(K, t(K), tolerance = 1e-10)

  # Brownian: K[i,j] = variance * min(t[i], t[j])
  expect_equal(K[5, 10], t[5], tolerance = 1e-10)
  expect_equal(K[10, 5], t[5], tolerance = 1e-10)
})

test_that("kernel.linear produces valid covariance matrix", {
  cov_func <- kernel.linear(variance = 1, offset = 0)
  t <- seq(0, 1, length.out = 20)
  K <- cov_func(t)

  # Check dimensions
  expect_equal(dim(K), c(20, 20))

  # Symmetric
  expect_equal(K, t(K), tolerance = 1e-10)

  # Linear: K[i,j] = variance * t[i] * t[j]
  expect_equal(K[5, 10], t[5] * t[10], tolerance = 1e-10)
})

test_that("kernel.polynomial produces valid covariance matrix", {
  cov_func <- kernel.polynomial(variance = 1, offset = 1, degree = 2)
  t <- seq(0, 1, length.out = 20)
  K <- cov_func(t)

  # Check dimensions
  expect_equal(dim(K), c(20, 20))

  # Symmetric
  expect_equal(K, t(K), tolerance = 1e-10)

  # Polynomial: K[i,j] = variance * (t[i] * t[j] + offset)^degree
  expect_equal(K[5, 10], (t[5] * t[10] + 1)^2, tolerance = 1e-10)
})

test_that("kernel.whitenoise produces diagonal matrix", {
  cov_func <- kernel.whitenoise(variance = 0.5)
  t <- seq(0, 1, length.out = 20)
  K <- cov_func(t)

  # Should be diagonal
  expect_equal(K, diag(0.5, 20), tolerance = 1e-10)
})

test_that("kernel.periodic produces valid periodic covariance", {
  cov_func <- kernel.periodic(variance = 1, length_scale = 0.5, period = 0.5)
  t <- seq(0, 1, length.out = 50)
  K <- cov_func(t)

  # Check dimensions
  expect_equal(dim(K), c(50, 50))

  # Symmetric
  expect_equal(K, t(K), tolerance = 1e-10)

  # Points separated by period should have same covariance as diagonal
  # (for identical points up to period)
  idx_0 <- which.min(abs(t - 0.2))
  idx_period <- which.min(abs(t - 0.7))  # 0.2 + 0.5 = 0.7
  expect_gt(K[idx_0, idx_period], 0.5)  # High correlation at period distance
})

test_that("kernel.add combines covariance functions", {
  cov_signal <- kernel.gaussian(variance = 1, length_scale = 0.2)
  cov_noise <- kernel.whitenoise(variance = 0.1)
  cov_total <- kernel.add(cov_signal, cov_noise)

  t <- seq(0, 1, length.out = 20)
  K_signal <- cov_signal(t)
  K_noise <- cov_noise(t)
  K_total <- cov_total(t)

  expect_equal(K_total, K_signal + K_noise, tolerance = 1e-10)
})

test_that("kernel.mult combines covariance functions", {
  cov1 <- kernel.gaussian(variance = 1, length_scale = 0.5)
  cov2 <- kernel.periodic(period = 0.3)
  cov_prod <- kernel.mult(cov1, cov2)

  t <- seq(0, 1, length.out = 20)
  K1 <- cov1(t)
  K2 <- cov2(t)
  K_prod <- cov_prod(t)

  expect_equal(K_prod, K1 * K2, tolerance = 1e-10)
})

# =============================================================================
# make.gaussian.process Tests
# =============================================================================

test_that("make.gaussian.process generates valid 1D fdata", {
  t <- seq(0, 1, length.out = 50)
  fd <- make.gaussian.process(n = 10, t = t,
                              cov = kernel.gaussian(length_scale = 0.2),
                              seed = 42)

  expect_s3_class(fd, "fdata")
  expect_equal(nrow(fd$data), 10)
  expect_equal(ncol(fd$data), 50)
  expect_equal(fd$argvals, t)
})

test_that("make.gaussian.process is reproducible with seed", {
  t <- seq(0, 1, length.out = 50)

  fd1 <- make.gaussian.process(n = 5, t = t,
                               cov = kernel.gaussian(length_scale = 0.2),
                               seed = 123)
  fd2 <- make.gaussian.process(n = 5, t = t,
                               cov = kernel.gaussian(length_scale = 0.2),
                               seed = 123)

  expect_equal(fd1$data, fd2$data)
})

test_that("make.gaussian.process produces different results without seed", {
  t <- seq(0, 1, length.out = 50)

  fd1 <- make.gaussian.process(n = 5, t = t, cov = kernel.gaussian())
  fd2 <- make.gaussian.process(n = 5, t = t, cov = kernel.gaussian())

  # Very unlikely to be exactly equal
  expect_false(all(fd1$data == fd2$data))
})

test_that("make.gaussian.process respects mean parameter", {
  t <- seq(0, 1, length.out = 100)

  # Scalar mean
  fd_scalar <- make.gaussian.process(n = 100, t = t,
                                     cov = kernel.gaussian(variance = 0.01),
                                     mean = 5, seed = 42)
  sample_mean <- mean(fd_scalar$data)
  expect_equal(sample_mean, 5, tolerance = 0.1)

  # Function mean
  mean_func <- function(t) sin(2 * pi * t)
  fd_func <- make.gaussian.process(n = 100, t = t,
                                   cov = kernel.gaussian(variance = 0.01),
                                   mean = mean_func, seed = 42)

  # Sample mean should approximate sin function
  empirical_mean <- colMeans(fd_func$data)
  expected_mean <- sin(2 * pi * t)
  expect_equal(empirical_mean, expected_mean, tolerance = 0.15)
})

test_that("make.gaussian.process generates 2D fdata", {
  s <- seq(0, 1, length.out = 15)
  t <- seq(0, 1, length.out = 20)

  fd <- make.gaussian.process(n = 5, t = list(s, t),
                              cov = kernel.gaussian(length_scale = 0.3),
                              seed = 42)

  expect_s3_class(fd, "fdata")
  expect_true(isTRUE(fd$fdata2d))
  # 2D fdata stores data as flattened matrix: n x (m1*m2)
  expect_equal(dim(fd$data), c(5, 15 * 20))
  expect_equal(fd$dims, c(15, 20))
})

test_that("make.gaussian.process works with different covariance functions", {
  t <- seq(0, 1, length.out = 50)

  # Gaussian - smooth samples
  fd_gauss <- make.gaussian.process(n = 5, t = t, cov = kernel.gaussian(), seed = 42)
  expect_s3_class(fd_gauss, "fdata")

  # Exponential - rough samples
  fd_exp <- make.gaussian.process(n = 5, t = t, cov = kernel.exponential(), seed = 42)
  expect_s3_class(fd_exp, "fdata")

  # Matern
  fd_matern <- make.gaussian.process(n = 5, t = t, cov = kernel.matern(nu = 1.5), seed = 42)
  expect_s3_class(fd_matern, "fdata")

  # Brownian
  fd_brown <- make.gaussian.process(n = 5, t = t, cov = kernel.brownian(), seed = 42)
  expect_s3_class(fd_brown, "fdata")
})

test_that("make.gaussian.process with combined covariance", {
  t <- seq(0, 1, length.out = 50)

  cov_combined <- kernel.add(kernel.gaussian(variance = 1, length_scale = 0.2),
                          kernel.whitenoise(variance = 0.05))

  fd <- make.gaussian.process(n = 5, t = t, cov = cov_combined, seed = 42)
  expect_s3_class(fd, "fdata")
})

# =============================================================================
# Print Method Tests
# =============================================================================

test_that("print.kernel works", {
  cov_func <- kernel.gaussian(variance = 2, length_scale = 0.5)

  expect_output(print(cov_func), "Covariance Kernel: gaussian")
  expect_output(print(cov_func), "variance = 2")
  expect_output(print(cov_func), "length_scale = 0.5")
})

# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("covariance functions reject invalid parameters", {
  expect_error(kernel.gaussian(variance = -1))
  expect_error(kernel.gaussian(variance = 0))
  expect_error(kernel.gaussian(length_scale = -1))

  expect_error(kernel.exponential(variance = -1))
  expect_error(kernel.matern(nu = -1))
  expect_error(kernel.brownian(variance = 0))
  expect_error(kernel.whitenoise(variance = -1))
  expect_error(kernel.periodic(period = -1))
  expect_error(kernel.polynomial(degree = 0))
})

test_that("kernel.brownian rejects 2D input", {
  cov_func <- kernel.brownian()
  expect_error(cov_func(list(seq(0, 1, 10), seq(0, 1, 10))))
})

test_that("kernel.periodic rejects 2D input", {
  cov_func <- kernel.periodic()
  expect_error(cov_func(list(seq(0, 1, 10), seq(0, 1, 10))))
})

test_that("make.gaussian.process rejects non-covariance function", {
  t <- seq(0, 1, length.out = 50)
  expect_error(make.gaussian.process(n = 5, t = t, cov = "invalid"))
})
