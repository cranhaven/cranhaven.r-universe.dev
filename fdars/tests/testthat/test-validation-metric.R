# Validation tests for metric functions
# Compare fdars results with fda.usc reference implementation

test_that("metric.lp matches fda.usc implementation", {
  skip_if_not_installed("fda.usc")

  set.seed(42)
  n <- 20
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid * (1 + 0.1 * i / n)) + rnorm(m, sd = 0.1)
  }

  fd_orig <- fda.usc::fdata(X, argvals = t_grid)
  fd_rust <- fdars::fdata(X, argvals = t_grid)

  # L2 metric
  D_orig <- fda.usc::metric.lp(fd_orig, lp = 2)
  D_rust <- fdars::metric.lp(fd_rust, lp = 2)

  # Compare values ignoring attributes (fda.usc adds metadata attributes)
  expect_equal(as.matrix(D_orig), D_rust, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("metric.hausdorff matches fda.usc implementation", {
  skip_if_not_installed("fda.usc")

  set.seed(42)
  n <- 20
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd_orig <- fda.usc::fdata(X, argvals = t_grid)
  fd_rust <- fdars::fdata(X, argvals = t_grid)

  D_orig <- fda.usc::metric.hausdorff(fd_orig)
  D_rust <- fdars::metric.hausdorff(fd_rust)

  # Compare values ignoring attributes (fda.usc adds metadata attributes)
  expect_equal(as.matrix(D_orig), D_rust, tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("metric.DTW produces valid distances", {
  set.seed(42)
  n <- 15
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid + 0.1 * i) + rnorm(m, sd = 0.05)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::metric.DTW(fd)

  # DTW distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero
  expect_true(all(diag(D) == 0))
  # Symmetric
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("semimetric.pca produces valid distances", {
  set.seed(42)
  n <- 20
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::semimetric.pca(fd, ncomp = 3)

  # Distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero
  expect_true(all(diag(D) == 0))
  # Symmetric
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("semimetric.deriv produces valid distances", {
  set.seed(42)
  n <- 20
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::semimetric.deriv(fd, nderiv = 1)

  # Distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero
  expect_true(all(diag(D) == 0))
  # Symmetric
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("semimetric.basis produces valid distances", {
  set.seed(42)
  n <- 20
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::semimetric.basis(fd, nbasis = 10)

  # Distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero (or very small)
  expect_true(all(abs(diag(D)) < 1e-10))
  # Symmetric
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("semimetric.fourier produces valid distances", {
  set.seed(42)
  n <- 20
  m <- 64  # Power of 2 for FFT
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::semimetric.fourier(fd, nfreq = 5)

  # Distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero
  expect_true(all(abs(diag(D)) < 1e-10))
  # Symmetric
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("semimetric.hshift produces valid distances", {
  set.seed(42)
  n <- 15
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid + 0.1 * i) + rnorm(m, sd = 0.05)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::semimetric.hshift(fd, max_shift = 5)

  # Distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero
  expect_true(all(diag(D) == 0))
  # Symmetric
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("metric.kl produces valid distances", {
  set.seed(42)
  n <- 20
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  # Create positive curves for KL
  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- abs(sin(2 * pi * t_grid) + 0.5 + rnorm(m, sd = 0.1))
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::metric.kl(fd)

  # KL distances should be non-negative
  expect_true(all(D >= 0))
  # Diagonal should be zero
  expect_true(all(abs(diag(D)) < 1e-10))
  # Symmetric (due to symmetrization)
  expect_equal(D, t(D), tolerance = 1e-10)
})

test_that("metric dispatches correctly", {
  set.seed(42)
  n <- 10
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)

  # Test various methods
  D_lp <- fdars::metric(fd, method = "lp")
  D_hausdorff <- fdars::metric(fd, method = "hausdorff")
  D_dtw <- fdars::metric(fd, method = "dtw")
  D_pca <- fdars::metric(fd, method = "pca")

  # All should be matrices of correct size
  expect_equal(dim(D_lp), c(n, n))
  expect_equal(dim(D_hausdorff), c(n, n))
  expect_equal(dim(D_dtw), c(n, n))
  expect_equal(dim(D_pca), c(n, n))
})

# ==============================================================================
# Additional tests for better coverage
# ==============================================================================

test_that("metric.lp cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n1 * m), n1, m)
  X2 <- matrix(rnorm(n2 * m), n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::metric.lp(fd1, fd2, p = 2)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("metric.lp with weights works", {
  set.seed(42)
  n <- 10
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  # Uniform weights
  D1 <- fdars::metric.lp(fd, w = rep(1, m))

  # Non-uniform weights
  w <- seq(1, 2, length.out = m)
  D2 <- fdars::metric.lp(fd, w = w)

  expect_equal(dim(D1), c(n, n))
  expect_equal(dim(D2), c(n, n))
  # Weighted distances should differ
  expect_false(isTRUE(all.equal(D1, D2)))
})

test_that("metric.lp L1 and Linf work", {
  set.seed(42)
  n <- 10
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_L1 <- fdars::metric.lp(fd, p = 1)
  D_Linf <- fdars::metric.lp(fd, p = Inf)

  expect_equal(dim(D_L1), c(n, n))
  expect_equal(dim(D_Linf), c(n, n))
  expect_true(all(D_L1 >= 0))
  expect_true(all(D_Linf >= 0))
})

test_that("metric.hausdorff cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n1 * m), n1, m)
  X2 <- matrix(rnorm(n2 * m), n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::metric.hausdorff(fd1, fd2)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("metric.DTW cross-distances work", {
  set.seed(42)
  n1 <- 8
  n2 <- 6
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n1 * m), n1, m)
  X2 <- matrix(rnorm(n2 * m), n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::metric.DTW(fd1, fd2, p = 2)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("metric.DTW with window constraint works", {
  set.seed(42)
  n <- 10
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_narrow <- fdars::metric.DTW(fd, w = 5)
  D_wide <- fdars::metric.DTW(fd, w = 15)

  expect_equal(dim(D_narrow), c(n, n))
  expect_equal(dim(D_wide), c(n, n))
})

test_that("semimetric.pca cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n1 * m), n1, m)
  X2 <- matrix(rnorm(n2 * m), n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::semimetric.pca(fd1, fd2, ncomp = 3)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("semimetric.deriv cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(0, n1, m)
  X2 <- matrix(0, n2, m)
  for (i in 1:n1) X1[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.05)
  for (i in 1:n2) X2[i, ] <- sin(2 * pi * t_grid + 0.5) + rnorm(m, sd = 0.05)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::semimetric.deriv(fd1, fd2, nderiv = 1)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("semimetric.basis with fourier basis works", {
  set.seed(42)
  n <- 15
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::semimetric.basis(fd, nbasis = 11, basis = "fourier")

  expect_equal(dim(D), c(n, n))
  expect_true(all(D >= 0))
})

test_that("semimetric.basis cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n1 * m), n1, m)
  X2 <- matrix(rnorm(n2 * m), n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::semimetric.basis(fd1, fd2, nbasis = 10)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("semimetric.fourier cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 64
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(n1 * m), n1, m)
  X2 <- matrix(rnorm(n2 * m), n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::semimetric.fourier(fd1, fd2, nfreq = 10)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("semimetric.hshift cross-distances work", {
  set.seed(42)
  n1 <- 8
  n2 <- 6
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(0, n1, m)
  X2 <- matrix(0, n2, m)
  for (i in 1:n1) X1[i, ] <- sin(2 * pi * t_grid + 0.1 * i)
  for (i in 1:n2) X2[i, ] <- sin(2 * pi * t_grid + 0.2 * i)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::semimetric.hshift(fd1, fd2, max_shift = 10)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("metric.kl cross-distances work", {
  set.seed(42)
  n1 <- 10
  n2 <- 8
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(abs(rnorm(n1 * m)) + 0.5, n1, m)
  X2 <- matrix(abs(rnorm(n2 * m)) + 0.5, n2, m)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::metric.kl(fd1, fd2)

  expect_equal(dim(D), c(n1, n2))
  expect_true(all(D >= 0))
})

test_that("metric.kl without normalization works", {
  set.seed(42)
  n <- 10
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(abs(rnorm(n * m)) + 0.5, n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D <- fdars::metric.kl(fd, normalize = FALSE)

  expect_equal(dim(D), c(n, n))
  expect_true(all(D >= 0))
})

test_that("metric dispatcher with all methods works", {
  set.seed(42)
  n <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(abs(rnorm(n * m)) + 0.5, n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  # Test all methods via dispatcher
  D_deriv <- fdars::metric(fd, method = "deriv")
  D_basis <- fdars::metric(fd, method = "basis")
  D_fourier <- fdars::metric(fd, method = "fourier")
  D_hshift <- fdars::metric(fd, method = "hshift")
  D_kl <- fdars::metric(fd, method = "kl")

  expect_equal(dim(D_deriv), c(n, n))
  expect_equal(dim(D_basis), c(n, n))
  expect_equal(dim(D_fourier), c(n, n))
  expect_equal(dim(D_hshift), c(n, n))
  expect_equal(dim(D_kl), c(n, n))
})

# ==============================================================================
# Additional tests to improve coverage
# ==============================================================================

test_that("metric dispatcher with lp and other parameters", {
  set.seed(42)
  n <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  # Test lp with different p values
  D_l1 <- fdars::metric(fd, method = "lp", p = 1)
  D_l2 <- fdars::metric(fd, method = "lp", p = 2)
  D_linf <- fdars::metric(fd, method = "lp", p = Inf)

  expect_equal(dim(D_l1), c(n, n))
  expect_equal(dim(D_l2), c(n, n))
  expect_equal(dim(D_linf), c(n, n))
})

test_that("metric dispatcher with dtw", {
  set.seed(42)
  n <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_dtw <- fdars::metric(fd, method = "dtw")
  expect_equal(dim(D_dtw), c(n, n))
})

test_that("metric dispatcher with hausdorff", {
  set.seed(42)
  n <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_haus <- fdars::metric(fd, method = "hausdorff")
  expect_equal(dim(D_haus), c(n, n))
})

test_that("metric dispatcher with pca", {
  set.seed(42)
  n <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_pca <- fdars::metric(fd, method = "pca")
  expect_equal(dim(D_pca), c(n, n))
})

test_that("semimetric functions with different ncomp/nbasis", {
  set.seed(42)
  n <- 10
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  # PCA with different ncomp
  D_pca3 <- fdars::semimetric.pca(fd, ncomp = 3)
  D_pca5 <- fdars::semimetric.pca(fd, ncomp = 5)
  expect_equal(dim(D_pca3), c(n, n))
  expect_equal(dim(D_pca5), c(n, n))

  # Basis with different nbasis
  D_basis8 <- fdars::semimetric.basis(fd, nbasis = 8)
  D_basis12 <- fdars::semimetric.basis(fd, nbasis = 12)
  expect_equal(dim(D_basis8), c(n, n))
  expect_equal(dim(D_basis12), c(n, n))
})

test_that("metric.lp with L-infinity norm", {
  set.seed(42)
  n <- 10
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_inf <- fdars::metric.lp(fd, p = Inf)
  expect_equal(dim(D_inf), c(n, n))
  expect_true(all(diag(D_inf) == 0))
})

test_that("metric.DTW with different window sizes", {
  set.seed(42)
  n <- 8
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  D_w5 <- fdars::metric.DTW(fd, window = 5)
  D_w10 <- fdars::metric.DTW(fd, window = 10)

  expect_equal(dim(D_w5), c(n, n))
  expect_equal(dim(D_w10), c(n, n))
})

test_that("metric.lp with 2D functional data", {
  set.seed(42)
  n <- 5
  m1 <- 10
  m2 <- 10

  # Create 2D fdata (surfaces)
  X <- array(rnorm(n * m1 * m2), dim = c(n, m1, m2))
  fd <- fdars::fdata(X, argvals = list(1:m1, 1:m2), fdata2d = TRUE)

  D <- fdars::metric.lp(fd)
  expect_equal(dim(D), c(n, n))
  expect_true(all(diag(D) == 0))
})

test_that("metric.lp cross-distances 2D", {
  set.seed(42)
  n1 <- 4
  n2 <- 3
  m1 <- 10
  m2 <- 10

  X1 <- array(rnorm(n1 * m1 * m2), dim = c(n1, m1, m2))
  X2 <- array(rnorm(n2 * m1 * m2), dim = c(n2, m1, m2))

  fd1 <- fdars::fdata(X1, argvals = list(1:m1, 1:m2), fdata2d = TRUE)
  fd2 <- fdars::fdata(X2, argvals = list(1:m1, 1:m2), fdata2d = TRUE)

  D <- fdars::metric.lp(fd1, fd2)
  expect_equal(dim(D), c(n1, n2))
})

test_that("metric.lp with weight vector", {
  set.seed(42)
  n <- 10
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  w <- rep(1, m)
  D <- fdars::metric.lp(fd, w = w)
  expect_equal(dim(D), c(n, n))
})
