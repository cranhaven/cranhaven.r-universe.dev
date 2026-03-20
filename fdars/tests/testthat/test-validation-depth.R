# Validation tests for depth functions
# Compare fdars results with fda.usc reference implementation

test_that("depth with method='FM' matches fda.usc implementation", {
  skip_if_not_installed("fda.usc")

  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid * (1 + 0.1 * i / n)) + rnorm(m, sd = 0.1)
  }

  fd_orig <- fda.usc::fdata(X, argvals = t_grid)
  fd_rust <- fdars::fdata(X, argvals = t_grid)

  D_orig <- fda.usc::depth.FM(fd_orig)$dep
  D_rust <- fdars::depth(fd_rust, method = "FM")

  # Compare ignoring names attribute
  expect_equal(as.numeric(D_orig), D_rust, tolerance = 1e-6)
})

test_that("depth with method='mode' produces valid depths", {
  # Note: fda.usc and fdars may have different mode depth implementations
  # (different kernel normalizations). We test correctness properties instead.
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd_rust <- fdars::fdata(X, argvals = t_grid)
  h <- 0.5
  D_rust <- fdars::depth(fd_rust, method = "mode", h = h)

  # Depths should be positive (mode depth not bounded to [0,1])
  expect_true(all(D_rust >= 0))
  expect_length(D_rust, n)

  # The deepest curve should be near the center
  deepest_idx <- which.max(D_rust)
  deepest_curve <- X[deepest_idx, ]
  mean_curve <- colMeans(X)
  # Deepest should be closer to mean than random outer curves
  expect_true(sum((deepest_curve - mean_curve)^2) < quantile(rowSums((X - rep(mean_curve, each = n))^2), 0.5))
})

test_that("depth with method='RP' produces valid depths", {
  # RP depth uses random projections - different implementations may not correlate well
  # because random projections are generated independently
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd_rust <- fdars::fdata(X, argvals = t_grid)
  nproj <- 50
  D_rust <- fdars::depth(fd_rust, method = "RP", nproj = nproj)

  # Depths should be in [0, 1]
  expect_true(all(D_rust >= 0 & D_rust <= 1))
  expect_length(D_rust, n)

  # RP depth uses random projections so the deepest curve may not always

  # be near the sample mean. Instead check that depths are not degenerate:
  # there should be meaningful variation in the depth values.
  expect_true(sd(D_rust) > 0)
})

test_that("depth with method='RT' produces valid depths", {
  # Note: RT depth implementations may differ in tie-breaking behavior
  # We test correctness properties instead of exact matching
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd_rust <- fdars::fdata(X, argvals = t_grid)
  D_rust <- fdars::depth(fd_rust, method = "RT")

  # Depths should be in [0, 1]
  expect_true(all(D_rust >= 0 & D_rust <= 1))
  expect_length(D_rust, n)

  # RT depth should give reasonable central curves high depth
  mean_curve <- colMeans(X)
  all_dists <- rowSums((X - matrix(mean_curve, n, m, byrow = TRUE))^2)
  # Top 30% by depth should overlap with inner 70% by distance to mean
  top_depth_idx <- order(D_rust, decreasing = TRUE)[1:floor(n * 0.3)]
  inner_dist_idx <- order(all_dists)[1:floor(n * 0.7)]
  expect_gt(length(intersect(top_depth_idx, inner_dist_idx)), 0)
})

test_that("depth with method='FSD' produces valid depths", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth(fd, method = "FSD")

  # Depths should be in [0, 1]
  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth with method='KFSD' produces valid depths", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth(fd, method = "KFSD", h = 0.5)

  # Depths should be in [0, 1]
  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth with method='RPD' produces valid depths", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth(fd, method = "RPD", nproj = 50, deriv = c(0, 1))

  # Depths should be in [0, 1]
  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("median returns valid median", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  med <- fdars::median(fd)

  expect_s3_class(med, "fdata")
  expect_equal(nrow(med$data), 1)
  expect_equal(ncol(med$data), m)
})

test_that("trimmed returns valid trimmed mean", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  trimmed <- fdars::trimmed(fd, trim = 0.1)

  expect_s3_class(trimmed, "fdata")
  expect_equal(nrow(trimmed$data), 1)
  expect_equal(ncol(trimmed$data), m)
})

# ==============================================================================
# Additional tests for better coverage
# ==============================================================================

test_that("depth with method='BD' produces valid depths", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth(fd, method = "BD")

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth with method='MBD' produces valid depths", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth(fd, method = "MBD")

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth with method='MEI' produces valid depths", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth(fd, method = "MEI")

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.FM direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.FM(fd)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.BD direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.BD(fd)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.MBD direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.MBD(fd)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.MEI direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.MEI(fd)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.RP direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.RP(fd, nproj = 30)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.RT direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.RT(fd)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.FSD direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.FSD(fd)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.KFSD direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.KFSD(fd, h = 0.5)

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("depth.RPD direct call works", {
  set.seed(42)
  n <- 20
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  D <- fdars::depth.RPD(fd, nproj = 30, deriv = c(0, 1))

  expect_true(all(D >= 0 & D <= 1))
  expect_length(D, n)
})

test_that("var.fdata returns valid variance", {
  set.seed(42)
  n <- 30
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  v <- fdars::var(fd)

  expect_s3_class(v, "fdata")
  expect_true(all(v$data >= 0))  # Variance must be non-negative
})

test_that("sd.fdata returns valid standard deviation", {
  set.seed(42)
  n <- 30
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  s <- fdars::sd(fd)

  expect_s3_class(s, "fdata")
  expect_true(all(s$data >= 0))  # SD must be non-negative
})

test_that("cov.fdata returns valid covariance", {
  set.seed(42)
  n <- 30
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  cv <- fdars::cov(fd)

  # cov.fdata returns a list with $cov element
  expect_true(is.list(cv))
  expect_true("cov" %in% names(cv))
  expect_true(is.matrix(cv$cov))
  expect_equal(dim(cv$cov), c(m, m))
  # Covariance matrix should be symmetric
  expect_equal(cv$cov, t(cv$cov), tolerance = 1e-10)
})

test_that("gmed returns valid geometric median", {
  set.seed(42)
  n <- 30
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)
  gm <- fdars::gmed(fd)

  expect_s3_class(gm, "fdata")
  expect_equal(nrow(gm$data), 1)
  expect_equal(ncol(gm$data), m)
})

test_that("depth with fdataref computes depths against reference", {
  set.seed(42)
  n1 <- 20
  n2 <- 15
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(0, n1, m)
  X2 <- matrix(0, n2, m)
  for (i in 1:n1) X1[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  for (i in 1:n2) X2[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd1 <- fdars::fdata(X1, argvals = t_grid)
  fd2 <- fdars::fdata(X2, argvals = t_grid)

  D <- fdars::depth(fd1, fdataref = fd2, method = "FM")

  expect_length(D, n1)
  expect_true(all(D >= 0 & D <= 1))
})

# ==============================================================================
# Additional tests to improve coverage
# ==============================================================================

test_that("depth with all methods and fdataref", {
  set.seed(42)
  n <- 15
  m <- 20
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  fd <- fdars::fdata(X, argvals = t_grid)

  Xref <- matrix(0, 10, m)
  for (i in 1:10) Xref[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  fdref <- fdars::fdata(Xref, argvals = t_grid)

  methods <- c("FM", "BD", "MBD", "MEI", "RP", "RT", "FSD", "KFSD", "RPD")
  for (meth in methods) {
    D <- fdars::depth(fd, fdataref = fdref, method = meth)
    expect_length(D, n)
    expect_true(all(is.finite(D)))
  }
})

test_that("var.fdata and sd.fdata work correctly", {
  set.seed(42)
  n <- 30
  m <- 40
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)

  # Variance returns fdata with 1 row
  v <- fdars::var(fd)
  expect_s3_class(v, "fdata")
  expect_equal(nrow(v$data), 1)
  expect_equal(ncol(v$data), m)
  expect_true(all(v$data >= 0))

  # Standard deviation returns fdata with 1 row
  s <- fdars::sd(fd)
  expect_s3_class(s, "fdata")
  expect_equal(nrow(s$data), 1)
  expect_equal(ncol(s$data), m)
  expect_true(all(s$data >= 0))

  # sd should be sqrt of var
  expect_equal(s$data, sqrt(v$data), tolerance = 1e-10)
})

test_that("gmed returns geometric median", {
  set.seed(42)
  n <- 20
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)

  gm <- fdars::gmed(fd)

  expect_s3_class(gm, "fdata")
  expect_equal(nrow(gm$data), 1)
})

test_that("cov.fdata with weighted option", {
  set.seed(42)
  n <- 20
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)

  fd <- fdars::fdata(X, argvals = t_grid)

  cv <- fdars::cov(fd, weighted = TRUE)
  expect_true(is.list(cv))
  expect_true("cov" %in% names(cv))
})

test_that("depth with 2D functional data", {
  set.seed(42)
  n <- 10
  m1 <- 8
  m2 <- 8

  # Create 2D fdata (surfaces)
  X <- array(rnorm(n * m1 * m2), dim = c(n, m1, m2))
  fd <- fdars::fdata(X, argvals = list(1:m1, 1:m2), fdata2d = TRUE)

  # FM depth is implemented for 2D
  D_fm <- fdars::depth(fd, method = "FM")
  expect_length(D_fm, n)
  expect_true(all(is.finite(D_fm)))
})

test_that("depth 2D with fdataref", {
  set.seed(42)
  n1 <- 8
  n2 <- 6
  m1 <- 8
  m2 <- 8

  X1 <- array(rnorm(n1 * m1 * m2), dim = c(n1, m1, m2))
  X2 <- array(rnorm(n2 * m1 * m2), dim = c(n2, m1, m2))

  fd1 <- fdars::fdata(X1, argvals = list(1:m1, 1:m2), fdata2d = TRUE)
  fd2 <- fdars::fdata(X2, argvals = list(1:m1, 1:m2), fdata2d = TRUE)

  D <- fdars::depth(fd1, fdataref = fd2, method = "FM")
  expect_length(D, n1)
  expect_true(all(is.finite(D)))
})
