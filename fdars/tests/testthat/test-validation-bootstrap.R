# Validation tests for bootstrap and other functions

test_that("fdata.bootstrap produces valid samples (naive)", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  boot <- fdars::fdata.bootstrap(fd, n.boot = 50, method = "naive", seed = 123)

  expect_s3_class(boot, "fdata.bootstrap")
  expect_equal(boot$n.boot, 50)
  expect_equal(boot$method, "naive")
  expect_length(boot$boot.samples, 50)

  # Each sample should have same dimensions as original
  for (i in 1:10) {
    expect_equal(nrow(boot$boot.samples[[i]]$data), n)
    expect_equal(ncol(boot$boot.samples[[i]]$data), m)
  }
})

test_that("fdata.bootstrap produces valid samples (smooth)", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  boot <- fdars::fdata.bootstrap(fd, n.boot = 50, method = "smooth", seed = 123)

  expect_s3_class(boot, "fdata.bootstrap")
  expect_equal(boot$method, "smooth")
  expect_length(boot$boot.samples, 50)
})

test_that("fdata.bootstrap.ci produces valid intervals", {
  set.seed(42)
  n <- 30
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  # mean returns fdata, extract data vector for bootstrap CI
  mean_vec <- function(x) as.numeric(mean(x)$data)
  ci <- fdars::fdata.bootstrap.ci(fd, statistic = mean_vec,
                                   n.boot = 50, alpha = 0.05, seed = 123)

  expect_s3_class(ci, "fdata.bootstrap.ci")
  expect_length(ci$estimate, m)
  expect_length(ci$ci.lower, m)
  expect_length(ci$ci.upper, m)

  # Lower bound should be <= estimate <= upper bound
  expect_true(all(ci$ci.lower <= ci$estimate + 1e-10))
  expect_true(all(ci$ci.upper >= ci$estimate - 1e-10))
})

test_that("fdata.bootstrap.ci works with different methods", {
  set.seed(42)
  n <- 20
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  # mean returns fdata, extract data vector for bootstrap CI
  mean_vec <- function(x) as.numeric(mean(x)$data)

  ci_perc <- fdars::fdata.bootstrap.ci(fd, statistic = mean_vec,
                                        n.boot = 30, method = "percentile", seed = 123)
  ci_basic <- fdars::fdata.bootstrap.ci(fd, statistic = mean_vec,
                                         n.boot = 30, method = "basic", seed = 123)
  ci_norm <- fdars::fdata.bootstrap.ci(fd, statistic = mean_vec,
                                        n.boot = 30, method = "normal", seed = 123)

  expect_equal(ci_perc$method, "percentile")
  expect_equal(ci_basic$method, "basic")
  expect_equal(ci_norm$method, "normal")
})

test_that("deriv works for 1D data", {
  set.seed(42)
  m <- 100
  t_grid <- seq(0, 2 * pi, length.out = m)

  # sin(t) -> cos(t)
  X <- matrix(sin(t_grid), nrow = 1)
  fd <- fdars::fdata(X, argvals = t_grid)
  fd_deriv <- fdars::deriv(fd, nderiv = 1)

  expect_s3_class(fd_deriv, "fdata")
  expect_equal(nrow(fd_deriv$data), 1)

  # Check derivative is approximately cos(t) at interior points
  mid_idx <- 50
  expect_equal(fd_deriv$data[1, mid_idx], cos(t_grid[mid_idx]), tolerance = 0.02)
})

test_that("deriv works for 2D data", {
  set.seed(42)
  n <- 5
  m1 <- 10
  m2 <- 15
  s_grid <- seq(0, 1, length.out = m1)
  t_grid <- seq(0, 1, length.out = m2)

  # f(s,t) = sin(2*pi*s) * cos(2*pi*t)
  X <- array(0, dim = c(n, m1, m2))
  for (i in 1:n) {
    for (si in 1:m1) {
      for (ti in 1:m2) {
        X[i, si, ti] <- sin(2 * pi * s_grid[si]) * cos(2 * pi * t_grid[ti])
      }
    }
  }

  fd2d <- fdars::fdata(X, argvals = list(s_grid, t_grid), fdata2d = TRUE)
  derivs <- fdars::deriv(fd2d, nderiv = 1)

  expect_true(is.list(derivs))
  expect_true(all(c("ds", "dt", "dsdt") %in% names(derivs)))

  expect_s3_class(derivs$ds, "fdata")
  expect_s3_class(derivs$dt, "fdata")
  expect_s3_class(derivs$dsdt, "fdata")

  expect_equal(nrow(derivs$ds$data), n)
  expect_equal(ncol(derivs$ds$data), m1 * m2)
})

test_that("fdata.cen centers data correctly", {
  set.seed(42)
  n <- 20
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + i/n + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  fd_cen <- fdars::fdata.cen(fd)

  # Centered data should have approximately zero mean
  mean_func <- colMeans(fd_cen$data)
  expect_true(all(abs(mean_func) < 1e-10))
})

test_that("mean computes correct mean", {
  set.seed(42)
  n <- 20
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  mean_func <- mean(fd)

  expect_s3_class(mean_func, "fdata")
  expect_equal(as.numeric(mean_func$data), colMeans(X), tolerance = 1e-10)
})

test_that("norm computes correct norms", {
  set.seed(42)
  n <- 10
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, n, m)
  for (i in 1:n) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  }

  fd <- fdars::fdata(X, argvals = t_grid)
  norms <- fdars::norm(fd, lp = 2)

  expect_length(norms, n)
  expect_true(all(norms >= 0))
})
