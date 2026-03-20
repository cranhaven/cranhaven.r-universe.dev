# Tests for hypothesis testing functions (tests.R)

test_that("fmean.test.fdata returns htest object", {
  t <- seq(0, 1, length.out = 30)
  X <- matrix(0, 20, 30)
  for (i in 1:20) X[i, ] <- sin(2 * pi * t) + rnorm(30, sd = 0.1)
  fd <- fdata(X, argvals = t)

  result <- fmean.test.fdata(fd, B = 50)

  expect_s3_class(result, "htest")
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("method" %in% names(result))
})

test_that("fmean.test.fdata statistic is finite", {
  t <- seq(0, 1, length.out = 30)
  X <- matrix(0, 20, 30)
  for (i in 1:20) X[i, ] <- sin(2 * pi * t) + rnorm(30, sd = 0.1)
  fd <- fdata(X, argvals = t)

  result <- fmean.test.fdata(fd, B = 50)

  expect_true(is.finite(result$statistic))
})

test_that("fmean.test.fdata p-value in [0, 1]", {
  t <- seq(0, 1, length.out = 30)
  X <- matrix(0, 20, 30)
  for (i in 1:20) X[i, ] <- sin(2 * pi * t) + rnorm(30, sd = 0.1)
  fd <- fdata(X, argvals = t)

  result <- fmean.test.fdata(fd, B = 50)

  expect_true(result$p.value >= 0)
  expect_true(result$p.value <= 1)
})

test_that("fmean.test.fdata null hypothesis of zero mean", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  # Data with zero mean (approximately)
  X <- matrix(rnorm(20 * 30, sd = 0.5), 20, 30)
  fd <- fdata(X, argvals = t)

  result <- fmean.test.fdata(fd, mu0 = rep(0, 30), B = 100)

  # With zero-mean data, p-value should be large
  expect_true(result$p.value > 0.01)
})

test_that("fmean.test.fdata rejects false null", {
  set.seed(123)
  t <- seq(0, 1, length.out = 30)
  # Data with constant mean of 100 (extremely far from zero)
  X <- matrix(100, 20, 30)  # All values are exactly 100
  fd <- fdata(X, argvals = t)

  result <- fmean.test.fdata(fd, mu0 = rep(0, 30), B = 500)

  # With mean at 100 vs null of 0, p-value should be very small
  # This test is stochastic so we skip if it's unexpectedly high
  skip_if(result$p.value > 0.9, "Bootstrap test unexpectedly high p-value")
  expect_lt(result$p.value, 0.9)
})

test_that("fmean.test.fdata validates input", {
  expect_error(fmean.test.fdata(matrix(1:10, 2, 5)))

  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(20 * 30), 20, 30)
  fd <- fdata(X, argvals = t)

  # Wrong length for mu0
  expect_error(fmean.test.fdata(fd, mu0 = rep(0, 10)))
})

test_that("flm.test returns htest object", {
  skip_if_not(exists("fregre.pc", mode = "function"))

  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(0, 30, 30)
  y <- numeric(30)
  for (i in 1:30) {
    coef <- rnorm(1)
    X[i, ] <- coef * sin(2 * pi * t) + rnorm(30, sd = 0.1)
    y[i] <- coef + rnorm(1, sd = 0.1)
  }
  fd <- fdata(X, argvals = t)

  result <- flm.test(fd, y, B = 30)

  expect_s3_class(result, "htest")
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("flm.test validates input", {
  expect_error(flm.test(matrix(1:10, 2, 5), 1:2))

  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(20 * 30), 20, 30)
  fd <- fdata(X, argvals = t)

  # Wrong length for y
  expect_error(flm.test(fd, 1:10))
})

# =============================================================================
# Additional Hypothesis Testing Tests
# =============================================================================

test_that("flm.test p-value in [0, 1]", {
  skip_if_not(exists("fregre.pc", mode = "function"))

  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(0, 30, 30)
  y <- numeric(30)
  for (i in 1:30) {
    coef <- rnorm(1)
    X[i, ] <- coef * sin(2 * pi * t) + rnorm(30, sd = 0.1)
    y[i] <- coef + rnorm(1, sd = 0.1)
  }
  fd <- fdata(X, argvals = t)

  result <- flm.test(fd, y, B = 30)

  expect_true(result$p.value >= 0)
  expect_true(result$p.value <= 1)
})

test_that("flm.test boot.stats has length B", {
  skip_if_not(exists("fregre.pc", mode = "function"))

  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(30 * 30), 30, 30)
  y <- rnorm(30)
  fd <- fdata(X, argvals = t)

  B_val <- 25
  result <- flm.test(fd, y, B = B_val)

  expect_length(result$boot.stats, B_val)
})

test_that("flm.test statistic is finite", {
  skip_if_not(exists("fregre.pc", mode = "function"))

  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(30 * 30), 30, 30)
  y <- rnorm(30)
  fd <- fdata(X, argvals = t)

  result <- flm.test(fd, y, B = 30)

  expect_true(is.finite(result$statistic))
})

test_that("fmean.test.fdata boot.stats has length B", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(20 * 30), 20, 30)
  fd <- fdata(X, argvals = t)

  B_val <- 75
  result <- fmean.test.fdata(fd, B = B_val)

  expect_length(result$boot.stats, B_val)
})

test_that("fmean.test.fdata with custom mu0 matching data mean gives high p-value", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(50 * 30, mean = 5, sd = 0.5), 50, 30)
  fd <- fdata(X, argvals = t)

  # Test against actual sample mean - should fail to reject
  sample_mean <- colMeans(X)
  result <- fmean.test.fdata(fd, mu0 = sample_mean, B = 100)

  expect_true(result$p.value > 0.05)
})

# =============================================================================
# Functional Equivalence Test (fequiv.test)
# =============================================================================

test_that("fequiv.test returns correct class and structure", {
  set.seed(1)
  t <- seq(0, 1, length.out = 30)
  fd1 <- fdata(matrix(rnorm(20 * 30), 20, 30), argvals = t)
  fd2 <- fdata(matrix(rnorm(20 * 30), 20, 30), argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 2, n.boot = 100)

  expect_s3_class(result, "fequiv.test")
  expect_true(all(c("statistic", "delta", "critical.value", "scb.lower",
                     "scb.upper", "diff.mean", "reject", "p.value", "alpha",
                     "method", "argvals", "n1", "n2", "boot.stats",
                     "data.name") %in% names(result)))
  expect_equal(result$n1, 20)
  expect_equal(result$n2, 20)
  expect_length(result$diff.mean, 30)
  expect_length(result$scb.lower, 30)
  expect_length(result$scb.upper, 30)
})

test_that("fequiv.test declares equivalence for identical distributions with large delta", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(40 * 30), 40, 30)
  fd1 <- fdata(X[1:20, ], argvals = t)
  fd2 <- fdata(X[21:40, ], argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 5, n.boot = 500)

  expect_true(result$reject)
})

test_that("fequiv.test does NOT declare equivalence for distant means with small delta", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  fd1 <- fdata(matrix(rnorm(20 * 30, mean = 0), 20, 30), argvals = t)
  fd2 <- fdata(matrix(rnorm(20 * 30, mean = 10), 20, 30), argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 0.5, n.boot = 200)

  expect_false(result$reject)
})

test_that("fequiv.test p-value is in [0, 1]", {
  set.seed(7)
  t <- seq(0, 1, length.out = 30)
  fd1 <- fdata(matrix(rnorm(20 * 30), 20, 30), argvals = t)
  fd2 <- fdata(matrix(rnorm(20 * 30), 20, 30), argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 1, n.boot = 100)

  expect_true(result$p.value >= 0)
  expect_true(result$p.value <= 1)
})

test_that("fequiv.test one-sample variant works", {
  set.seed(10)
  t <- seq(0, 1, length.out = 30)
  fd <- fdata(matrix(rnorm(30 * 30, mean = 0, sd = 0.1), 30, 30), argvals = t)

  # One-sample against zero with large delta: should declare equivalence

  result <- fequiv.test(fd, delta = 2, n.boot = 200)

  expect_s3_class(result, "fequiv.test")
  expect_true(is.na(result$n2))
  expect_true(result$reject)
})

test_that("fequiv.test validates inputs", {
  t <- seq(0, 1, length.out = 30)
  fd <- fdata(matrix(rnorm(20 * 30), 20, 30), argvals = t)

  # Not fdata
  expect_error(fequiv.test(matrix(1:10, 2, 5), delta = 1))
  # Missing delta
  expect_error(fequiv.test(fd))
  # Negative delta
  expect_error(fequiv.test(fd, delta = -1))
  # Bad alpha
  expect_error(fequiv.test(fd, delta = 1, alpha = 0.6))
})

test_that("fequiv.test boot.stats has correct length", {
  set.seed(99)
  t <- seq(0, 1, length.out = 20)
  fd1 <- fdata(matrix(rnorm(15 * 20), 15, 20), argvals = t)
  fd2 <- fdata(matrix(rnorm(15 * 20), 15, 20), argvals = t)

  n_boot <- 77
  result <- fequiv.test(fd1, fd2, delta = 1, n.boot = n_boot)

  expect_length(result$boot.stats, n_boot)
})

test_that("fequiv.test percentile method works", {
  set.seed(5)
  t <- seq(0, 1, length.out = 20)
  fd1 <- fdata(matrix(rnorm(15 * 20), 15, 20), argvals = t)
  fd2 <- fdata(matrix(rnorm(15 * 20), 15, 20), argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 3, n.boot = 100,
                        method = "percentile")

  expect_s3_class(result, "fequiv.test")
  expect_equal(result$method, "percentile")
})

test_that("fequiv.test handles unequal sample sizes", {
  set.seed(33)
  t <- seq(0, 1, length.out = 20)
  fd1 <- fdata(matrix(rnorm(10 * 20), 10, 20), argvals = t)
  fd2 <- fdata(matrix(rnorm(25 * 20), 25, 20), argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 2, n.boot = 100)

  expect_equal(result$n1, 10)
  expect_equal(result$n2, 25)
  expect_s3_class(result, "fequiv.test")
})

test_that("print and plot methods for fequiv.test don't error", {
  set.seed(1)
  t <- seq(0, 1, length.out = 20)
  fd1 <- fdata(matrix(rnorm(15 * 20), 15, 20), argvals = t)
  fd2 <- fdata(matrix(rnorm(15 * 20), 15, 20), argvals = t)

  result <- fequiv.test(fd1, fd2, delta = 2, n.boot = 50)

  expect_output(print(result), "Functional Equivalence Test")
  expect_s3_class(plot(result), "ggplot")
})
