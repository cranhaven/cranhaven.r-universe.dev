test_that("update_beta_gaussian returns correct length and is reproducible", {
  set.seed(1)
  n <- 20
  p <- 3
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  x <- rnorm(n)
  sigma2 <- 0.5
  b0 <- rep(0, p)
  B0 <- diag(10, p)

  set.seed(123)
  b1 <- update_beta_gaussian(y, X, x, sigma2, b0, B0)
  set.seed(123)
  b2 <- update_beta_gaussian(y, X, x, sigma2, b0, B0)

  expect_length(b1, p)
  expect_equal(b1, b2)
})


test_that("update_beta_gaussian recovers signal in a simple toy setup", {
  set.seed(2)
  n <- 200
  p <- 2
  X <- cbind(1, rnorm(n))
  beta_true <- c(0.7, -1.2)
  x <- rep(0, n) # isolate regression layer from spatial effects
  sigma2 <- 0.25^2
  y <- as.numeric(X %*% beta_true + x + rnorm(n, sd = sqrt(sigma2)))

  # weak prior
  b0 <- rep(0, p)
  B0 <- diag(1e6, p)

  # draw many betas (conditional on x,sigma2) and average
  B <- 2000
  draws <- replicate(B, update_beta_gaussian(y, X, x, sigma2, b0, B0))
  beta_hat <- rowMeans(draws)

  expect_equal(beta_hat[1], beta_true[1], tolerance = 0.1)
  expect_equal(beta_hat[2], beta_true[2], tolerance = 0.1)
})


test_that("update_sigma2_ig is reproducible and positive", {
  set.seed(3)
  n <- 50
  X <- cbind(1, rnorm(n))
  beta <- c(0.2, 0.5)
  x <- rnorm(n, sd = 0.3)
  y <- as.numeric(X %*% beta + x + rnorm(n, sd = 0.4))

  set.seed(99)
  s1 <- update_sigma2_ig(y, X, beta, x, a0 = 2, b0 = 1)
  set.seed(99)
  s2 <- update_sigma2_ig(y, X, beta, x, a0 = 2, b0 = 1)

  expect_true(is.finite(s1) && s1 > 0)
  expect_equal(s1, s2)
})


test_that("update_beta_gaussian validates inputs", {
  n <- 5
  X <- matrix(1, n, 1)
  y <- rnorm(n)
  x <- rnorm(n)

  expect_error(update_beta_gaussian(y, X, x, sigma2 = -1, b0 = 0, B0 = matrix(1)), "sigma2")
  expect_error(update_beta_gaussian(y, X, x[-1], sigma2 = 1, b0 = 0, B0 = matrix(1)), "x")
  expect_error(update_beta_gaussian(y, X, x, sigma2 = 1, b0 = c(0, 1), B0 = diag(1)), "b0")
})


test_that("update_beta_gaussian rejects non-finite y/X/x and bad types", {
  n <- 10; p <- 2
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- rep(0, p)
  B0 <- diag(1, p)

  y2 <- y; y2[3] <- NA_real_
  expect_error(update_beta_gaussian(y2, X, x, 1, b0, B0), "finite")

  x2 <- x; x2[1] <- Inf
  expect_error(update_beta_gaussian(y, X, x2, 1, b0, B0), "finite")

  X2 <- X; X2[2,1] <- NaN
  expect_error(update_beta_gaussian(y, X2, x, 1, b0, B0), "finite")

  expect_error(update_beta_gaussian(as.integer(y), X, x, 1, b0, B0), "double vector")

  expect_error(update_beta_gaussian(y, as.data.frame(X), x, 1, b0, B0), "X", fixed = FALSE)
})


test_that("update_beta_gaussian catches dimension mismatches cleanly", {
  n <- 8; p <- 3
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- rep(0, p)
  B0 <- diag(1, p)

  expect_error(update_beta_gaussian(y[-1], X, x, 1, b0, B0), "nrow", fixed = FALSE)
  expect_error(update_beta_gaussian(y, X[, -1, drop = FALSE], x, 1, b0, B0), "b0", fixed = FALSE)
  expect_error(update_beta_gaussian(y, X, x, 1, b0[-1], B0), "b0", fixed = FALSE)
  expect_error(update_beta_gaussian(y, X, x, 1, b0, B0[-1, -1]), "B0", fixed = FALSE)
})


test_that("update_beta_gaussian rejects singular or non-PD B0", {
  n <- 20; p <- 2
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- rep(0, p)

  B0_singular <- matrix(c(1, 1, 1, 1), 2, 2)  # rank 1
  expect_error(update_beta_gaussian(y, X, x, 1, b0, B0_singular), "B0", fixed = FALSE)

  B0_indef <- matrix(c(1, 2, 2, 1), 2, 2)     # has eigenvalues 3 and -1
  expect_error(update_beta_gaussian(y, X, x, 1, b0, B0_indef), "B0", fixed = FALSE)
})


test_that("update_beta_gaussian handles rank-deficient X without crashing (proper prior)", {
  n <- 50
  x1 <- rnorm(n)
  X <- cbind(1, x1, x1)  # perfectly collinear columns 2 and 3
  p <- ncol(X)

  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- rep(0, p)
  B0 <- diag(10, p)

  set.seed(1)
  b <- update_beta_gaussian(y, X, x, sigma2 = 0.5, b0, B0)
  expect_true(all(is.finite(b)))
  expect_length(b, p)
})


test_that("update_beta_gaussian is stable under near-collinearity", {
  set.seed(1)
  n <- 200
  z <- rnorm(n)
  X <- cbind(1, z, z + 1e-10 * rnorm(n))  # nearly the same
  p <- ncol(X)

  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- rep(0, p)
  B0 <- diag(100, p)

  b <- update_beta_gaussian(y, X, x, sigma2 = 1, b0, B0)
  expect_true(all(is.finite(b)))
})


test_that("update_beta_gaussian behaves under extreme sigma2", {
  set.seed(1)
  n <- 60; p <- 3
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- rep(0, p)
  B0 <- diag(1, p)

  b_small <- update_beta_gaussian(y, X, x, sigma2 = 1e-12, b0, B0)
  b_large <- update_beta_gaussian(y, X, x, sigma2 = 1e+12, b0, B0)

  expect_true(all(is.finite(b_small)))
  expect_true(all(is.finite(b_large)))
})


test_that("update_beta_gaussian handles extreme scaling in X", {
  set.seed(1)
  n <- 30
  X <- cbind(1, 1e150 * rnorm(n))  # very large
  y <- rnorm(n)
  x <- rnorm(n)
  b0 <- c(0, 0)
  B0 <- diag(1, 2)

  b <- update_beta_gaussian(y, X, x, sigma2 = 1, b0, B0)
  expect_true(all(is.finite(b)))
})


test_that("update_sigma2_ig rejects invalid hyperparameters", {
  n <- 10
  X <- cbind(1, rnorm(n))
  beta <- c(0, 0)
  x <- rnorm(n)
  y <- rnorm(n)

  expect_error(update_sigma2_ig(y, X, beta, x, a0 = 0, b0 = 1), "a0")
  expect_error(update_sigma2_ig(y, X, beta, x, a0 = 1, b0 = 0), "b0")
})

test_that("update_sigma2_ig stays finite with huge residuals", {
  set.seed(1)
  n <- 50
  X <- cbind(1, rnorm(n))
  beta <- c(0, 0)
  x <- rep(0, n)
  y <- 1e8 * rnorm(n)  # huge

  s <- update_sigma2_ig(y, X, beta, x, a0 = 2, b0 = 2)
  expect_true(is.finite(s) && s > 0)
})
