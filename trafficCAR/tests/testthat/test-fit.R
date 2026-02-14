test_that("fit_car basic smoke test runs and returns correct structure", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(1)

  n <- 6
  A <- matrix(0, n, n)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[4, 5] <- 1; A[5, 4] <- 1
  # node 6 isolate (ICAR allowed, but should warn)

  X <- cbind(1, seq_len(n))
  beta_true <- c(0.5, -0.2)
  x_true <- c(0.2, -0.1, -0.1, 0.3, 0.0, 0.0)
  sigma2_true <- 0.25
  y <- as.double(X %*% beta_true + x_true + rnorm(n, sd = sqrt(sigma2_true)))

  warned <- FALSE
  fit <- withCallingHandlers(
    fit_car(
      y = y, A = A, X = X,
      type = "icar", tau = 2,
      n_iter = 50, burn_in = 10, thin = 2,
      a0 = 2, b0_sigma = 1,
      center_icar = TRUE
    ),
    warning = function(w) {
      warned <<- TRUE
      expect_match(
        conditionMessage(w),
        "isolat|degree 0|singular",
        ignore.case = TRUE
      )
      invokeRestart("muffleWarning")
    }
  )
  expect_true(warned)

  expect_s3_class(fit, "trafficCAR_fit")
  expect_true(is.list(fit$draws))
  expect_true(all(c("x", "beta", "sigma2") %in% names(fit$draws)))

  n_keep <- length(seq.int(11, 50, by = 2))
  expect_equal(dim(fit$draws$x), c(n_keep, n))
  expect_equal(dim(fit$draws$beta), c(n_keep, ncol(X)))
  expect_equal(length(fit$draws$sigma2), n_keep)

  expect_true(all(is.finite(fit$draws$x)))
  expect_true(all(is.finite(fit$draws$beta)))
  expect_true(all(is.finite(fit$draws$sigma2)))
  expect_true(all(fit$draws$sigma2 > 0))
})



test_that("fit_car reproducibility under set.seed", {
  skip_if_not(exists("fit_car", mode = "function"))

  n <- 5
  A <- matrix(0, n, n)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1
  A[4, 5] <- 1; A[5, 4] <- 1

  X <- cbind(1, seq_len(n))
  y <- as.double(rnorm(n))

  set.seed(123)
  fit1 <- fit_car(y, A, X = X, type = "proper", rho = 0.5, tau = 1, n_iter = 30, burn_in = 10, thin = 1)

  set.seed(123)
  fit2 <- fit_car(y, A, X = X, type = "proper", rho = 0.5, tau = 1, n_iter = 30, burn_in = 10, thin = 1)

  expect_equal(fit1$draws$sigma2, fit2$draws$sigma2)
  expect_equal(fit1$draws$beta, fit2$draws$beta)
  expect_equal(fit1$draws$x, fit2$draws$x)
})


test_that("fit_car ICAR centering: sum-to-zero per connected component (including isolate)", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(2)

  n <- 6
  A <- matrix(0, n, n)
  # component 1: 1-2-3
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  # component 2: 4-5
  A[4, 5] <- 1; A[5, 4] <- 1
  # isolate: 6

  y <- as.double(rnorm(n))

  warned <- FALSE
  fit <- withCallingHandlers(
    fit_car(
      y = y, A = A, X = NULL,
      type = "icar", tau = 1,
      n_iter = 40, burn_in = 10, thin = 1,
      center_icar = TRUE
    ),
    warning = function(w) {
      warned <<- TRUE
      testthat::expect_match(
        conditionMessage(w),
        "isolat|degree 0|singular",
        ignore.case = TRUE
      )
      invokeRestart("muffleWarning")
    }
  )
  expect_true(warned)

  x_draws <- fit$draws$x

  comp1 <- 1:3
  comp2 <- 4:5
  iso <- 6

  m1 <- rowMeans(x_draws[, comp1, drop = FALSE])
  m2 <- rowMeans(x_draws[, comp2, drop = FALSE])
  m3 <- x_draws[, iso]

  expect_true(max(abs(m1)) < 1e-10)
  expect_true(max(abs(m2)) < 1e-10)
  expect_true(max(abs(m3)) < 1e-10)
})


test_that("fit_car supports no-regression case (X = NULL): beta draws are 0-column", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(3)
  n <- 4
  A <- matrix(0, n, n)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1

  y <- as.double(rnorm(n))

  fit <- fit_car(y, A, X = NULL, type = "proper", rho = 0.7, tau = 1, n_iter = 20, burn_in = 5, thin = 1)

  testthat::expect_equal(ncol(fit$draws$beta), 0)
  testthat::expect_equal(nrow(fit$draws$beta), length(seq.int(6, 20, by = 1)))
})


testthat::test_that("fit_car rejects invalid y", {
  skip_if_not(exists("fit_car", mode = "function"))

  A <- diag(0, 3)

  testthat::expect_error(fit_car(y = c(1, NA, 3), A = A), "y", ignore.case = TRUE)
  testthat::expect_error(fit_car(y = c(1, Inf, 3), A = A), "y", ignore.case = TRUE)
  testthat::expect_error(fit_car(y = "bad", A = A), "y", ignore.case = TRUE)
})


test_that("fit_car rejects invalid A (shape/type)", {
  skip_if_not(exists("fit_car", mode = "function"))

  y <- rnorm(3)

  expect_error(fit_car(y, A = NULL), "A", ignore.case = TRUE)
  expect_error(fit_car(y, A = list()), "A", ignore.case = TRUE)

  A_ns <- matrix(0, 3, 2)
  expect_error(fit_car(y, A = A_ns), "square", ignore.case = TRUE)

  A_badn <- matrix(0, 4, 4)
  expect_error(fit_car(y, A = A_badn), "length\\(y\\)", ignore.case = TRUE)
})


test_that("fit_car rejects invalid MCMC controls", {
  skip_if_not(exists("fit_car", mode = "function"))

  y <- rnorm(3)
  A <- diag(0, 3)

  expect_error(fit_car(y, A, n_iter = 0), "n_iter", ignore.case = TRUE)
  expect_error(fit_car(y, A, n_iter = 10, burn_in = 10), "burn_in", ignore.case = TRUE)
  expect_error(fit_car(y, A, n_iter = 10, burn_in = -1), "burn_in", ignore.case = TRUE)
  expect_error(fit_car(y, A, thin = 0), "thin", ignore.case = TRUE)
})


test_that("fit_car rejects invalid hyperparameters", {
  skip_if_not(exists("fit_car", mode = "function"))

  y <- rnorm(3)
  A <- diag(0, 3)

  expect_error(fit_car(y, A, tau = 0), "tau", ignore.case = TRUE)
  expect_error(fit_car(y, A, tau = -1), "tau", ignore.case = TRUE)
  expect_error(fit_car(y, A, rho = NaN, type = "proper"), "rho", ignore.case = TRUE)

  expect_error(fit_car(y, A, a0 = 0), "a0", ignore.case = TRUE)
  expect_error(fit_car(y, A, b0_sigma = 0), "b0_sigma", ignore.case = TRUE)

  expect_error(fit_car(y, A, sigma2_init = 0), "sigma2_init", ignore.case = TRUE)
  expect_error(fit_car(y, A, sigma2_init = -1), "sigma2_init", ignore.case = TRUE)
})



test_that("fit_car rejects invalid regression inputs and priors", {
  skip_if_not(exists("fit_car", mode = "function"))

  y <- rnorm(4)
  A <- diag(0, 4)

  X_badn <- matrix(rnorm(6), nrow = 3)
  expect_error(fit_car(y, A, X = X_badn), "nrow\\(X\\)", ignore.case = TRUE)

  X_nf <- matrix(c(1, 2, NA, 4, 5, 6, 7, 8), nrow = 4)
  expect_error(fit_car(y, A, X = X_nf), "X", ignore.case = TRUE)

  X <- cbind(1, 1:4)

  expect_error(fit_car(y, A, X = X, b0 = c(0, 0, 0)), "b0", ignore.case = TRUE)

  B0_bad <- diag(1, 3)
  expect_error(fit_car(y, A, X = X, B0 = B0_bad), "B0", ignore.case = TRUE)

  expect_error(fit_car(y, A, X = X, beta_init = c(1, 2, 3)), "beta_init", ignore.case = TRUE)
  expect_error(fit_car(y, A, X = X, x_init = c(1, 2)), "x_init", ignore.case = TRUE)
  expect_error(fit_car(y, A, X = X, x_init = c(1, 2, NA, 4)), "x_init", ignore.case = TRUE)
})


test_that("proper CAR handles disconnected graphs with no isolates", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(4)

  n <- 6
  A <- matrix(0, n, n)
  # component 1: 1-2-3 (no isolates)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  # component 2: 4-5-6 (no isolates)
  A[4, 5] <- 1; A[5, 4] <- 1
  A[5, 6] <- 1; A[6, 5] <- 1

  y <- as.double(rnorm(n))
  X <- cbind(1, rnorm(n))

  fit_p <- fit_car(y, A, X = X, type = "proper", rho = 0.4, tau = 1,
                   n_iter = 25, burn_in = 5, thin = 1)

  expect_true(all(is.finite(fit_p$draws$sigma2)))
  expect_true(all(fit_p$draws$sigma2 > 0))
  expect_equal(ncol(fit_p$draws$x), n)
})


test_that("ICAR handles disconnected graphs with isolates", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(4)

  n <- 7
  A <- matrix(0, n, n)
  # two small components + isolates
  A[1, 2] <- 1; A[2, 1] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1
  # isolates: 5,6,7

  y <- as.double(rnorm(n))
  X <- cbind(1, rnorm(n))

  warned <- FALSE
  fit_i <- withCallingHandlers(
    fit_car(y, A, X = X, type = "icar", tau = 1,
            n_iter = 25, burn_in = 5, thin = 1, center_icar = TRUE),
    warning = function(w) {
      warned <<- TRUE
      expect_match(conditionMessage(w), "isolat|degree 0|singular", ignore.case = TRUE)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(warned)

  expect_true(all(is.finite(fit_i$draws$sigma2)))
  expect_true(all(fit_i$draws$sigma2 > 0))
  expect_equal(ncol(fit_i$draws$x), n)
})


test_that("n = 1 ICAR isolate runs", {
  set.seed(10)

  y <- 0.5
  A <- matrix(0, 1, 1)

  warned <- FALSE
  fit <- withCallingHandlers(
    fit_car(y, A, type = "icar", tau = 1, n_iter = 12, burn_in = 2, thin = 2, center_icar = TRUE),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )

  expect_true(warned)
  expect_equal(ncol(fit$draws$x), 1)
  expect_true(all(is.finite(fit$draws$x)))
})



test_that("dense complete graph runs (proper)", {
  set.seed(11)

  n <- 8
  A <- matrix(1, n, n); diag(A) <- 0
  y <- as.double(rnorm(n))
  X <- cbind(1, rnorm(n))

  fit <- fit_car(y, A, X = X, type = "proper", rho = 0.3, tau = 1,
                 n_iter = 30, burn_in = 10, thin = 2)

  expect_true(all(is.finite(fit$draws$x)))
  expect_true(all(is.finite(fit$draws$beta)))
})


test_that("weighted adjacency runs", {
  set.seed(12)

  n <- 6
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 0.2 + i / 10
    A[i + 1, i] <- 0.2 + i / 10
  }

  y <- as.double(rnorm(n))

  fit <- fit_car(y, A, type = "proper", rho = 0.4, tau = 2,
                 n_iter = 25, burn_in = 5, thin = 1)

  expect_true(all(is.finite(fit$draws$sigma2)))
})



test_that("rank-deficient X runs", {
  set.seed(13)

  n <- 8
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }

  z <- rnorm(n)
  X <- cbind(1, z, 2 * z)
  y <- as.double(rnorm(n))

  fit <- fit_car(y, A, X = X, type = "proper", rho = 0.5, tau = 1,
                 n_iter = 25, burn_in = 5, thin = 1)

  expect_true(all(is.finite(fit$draws$beta)))
})



test_that("p > n runs", {
  set.seed(14)

  n <- 6
  p <- 10
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }

  X <- matrix(rnorm(n * p), n, p)
  y <- as.double(rnorm(n))

  fit <- fit_car(y, A, X = X, type = "proper", rho = 0.5, tau = 1,
                 n_iter = 20, burn_in = 5, thin = 1)

  expect_equal(ncol(fit$draws$beta), p)
  expect_true(all(is.finite(fit$draws$beta)))
})



test_that("tau and sigma2_init extremes stay finite", {
  set.seed(15)

  n <- 10
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }

  y <- as.double(rnorm(n))

  fit1 <- fit_car(y, A, type = "proper", rho = 0.4, tau = 1e-8,
                  sigma2_init = 1e-8, n_iter = 20, burn_in = 5, thin = 1)

  fit2 <- fit_car(y, A, type = "proper", rho = 0.4, tau = 1e8,
                  sigma2_init = 1e6, n_iter = 20, burn_in = 5, thin = 1)

  expect_true(all(is.finite(fit1$draws$x)))
  expect_true(all(is.finite(fit1$draws$sigma2)))
  expect_true(all(is.finite(fit2$draws$x)))
  expect_true(all(is.finite(fit2$draws$sigma2)))
})


test_that("burn_in = n_iter - 1 keeps one draw", {
  set.seed(16)

  n <- 5
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }

  y <- as.double(rnorm(n))

  fit <- fit_car(y, A, type = "proper", rho = 0.5, tau = 1,
                 n_iter = 10, burn_in = 9, thin = 1)

  expect_equal(nrow(fit$draws$x), 1)
  expect_equal(length(fit$draws$sigma2), 1)
})
