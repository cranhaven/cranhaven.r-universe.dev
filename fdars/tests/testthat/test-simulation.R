# Tests for simulation functions

test_that("eFun generates correct dimensions", {
  t <- seq(0, 1, length.out = 100)
  phi <- eFun(t, M = 5, type = "Fourier")
  expect_equal(dim(phi), c(100, 5))

  phi_poly <- eFun(t, M = 3, type = "Poly")
  expect_equal(dim(phi_poly), c(100, 3))

  phi_wiener <- eFun(t, M = 7, type = "Wiener")
  expect_equal(dim(phi_wiener), c(100, 7))
})

test_that("eFun Fourier eigenfunctions are approximately orthonormal", {
  t <- seq(0, 1, length.out = 1000)
  phi <- eFun(t, M = 5, type = "Fourier")
  dt <- 1 / (length(t) - 1)

  # Compute Gram matrix (approximate inner products)
  gram <- t(phi) %*% phi * dt

  # Should be approximately identity
  expect_true(all(abs(diag(gram) - 1) < 0.05))
  expect_true(all(abs(gram[upper.tri(gram)]) < 0.05))
})

test_that("eFun Wiener eigenfunctions are approximately orthonormal", {
  t <- seq(0, 1, length.out = 1000)
  phi <- eFun(t, M = 5, type = "Wiener")
  dt <- 1 / (length(t) - 1)

  gram <- t(phi) %*% phi * dt

  expect_true(all(abs(diag(gram) - 1) < 0.05))
  expect_true(all(abs(gram[upper.tri(gram)]) < 0.05))
})

test_that("eVal returns correct decay patterns", {
  # Linear decay
  lambda_lin <- eVal(5, "linear")
  expect_equal(length(lambda_lin), 5)
  expect_equal(lambda_lin[1], 1)
  expect_equal(lambda_lin[2], 0.5)
  expect_true(all(diff(lambda_lin) < 0))  # Decreasing

  # Exponential decay
  lambda_exp <- eVal(5, "exponential")
  expect_equal(lambda_exp[1], exp(-1))
  expect_true(all(diff(lambda_exp) < 0))

  # Wiener eigenvalues
  lambda_wie <- eVal(3, "wiener")
  expect_equal(length(lambda_wie), 3)
  expect_true(all(diff(lambda_wie) < 0))
})

test_that("simFunData produces valid fdata objects", {
  t <- seq(0, 1, length.out = 100)
  fd <- simFunData(n = 20, argvals = t, M = 5)

  expect_s3_class(fd, "fdata")
  expect_equal(nrow(fd$data), 20)
  expect_equal(ncol(fd$data), 100)
  expect_equal(fd$argvals, t)
})

test_that("simFunData is reproducible with seed", {
  t <- seq(0, 1, length.out = 50)

  fd1 <- simFunData(n = 10, argvals = t, M = 5, seed = 42)
  fd2 <- simFunData(n = 10, argvals = t, M = 5, seed = 42)

  expect_equal(fd1$data, fd2$data)

  # Different seed should give different results
  fd3 <- simFunData(n = 10, argvals = t, M = 5, seed = 123)
  expect_false(all(fd1$data == fd3$data))
})

test_that("simFunData with mean function works", {
  t <- seq(0, 1, length.out = 100)
  mean_fn <- function(t) sin(2 * pi * t)

  fd <- simFunData(n = 50, argvals = t, M = 5, mean = mean_fn, seed = 42)

  # Empirical mean should be close to true mean
  empirical_mean <- colMeans(fd$data)
  true_mean <- mean_fn(t)

  # Allow for sampling variability
  expect_true(cor(empirical_mean, true_mean) > 0.9)
})

test_that("simMultiFunData creates valid structure", {
  t1 <- seq(0, 1, length.out = 100)
  t2 <- seq(0, 0.5, length.out = 50)

  mfd <- simMultiFunData(
    n = 15,
    argvals = list(t1, t2),
    M = c(5, 3),
    eFun.type = c("Fourier", "Wiener"),
    eVal.type = c("exponential", "linear"),
    seed = 42
  )

  expect_s3_class(mfd, "multiFunData")
  expect_equal(mfd$n, 15)
  expect_equal(mfd$p, 2)
  expect_length(mfd$components, 2)
  expect_s3_class(mfd$components[[1]], "fdata")
  expect_s3_class(mfd$components[[2]], "fdata")
  expect_equal(nrow(mfd$components[[1]]$data), 15)
  expect_equal(nrow(mfd$components[[2]]$data), 15)
})

test_that("addError adds noise with correct variance", {
  t <- seq(0, 1, length.out = 100)
  fd <- simFunData(n = 100, argvals = t, M = 5, seed = 42)
  fd_noisy <- addError(fd, sd = 0.5, seed = 123)

  noise <- fd_noisy$data - fd$data
  empirical_sd <- sd(as.vector(noise))

  # Should be close to 0.5
  expect_true(abs(empirical_sd - 0.5) < 0.1)
})

test_that("addError preserves fdata structure", {
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 10, argvals = t, M = 5, seed = 42)
  fd_noisy <- addError(fd, sd = 0.1)

  expect_s3_class(fd_noisy, "fdata")
  expect_equal(dim(fd_noisy$data), dim(fd$data))
  expect_equal(fd_noisy$argvals, fd$argvals)
  expect_equal(fd_noisy$id, fd$id)
})

test_that("addError curve noise is constant per curve", {
  t <- seq(0, 1, length.out = 100)
  fd <- simFunData(n = 10, argvals = t, M = 5, seed = 42)
  fd_noisy <- addError(fd, sd = 1, type = "curve", seed = 123)

  noise <- fd_noisy$data - fd$data

  # Each row should have constant noise
  for (i in 1:nrow(noise)) {
    row_noise <- noise[i, ]
    expect_true(sd(row_noise) < 0.01)  # Should be near-constant
  }
})

# =============================================================================
# Additional Simulation Tests
# =============================================================================

test_that("simMultiFunData produces valid structure", {
  t1 <- seq(0, 1, length.out = 50)
  t2 <- seq(0, 0.5, length.out = 30)

  mfd <- simMultiFunData(n = 10, argvals = list(t1, t2), M = 3, seed = 42)

  expect_s3_class(mfd, "multiFunData")
  expect_equal(mfd$n, 10)
  expect_equal(mfd$p, 2)
  expect_equal(ncol(mfd$components[[1]]$data), 50)
  expect_equal(ncol(mfd$components[[2]]$data), 30)
})

test_that("simMultiFunData with scalar M expands to all components", {
  t1 <- seq(0, 1, length.out = 50)
  t2 <- seq(0, 0.5, length.out = 30)

  mfd <- simMultiFunData(n = 5, argvals = list(t1, t2), M = 4, seed = 42)

  expect_equal(mfd$p, 2)
  expect_equal(nrow(mfd$components[[1]]$data), 5)
  expect_equal(nrow(mfd$components[[2]]$data), 5)
})

test_that("simMultiFunData print method works", {
  t1 <- seq(0, 1, length.out = 50)
  t2 <- seq(0, 0.5, length.out = 30)

  mfd <- simMultiFunData(n = 5, argvals = list(t1, t2), M = 3, seed = 42)
  expect_output(print(mfd), "Multivariate Functional Data")
})

test_that("eFun with PolyHigh type", {
  t <- seq(0, 1, length.out = 100)
  phi <- eFun(t, M = 3, type = "PolyHigh")

  expect_equal(dim(phi), c(100, 3))
  # Should be finite
  expect_true(all(is.finite(phi)))
})

test_that("eFun with M=1 (single eigenfunction)", {
  t <- seq(0, 1, length.out = 100)
  phi <- eFun(t, M = 1, type = "Fourier")

  expect_equal(dim(phi), c(100, 1))
})

test_that("eVal wiener values match theoretical formula", {
  lambda <- eVal(5, "wiener")

  for (k in 1:5) {
    expected <- 1 / ((k - 0.5) * pi)^2
    expect_equal(lambda[k], expected, tolerance = 1e-10)
  }
})

test_that("addError is reproducible with seed", {
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 5, argvals = t, M = 3, seed = 42)

  fd_noisy1 <- addError(fd, sd = 0.1, seed = 123)
  fd_noisy2 <- addError(fd, sd = 0.1, seed = 123)

  expect_equal(fd_noisy1$data, fd_noisy2$data)
})

test_that("simFunData with numeric mean vector", {
  t <- seq(0, 1, length.out = 50)
  mean_vec <- rep(10, 50)

  fd <- simFunData(n = 20, argvals = t, M = 3, mean = mean_vec, seed = 42)

  # Empirical mean should be close to 10
  emp_mean <- colMeans(fd$data)
  expect_true(mean(abs(emp_mean - 10)) < 1)
})

test_that("simFunData with different eFun/eVal combinations", {
  t <- seq(0, 1, length.out = 50)

  for (ef in c("Fourier", "Poly", "PolyHigh", "Wiener")) {
    for (ev in c("linear", "exponential", "wiener")) {
      fd <- simFunData(n = 5, argvals = t, M = 3,
                       eFun.type = ef, eVal.type = ev, seed = 42)
      expect_s3_class(fd, "fdata")
      expect_equal(nrow(fd$data), 5)
    }
  }
})

test_that("simFunData mean length mismatch errors", {
  t <- seq(0, 1, length.out = 50)
  expect_error(simFunData(n = 5, argvals = t, M = 3, mean = rep(0, 10)),
               "same length")
})

test_that("addError validates input", {
  expect_error(addError(matrix(1:10, 2, 5)), "fdata")
})
