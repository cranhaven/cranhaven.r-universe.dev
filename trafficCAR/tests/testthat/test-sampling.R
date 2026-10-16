

test_that("sample_proper_car returns expected shapes and finite values", {
  set.seed(1)

  # simple 4-node path graph adjacency
  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1

  y <- rep(0, 4)

  fit <- sample_proper_car(
    y = y, A = A, rho = 0.9,
    n_iter = 50, burn = 10, thin = 2,
    a_tau = 1, b_tau = 1, a_kappa = 1, b_kappa = 1
  )

  expect_true(is.list(fit))
  expect_true(is.matrix(fit$x))
  expect_equal(ncol(fit$x), 4)
  expect_equal(nrow(fit$x), length(seq.int(11, 50, by = 2)))
  expect_equal(length(fit$tau), nrow(fit$x))
  expect_equal(length(fit$kappa), nrow(fit$x))

  expect_true(all(is.finite(fit$x)))
  expect_true(all(is.finite(fit$tau)))
  expect_true(all(is.finite(fit$kappa)))
  expect_true(all(fit$tau > 0))
  expect_true(all(fit$kappa > 0))
})


test_that("reproducibility: same seed gives identical draws", {
  # 3-node chain
  A <- Matrix::Matrix(0, 3, 3, sparse = TRUE)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  y <- c(0, 0, 0)

  set.seed(123)
  fit1 <- sample_proper_car(y, A, rho = 0.8, n_iter = 25)

  set.seed(123)
  fit2 <- sample_proper_car(y, A, rho = 0.8, n_iter = 25)

  expect_equal(fit1$x, fit2$x)
  expect_equal(fit1$tau, fit2$tau)
  expect_equal(fit1$kappa, fit2$kappa)
})


test_that("posterior shrinks toward 0 when y = 0", {
  set.seed(2)

  # 5-node path
  n <- 5
  A <- Matrix::Matrix(0, n, n, sparse = TRUE)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }

  y <- rep(0, n)

  fit <- sample_proper_car(
    y = y, A = A, rho = 0.9,
    n_iter = 80, burn = 20, thin = 2,
    a_tau = 2, b_tau = 2, a_kappa = 2, b_kappa = 2
  )

  post_mean <- colMeans(fit$x)
  # loose threshold; ensure no obvious drift/explosion
  expect_true(all(abs(post_mean) < 0.25))
})


test_that("asymmetric A works when symmetrize=TRUE and fails when symmetrize=FALSE (check=TRUE)", {
  set.seed(3)

  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1, 2] <- 1
  A[2, 3] <- 1
  A[3, 4] <- 1
  # intentionally asymmetric

  y <- rep(0, 4)

  # symmetrize should make it usable
  fit <- sample_proper_car(y, A, rho = 0.7, n_iter = 10, symmetrize = TRUE, check = TRUE)
  expect_equal(ncol(fit$x), 4)

  # without symmetrize, as_sparse_adjacency/check should complain
  expect_error(
    sample_proper_car(y, A, rho = 0.7, n_iter = 10, symmetrize = FALSE, check = TRUE)
  )
})


test_that("proper CAR rejects isolates", {
  # isolated node 4
  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1

  y <- rep(0, 4)

  expect_error(
    sample_proper_car(y, A, rho = 0.9, n_iter = 10),
    "isolated"
  )
})


test_that("extreme rho values are handled (near-boundary but admissible)", {
  set.seed(4)

  # 6-node cycle (no isolates)
  n <- 6
  A <- Matrix::Matrix(0, n, n, sparse = TRUE)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }
  A[1, n] <- 1; A[n, 1] <- 1

  y <- rep(0, n)

  # rho close to 1 but still < 1; should run (might be numerically tougher)
  fit <- sample_proper_car(y, A, rho = 0.999, n_iter = 20, burn = 0, thin = 1)
  expect_true(all(is.finite(fit$x)))
  expect_true(all(is.finite(fit$tau)))
  expect_true(all(is.finite(fit$kappa)))
})


test_that("invalid sampler arguments fail fast (adversarial inputs)", {
  A <- Matrix::Matrix(0, 3, 3, sparse = TRUE)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  y <- rep(0, 3)

  expect_error(sample_proper_car(y = "nope", A = A, n_iter = 10))
  expect_error(sample_proper_car(y = y, A = A, n_iter = 0))
  expect_error(sample_proper_car(y = y, A = A, n_iter = 10, burn = 10))
  expect_error(sample_proper_car(y = y, A = A, n_iter = 10, thin = 0))
  expect_error(sample_proper_car(y = y, A = A, rho = 1.0, n_iter = 10))  # car_precision check
  expect_error(sample_proper_car(y = y, A = A, rho = -1.0, n_iter = 10)) # car_precision check

  expect_error(sample_proper_car(y = y[-1], A = A, n_iter = 10)) # length mismatch

  # bad init
  expect_error(sample_proper_car(y = y, A = A, n_iter = 10, init = list(tau = -1)))
  expect_error(sample_proper_car(y = y, A = A, n_iter = 10, init = list(kappa = 0)))
  expect_error(sample_proper_car(y = y, A = A, n_iter = 10, init = list(x = c(1, 2))))
})


test_that("extreme y values do not produce NaN/Inf", {
  set.seed(5)

  # 5-node complete graph minus diagonal (dense-ish)
  n <- 5
  A <- Matrix::Matrix(1, n, n, sparse = TRUE)
  Matrix::diag(A) <- 0

  # very large magnitude observations
  y <- c(1e6, -1e6, 5e5, -5e5, 1e6)

  fit <- sample_proper_car(
    y = y, A = A, rho = 0.5,
    n_iter = 30, burn = 10, thin = 1,
    a_tau = 1, b_tau = 1,
    a_kappa = 1, b_kappa = 1
  )

  expect_true(all(is.finite(fit$x)))
  expect_true(all(is.finite(fit$tau)))
  expect_true(all(is.finite(fit$kappa)))
  expect_true(all(fit$tau > 0))
  expect_true(all(fit$kappa > 0))
})


test_that("extreme hyperpriors remain well-defined (small/large shapes and rates)", {
  set.seed(6)

  # 4-node chain
  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1
  y <- rep(0, 4)

  # very small shape/rate (still positive) and very large rate
  fit <- sample_proper_car(
    y = y, A = A, rho = 0.7,
    n_iter = 25,
    a_tau = 1e-3, b_tau = 1e3,
    a_kappa = 1e-3, b_kappa = 1e3
  )

  expect_true(all(is.finite(fit$tau)))
  expect_true(all(is.finite(fit$kappa)))
  expect_true(all(fit$tau > 0))
  expect_true(all(fit$kappa > 0))
})


test_that("ill-conditioned posterior precision does not crash (rho ~ 1, large kappa, small tau)", {
  set.seed(7)

  # 8-node cycle (no isolates, moderately connected)
  n <- 8
  A <- Matrix::Matrix(0, n, n, sparse = TRUE)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }
  A[1, n] <- 1
  A[n, 1] <- 1

  # mild data signal; not all zeros
  y <- seq(-1, 1, length.out = n)

  # rho near boundary, huge kappa, tiny tau (posterior precision ~ kappa Q)
  fit <- sample_proper_car(
    y = y, A = A, rho = 0.999,
    n_iter = 15, burn = 0, thin = 1,
    init = list(
      x = rep(0, n),
      tau = 1e-8,
      kappa = 1e8
    )
  )

  expect_true(all(is.finite(fit$x)))
  expect_true(all(is.finite(fit$tau)))
  expect_true(all(is.finite(fit$kappa)))
  expect_true(all(fit$tau > 0))
  expect_true(all(fit$kappa > 0))
})
