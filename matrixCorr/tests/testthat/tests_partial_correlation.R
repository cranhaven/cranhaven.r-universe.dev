# Helper: direct partial correlation from a precision matrix
pcor_from_precision <- function(Theta) {
  d <- diag(Theta)
  S <- -Theta / sqrt(outer(d, d))
  diag(S) <- 1
  S
}

# Helper: OAS shrinkage (to scaled identity) IMPLEMENTED IN R to mirror C++ code
oas_shrink_R <- function(X) {
  n <- nrow(X); p <- ncol(X)
  mu <- colMeans(X)
  Xc <- sweep(X, 2, mu, check.margin = FALSE)
  S_mle <- crossprod(Xc) / n  # MLE scaling
  trS   <- sum(diag(S_mle))
  trS2  <- sum(S_mle * S_mle)
  mu_sc <- trS / p
  num <- (1 - 2 / p) * trS2 + trS^2
  den <- (n + 1 - 2 / p) * (trS2 - trS^2 / p)
  rho <- if (den > 0) num / den else 1
  rho <- max(0, min(1, rho))
  Sigma <- (1 - rho) * S_mle
  diag(Sigma) <- diag(Sigma) + rho * mu_sc
  list(Sigma = Sigma, rho = rho)
}

# ------------------------------------------------------------------------------
test_that("Sample partial correlation close to truth (Fisher-z tolerance)", {
  testthat::skip_if_not_installed("MASS")
  set.seed(20240819)

  p <- 8
  alpha <- 0.3
  Theta <- diag(p)
  for (j in 1:(p - 1)) Theta[j, j + 1] <- Theta[j + 1, j] <- -alpha
  Sigma <- solve(Theta)

  n <- 1500
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- paste0("V", seq_len(p))

  ours  <- partial_correlation(X, method = "sample", return_cov_precision = FALSE)$pcor
  truth <- {
    d <- diag(Theta); S <- -Theta / sqrt(outer(d, d)); diag(S) <- 1; S
  }

  # Fisher-z transform and SE
  UT <- upper.tri(ours, diag = FALSE)
  z_hat  <- atanh(ours[UT])
  z_true <- atanh(truth[UT])
  se_z   <- 1 / sqrt(n - p - 1)

  # 3-sigma band on z-scale
  expect_lt(max(abs(z_hat - z_true)), 3 * se_z)

  # Structural zeros (non-edges) are small on r-scale
  non_edge <- matrix(TRUE, p, p)
  diag(non_edge) <- FALSE
  non_edge[abs(row(non_edge) - col(non_edge)) == 1] <- FALSE
  expect_lt(max(abs(ours[non_edge])), 0.06)  # slightly relaxed

  # Sanity
  expect_true(isSymmetric(ours, tol = 1e-12))
  expect_equal(as.numeric(diag(ours)), rep(1, p), tolerance = 1e-12)
})

# ------------------------------------------------------------------------------
test_that("Sample method equals base-R construction from unbiased covariance", {
  set.seed(123)
  n <- 200; p <- 12
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("V", seq_len(p))

  # Our function
  ours <- partial_correlation(X, method = "sample", return_cov_precision = TRUE)

  # Base-R reference
  S_unb  <- stats::cov(X)                # unbiased covariance (n-1)
  Theta  <- solve(S_unb)
  d      <- diag(Theta)
  ref    <- -Theta / sqrt(outer(d, d))
  diag(ref) <- 1

  expect_equal(ours$pcor, ref, tolerance = 1e-10)
  expect_true(isSymmetric(ours$pcor))
  expect_equal(as.numeric(diag(ours$pcor)), rep(1, p))
})

# ------------------------------------------------------------------------------
test_that("Ridge method equals base-R ridge construction", {
  set.seed(99)
  n <- 180; p <- 10
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("V", seq_len(p))
  lambda <- 5e-3

  # Our function
  ours <- partial_correlation(X, method = "ridge", lambda = lambda,
                              return_cov_precision = TRUE)

  # Base-R reference: Sigma = cov + lambda * I
  S_unb   <- stats::cov(X)
  diag(S_unb) <- diag(S_unb) + lambda
  Theta   <- solve(S_unb)
  d       <- diag(Theta)
  ref     <- -Theta / sqrt(outer(d, d))
  diag(ref) <- 1

  expect_equal(ours$pcor, ref, tolerance = 1e-10)
  # Also check Sigma %*% Theta approx I
  I <- diag(p)
  dimnames(I) <- dimnames(S_unb)  # carry names
  expect_equal(S_unb %*% Theta, I, tolerance = 1e-8)
})

# ------------------------------------------------------------------------------
test_that("OAS method matches an R implementation of the same formula", {
  set.seed(7)
  n <- 120; p <- 25
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("V", seq_len(p))

  # Our function
  ours <- partial_correlation(X, method = "oas", return_cov_precision = TRUE)

  # R implementation of the identical OAS shrinkage (to scaled identity)
  oas <- oas_shrink_R(X)
  Sigma <- oas$Sigma
  Theta <- solve(Sigma)
  d     <- diag(Theta)
  ref   <- -Theta / sqrt(outer(d, d))
  diag(ref) <- 1

  expect_equal(ours$pcor, ref, tolerance = 1e-10)
  expect_true(isSymmetric(ours$pcor))
  expect_equal(as.numeric(diag(ours$pcor)), rep(1, p))
})

# ------------------------------------------------------------------------------
test_that("p >> n: sample may fail, OAS returns finite, well-formed matrix", {
  set.seed(4242)
  n <- 40; p <- 80
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("V", seq_len(p))

  # OAS should succeed, be symmetric, unit diagonal, and bounded in [-1, 1]
  oas <- partial_correlation(X, method = "oas", return_cov_precision = TRUE)
  M <- oas$pcor
  expect_true(is.matrix(M))
  expect_true(isSymmetric(M, tol = 1e-12))
  expect_equal(as.numeric(diag(M)), rep(1, p), tolerance = 1e-12)
  expect_true(all(is.finite(M)))
  expect_true(all(abs(M[upper.tri(M, diag = FALSE)]) <= 1 + 1e-10))

  # Covariance/precision consistency
  I <- diag(p)
  dimnames(I) <- dimnames(oas$cov)  # carry names
  expect_equal(oas$cov %*% oas$precision, I, tolerance = 1e-6)
})

# ------------------------------------------------------------------------------
test_that("Non-numeric columns are ignored and dimnames propagate", {
  set.seed(1)
  X <- data.frame(
    a = rnorm(50),
    b = rnorm(50),
    f = factor(sample(letters[1:3], 50, TRUE)),  # must be dropped
    c = rnorm(50),
    s = sample(c("x","y","z"), 50, TRUE),        # character, dropped
    l = sample(c(TRUE, FALSE), 50, TRUE)         # logical, dropped
  )
  cols <- c("a", "b", "c")

  pc <- partial_correlation(X, method = "oas", return_cov_precision = FALSE)$pcor
  expect_equal(dim(pc), c(3L, 3L))
  expect_equal(dimnames(pc), list(cols, cols))
})
