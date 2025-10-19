# tests/testthat/test-schafer-corr.R

test_that("base-R reference agrees (within tolerance) and attributes sane", {
  # Helper: base-R reference shrinkage in correlation space (Schafer–Strimmer)
  schafer_shrink_base <- function(X) {
    stopifnot(is.matrix(X))
    n <- nrow(X)
    R <- stats::cor(X)

    # Handle zero-variance variables in the same way as schafer_corr()
    zero <- is.na(diag(R))
    if (any(zero)) {
      R[zero, ] <- NA_real_
      R[, zero] <- NA_real_
    }

    off <- upper.tri(R, diag = FALSE)
    r   <- R[off]
    r   <- r[is.finite(r)]
    sum_sq <- sum(r^2)

    if (sum_sq > 0) {
      v <- ((1 - r^2)^2) / (n - 1)
      lambda <- sum(v) / sum_sq
      lambda <- min(max(lambda, 0), 1)
    } else {
      lambda <- 1
    }

    Rsh <- R
    Rsh[off] <- (1 - lambda) * R[off]
    Rsh[lower.tri(Rsh)] <- t(Rsh)[lower.tri(Rsh)]
    diag(Rsh) <- ifelse(is.na(diag(R)), NA_real_, 1)
    attr(Rsh, "lambda") <- lambda
    Rsh
  }

  # Helper: drop non-structural attributes to compare pure numerics
  drop_attrs <- function(m) {
    m2 <- as.matrix(m)
    attributes(m2) <- attributes(m2)[c("dim","dimnames")]
    m2
  }

  set.seed(123)
  n <- 80; p <- 30
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("V", seq_len(p))

  R_pkg  <- schafer_corr(X)
  R_base <- schafer_shrink_base(X)

  # Class & metadata
  expect_s3_class(R_pkg, "schafer_corr")
  expect_true(is.matrix(R_pkg))
  expect_true(isSymmetric(R_pkg))
  expect_identical(attr(R_pkg, "method"), "schafer_shrinkage")
  expect_true(grepl("Scha", attr(R_pkg, "description"), fixed = TRUE))

  # Numerics vs base reference:
  # - Compare bare matrices (ignore differing attributes/classes)
  # - Allow a small tolerance for BLAS/centering path differences
  expect_equal(
    drop_attrs(R_pkg),
    drop_attrs(R_base),
    tolerance = 2e-4, scale = 1
  )

  # Diagonal ones in this case (no zero-variance)
  expect_true(all(diag(R_pkg) == 1))
})

test_that("deterministic small example matches base reference tightly", {
  # A tiny, deterministic matrix (fixed values, no NAs)
  X <- matrix(
    c(1.1, 0.3, -0.7,
      0.9, 0.4, -0.5,
      1.2, 0.2, -0.6,
      0.8, 0.5, -0.4,
      1.0, 0.1, -0.8),
    nrow = 5, ncol = 3, byrow = TRUE
  )
  colnames(X) <- c("A","B","C")

  # Base reference
  schafer_shrink_base <- function(X) {
    n <- nrow(X); R <- stats::cor(X)
    off <- upper.tri(R, diag = FALSE)
    r <- R[off]; r <- r[is.finite(r)]
    sum_sq <- sum(r^2)
    lambda <- if (sum_sq > 0) {
      min(max(sum(((1 - r^2)^2) / (n - 1)) / sum_sq, 0), 1)
    } else 1
    Rsh <- R
    Rsh[off] <- (1 - lambda) * R[off]
    Rsh[lower.tri(Rsh)] <- t(Rsh)[lower.tri(Rsh)]
    diag(Rsh) <- 1
    Rsh
  }

  R_pkg  <- schafer_corr(X)
  R_base <- schafer_shrink_base(X)

  # Put this at the top of the test file
  num_only <- function(x) {
    m <- as.matrix(x)
    attributes(m) <- attributes(m)[c("dim", "dimnames")]  # keep only structure
    unname(m)  # also drop dimnames for a pure numeric compare
  }

  # Then compare
  expect_equal(
    num_only(R_pkg),
    num_only(R_base),
    tolerance = 1e-10
  )
})

test_that("zero-variance column propagates NA row/column", {
  skip_on_cran()

  set.seed(42)
  n <- 50; p <- 6
  X <- matrix(rnorm(n * (p - 1)), n, p - 1)
  X <- cbind(X, const = 5)  # constant column
  colnames(X) <- paste0("V", seq_len(p))

  R_pkg <- schafer_corr(X)

  j <- p
  expect_true(all(is.na(R_pkg[j, ])))
  expect_true(all(is.na(R_pkg[, j])))
  expect_true(all(diag(R_pkg)[-j] == 1))
  expect_true(isSymmetric(R_pkg))
})

test_that("independent columns → closer to identity than raw cor()", {
  skip_on_cran()

  set.seed(99)
  n <- 60; p <- 40
  X <- matrix(rnorm(n * p), n, p)

  R_raw <- stats::cor(X)
  R_shr <- schafer_corr(X)
  I <- diag(p)

  # Frobenius norm: shrinkage should reduce error to identity
  err_raw <- sqrt(sum((R_raw - I)^2))
  err_shr <- sqrt(sum((R_shr - I)^2))
  expect_lt(err_shr, err_raw)

  # With lambda computed from the raw R, off-diagonals should match (1 - lambda)*R_raw
  off <- upper.tri(R_raw, diag = FALSE)
  r <- R_raw[off]
  lambda_hat <- {
    r2 <- r^2
    sum_sq <- sum(r2[is.finite(r2)])
    if (sum_sq > 0) {
      v <- ((1 - r^2)^2) / (n - 1)
      min(max(sum(v[is.finite(v)]) / sum_sq, 0), 1)
    } else 1
  }
  expect_equal(
    unname(as.vector(R_shr[off])),
    unname(as.vector((1 - lambda_hat) * r)),
    tolerance = 5e-5
  )
})

test_that("p >> n returns symmetric PSD within tolerance", {
  skip_on_cran()

  set.seed(2024)
  n <- 30; p <- 120
  X <- matrix(rnorm(n * p), n, p)

  R_shr <- schafer_corr(X)

  expect_equal(dim(R_shr), c(p, p))
  expect_true(isSymmetric(R_shr))

  # Numerical PSD check: eigenvalues >= small negative tolerance
  ev <- eigen(R_shr, symmetric = TRUE, only.values = TRUE)$values
  expect_gte(min(ev), -1e-8)
})


test_that("handles zero-variance columns by propagating NA row/col", {

  set.seed(42)
  n <- 50; p <- 6
  X <- matrix(rnorm(n * (p - 1)), n, p - 1)
  const_col <- rep(5, n)
  X <- cbind(X, const_col)
  colnames(X) <- paste0("V", seq_len(p))

  R_pkg <- schafer_corr(X)

  # The constant column should be fully NA (row and column), including diagonal
  j <- p
  expect_true(all(is.na(R_pkg[j, ])))
  expect_true(all(is.na(R_pkg[, j])))

  # Other diagonals must be 1
  expect_true(all(diag(R_pkg)[-j] == 1))

  # Symmetry preserved
  expect_true(isSymmetric(R_pkg))
})

test_that("agrees with data.frame inputs and ignores non-numeric", {
  set.seed(7)
  n <- 40
  df <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    grp = rep(letters[1:4], length.out = n),      # non-numeric
    flag = rep(c(TRUE, FALSE), length.out = n)    # non-numeric logical
  )
  out <- schafer_corr(df)

  # Only numeric columns 'a' and 'b' should be used
  expect_equal(dim(out), c(2, 2))
  expect_setequal(colnames(out), c("a", "b"))
  expect_true(isSymmetric(out))
  expect_true(all(diag(out) == 1))
})

test_that("known-truth scenario", {
  # Truth: columns are independent -> population correlation = I_p
  # The shrinkage estimator should be clsoer
  set.seed(99)
  n <- 60; p <- 40
  X <- matrix(rnorm(n * p), n, p)

  R_raw <- stats::cor(X)
  R_shr <- schafer_corr(X)
  I <- diag(p)

  # Frobenius norm errors
  err_raw <- sqrt(sum((R_raw - I)^2))
  err_shr <- sqrt(sum((R_shr - I)^2))
  expect_lt(err_shr, err_raw)

  # Shrinkage applies a uniform factor to off-diagonals: ratio should be ~constant
  off <- upper.tri(R_raw, diag = FALSE)
  r_raw <- R_raw[off]; r_shr <- R_shr[off]
  idx <- is.finite(r_raw) & (abs(r_raw) > 0)
  ratio <- r_shr[idx] / r_raw[idx]
  # All ratios should equal (1 - lambda); allow some numerical scatter
  expect_lt(sd(ratio), 1e-10)  # extremely tight because it is a pure scaling
  expect_true(all(abs(ratio - ratio[1]) < 1e-10))
})

test_that("large p > n returns a valid symmetric correlation", {
  skip_on_cran()

  set.seed(2024)
  n <- 30; p <- 120
  X <- matrix(rnorm(n * p), n, p)

  R_shr <- schafer_corr(X)

  expect_equal(dim(R_shr), c(p, p))
  expect_true(isSymmetric(R_shr))

  # Diagonal ones
  expect_true(all(diag(R_shr) == 1))

  # Eigenvalues should be >= 0 within numerical tolerance
  ev <- eigen(R_shr, symmetric = TRUE, only.values = TRUE)$values
  expect_gte(min(ev), -1e-8)
})
