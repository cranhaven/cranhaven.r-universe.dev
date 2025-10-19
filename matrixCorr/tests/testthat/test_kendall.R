test_that("kendall_tau matches base::cor(..., method = 'kendall')", {
  for (i in 1:10) {
    n <- sample(c(10, 50, 100, 500), 1)
    p <- sample(2:6, 1)
    mat <- replicate(p, rnorm(n))
    colnames(mat) <- paste0("V", seq_len(p))

    base_cor <- cor(mat, method = "kendall")
    fast_cor <- suppressWarnings(kendall_tau(mat))

    attributes(fast_cor) <- NULL
    attributes(base_cor) <- NULL

    expect_equal(
      fast_cor,
      base_cor,
      tolerance = 1e-8,
      info = paste("Mismatch on test dataset", i, "n =", n, "p =", p)
    )
  }
})

test_that("kendall_tau handles ties correctly and matches base::cor", {
 for (i in 1:5) {
    n <- sample(c(50, 100, 200), 1)
    p <- sample(2:5, 1)

    # Create tied data
    mat <- replicate(p, sample(rep(1:5, length.out = n)))
    colnames(mat) <- paste0("T", seq_len(p))

    base_cor <- cor(mat, method = "kendall")
    fast_cor <- suppressWarnings(kendall_tau(mat))

    attributes(fast_cor) <- NULL
    attributes(base_cor) <- NULL

    expect_equal(
      fast_cor,
      base_cor,
      tolerance = 1e-8,
      info = paste("Mismatch on tied test dataset", i, "n =", n, "p =", p)
    )
  }
})

test_that("kendall_tau numerics match stats::cor(Kendall)", {
  set.seed(123)

  X <- matrix(rnorm(200), ncol = 2)
  X_mono <- cbind(exp(X[,1]), log1p(exp(X[,2])))  # strictly monotone ↑

  base_cor <- unname(cor(X, method = "kendall"))

  # Coerce fast result to a bare numeric matrix (no class, no dimnames)
  fast_raw <- kendall_tau(X_mono)
  fast_cor <- unname(as.matrix(fast_raw))
  class(fast_cor) <- NULL
  dimnames(fast_cor) <- NULL

  # Basic shape/symmetry checks
  expect_equal(dim(fast_cor), dim(base_cor))
  expect_true(isSymmetric(matrix(fast_cor, nrow = nrow(base_cor))))
  expect_equal(diag(fast_cor), rep(1, ncol(X)))

  # Numerical equality
  expect_equal(as.numeric(fast_cor),
               as.numeric(unname(cor(X, method = "kendall"))),
               tolerance = 1e-12)

  # Also check 2-vector path and sign flip under decreasing transform
  x <- X[,1]; y <- X[,2]
  base12 <- cor(x, y, method = "kendall")
  fast12 <- as.numeric(as.matrix(kendall_tau(cbind(x, y)))[1,2])
  expect_equal(fast12, base12, tolerance = 1e-12)

  y_rev <- -y
  expect_equal(
    as.numeric(as.matrix(kendall_tau(cbind(x, y_rev)))[1,2]),
    -fast12, tolerance = 1e-12
  )
})


test_that("kendall_tau returns NA when a column is constant", {
  X <- cbind(a = rnorm(20), b = rep(1, 20))
  kt <- kendall_tau(X)

  expect_true(all(is.na(kt["b", 1])))
  expect_true(all(is.na(kt[1, "b"])))
})

test_that("kendall_tau matches base::cor on a known toy dataset", {
  X <- matrix(c(1, 2, 3, 4,
                4, 3, 2, 1), ncol = 2)
  colnames(X) <- c("x", "y")

  base_cor <- cor(X, method = "kendall")
  fast_cor <- kendall_tau(X)

  expect_equal(as.numeric(fast_cor),
               as.numeric(unname(base_cor)),
               tolerance = 1e-12)
})

test_that("kendall_tau estimates agree with theoretical BVN relationship", {
  # For BVN, tau = (2/pi) * arcsin(r)
  set.seed(321)
  rhos <- c(-0.8, -0.4, 0, 0.4, 0.8)
  n <- 2000

  for (r in rhos) {
    Sigma <- matrix(c(1, r, r, 1), 2, 2)
    Z <- MASS::mvrnorm(n, mu = c(0,0), Sigma = Sigma)

    est <- kendall_tau(Z)[1,2]
    theory <- (2 / pi) * asin(r)

    expect_equal(est, theory, tolerance = 0.05,
                 info = paste("Mismatch for true rho =", r))
  }
})
