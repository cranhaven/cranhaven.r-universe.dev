test_that("spearman_rho matches base::cor(..., method = 'spearman')", {
  for (i in 1:10) {
    n <- sample(c(10, 50, 100, 500), 1)
    p <- sample(2:6, 1)
    mat <- replicate(p, rnorm(n))
    colnames(mat) <- paste0("S", seq_len(p))

    base_cor <- cor(mat, method = "spearman")
    fast_cor <- suppressWarnings(spearman_rho(mat))

    attributes(fast_cor) <- NULL
    attributes(base_cor) <- NULL

    expect_equal(
      fast_cor,
      base_cor,
      tolerance = 1e-8,
      info = paste("Mismatch on Spearman test dataset", i, "n =", n, "p =", p)
    )
  }
})

test_that("spearman_rho handles ties correctly and matches base::cor", {
  for (i in 1:5) {
    n <- sample(c(50, 100, 200), 1)
    p <- sample(2:5, 1)

    mat <- replicate(p, sample(rep(1:5, length.out = n)))
    colnames(mat) <- paste0("T", seq_len(p))

    base_cor <- cor(mat, method = "spearman")
    fast_cor <- suppressWarnings(spearman_rho(mat))

    attributes(fast_cor) <- NULL
    attributes(base_cor) <- NULL

    expect_equal(
      fast_cor,
      base_cor,
      tolerance = 1e-8,
      info = paste("Mismatch on tied Spearman dataset", i, "n =", n, "p =", p)
    )
  }
})

test_that("spearman_rho is invariant to strictly monotone transformations", {
  set.seed(123)
  X <- matrix(rnorm(200), ncol = 2)
  X_mono <- X
  X_mono[,1] <- exp(X_mono[,1])     # monotone transform
  X_mono[,2] <- log1p(exp(X_mono[,2]))  # monotone transform

  base_cor <- cor(X, method = "spearman")
  fast_cor <- spearman_rho(X_mono)

  expect_equal(as.numeric(fast_cor),
               as.numeric(unname(base_cor)),
               tolerance = 1e-12)
})

test_that("spearman_rho returns NA when a column is constant", {
  X <- cbind(a = rnorm(20), b = rep(1, 20))
  sp <- spearman_rho(X)

  expect_true(all(is.na(sp["b", ])))
  expect_true(all(is.na(sp[, "b"])))
})

test_that("spearman_rho agrees with base::cor on known values (small matrix)", {
  X <- matrix(c(1, 2, 3, 4,
                1, 4, 9, 16), ncol = 2)
  colnames(X) <- c("x", "y")

  base_cor <- cor(X, method = "spearman")
  fast_cor <- spearman_rho(X)

  expect_equal(as.numeric(fast_cor),
               as.numeric(unname(base_cor)),
               tolerance = 1e-12)
})

test_that("theoretical BVN formula for Spearman holds approximately", {
  set.seed(321)
  rhos <- c(-0.8, -0.4, 0, 0.4, 0.8)
  n <- 2000

  for (r in rhos) {
    Sigma <- matrix(c(1, r, r, 1), 2, 2)
    Z <- MASS::mvrnorm(n, mu = c(0,0), Sigma = Sigma)

    est <- spearman_rho(Z)[1,2]
    theory <- (6/pi) * asin(r/2)

    expect_equal(est, theory, tolerance = 0.05,
                 info = paste("Mismatch for true rho =", r))
  }
})
