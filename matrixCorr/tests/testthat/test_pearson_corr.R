test_that("pearson_corr matches base::cor(..., method = 'pearson')", {
  for (i in 1:10) {
    n <- sample(c(10, 50, 100, 500), 1)
    p <- sample(2:6, 1)
    mat <- replicate(p, rnorm(n))
    colnames(mat) <- paste0("P", seq_len(p))

    base_cor <- cor(mat, method = "pearson")
    fast_cor <- suppressWarnings(pearson_corr(mat))

    attributes(fast_cor) <- NULL
    attributes(base_cor) <- NULL

    expect_equal(
      fast_cor,
      base_cor,
      tolerance = 1e-10,
      info = paste("Mismatch on Pearson test dataset", i, "n =", n, "p =", p)
    )
  }
})

test_that("pearson_corr matches stats::cor on transformed data", {
  set.seed(123)
  X <- matrix(rnorm(200), ncol = 2)

  X_affine <- X
  X_affine[,1] <-  3 * X_affine[,1] + 5
  X_affine[,2] <- -2 * X_affine[,2] + 7

  base <- unname(cor(X_affine, method = "pearson"))
  fast <- unname(as.matrix(unclass(pearson_corr(X_affine))))

  expect_equal(as.numeric(fast),
               as.numeric(unname(base)),
               tolerance = 1e-12)
})

test_that("pearson_corr returns NA when a column is constant", {
  X <- cbind(a = rnorm(20), b = rep(1, 20))
  pc <- pearson_corr(X)

  expect_true(all(is.na(pc["b", ])))
  expect_true(all(is.na(pc[, "b"])))
})

test_that("pearson_corr matches base::cor on a known toy dataset", {
  X <- matrix(c(1, 2, 3, 4,
                2, 4, 6, 8), ncol = 2)
  colnames(X) <- c("x", "y")

  base_cor <- cor(X, method = "pearson")
  fast_cor <- pearson_corr(X)
  expect_equal(as.numeric(fast_cor),
               as.numeric(unname(base_cor)),
               tolerance = 1e-12)
})

test_that("pearson_corr recovers true correlation in large MVN sample", {
  set.seed(42)
  p <- 2; n <- 5000; rho <- 0.7
  Sigma <- matrix(c(1, rho, rho, 1), 2, 2)
  Z <- MASS::mvrnorm(n, mu = c(0,0), Sigma = Sigma)

  est <- pearson_corr(Z)[1,2]

  expect_equal(est, rho, tolerance = 0.02,
               info = paste("Estimated =", round(est,3),
                            "Expected =", rho))
})

