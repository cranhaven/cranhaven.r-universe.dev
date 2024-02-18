test_that("fast_cov", {
  a <- matrix(rnorm(1000), nrow = 100)
  b <- matrix(1:5000, nrow = 100)
  eps <- .Machine$double.eps^0.5

  # default
  y <- cov(a)
  z <- fast_cov(a)
  expect_equal(z, y, tolerance = eps)

  y <- cov(a, b)
  z <- fast_cov(a, b)
  expect_equal(z, y, tolerance = eps)

  # Selected columns
  col1 <- sample(c(NA, -3:13), 20, replace = TRUE)
  col1_ <- col1
  col1[is.na(col1) | col1 < 1 | col1 > 10] <- NA

  y <- cov(a[, col1], b)
  z <- fast_cov(a, b, col_x = col1_)
  expect_equal(z, y, tolerance = eps)

  col2 <- sample(c(NA, -3:60), 100, replace = TRUE)
  col2_ <- col2
  col2[is.na(col2) | col2 < 1 | col2 > 50] <- NA

  y <- cov(a, b[, col2])
  z <- fast_cov(a, b, col_y = col2_)
  expect_equal(z, y, tolerance = eps)

  y <- cov(a[, col1], b[, col2])
  z <- fast_cov(a, b, col_x = col1_, col_y = col2_)
  expect_equal(z, y, tolerance = eps)

})
