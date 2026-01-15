test_that("Flagged skew tableaux", {
  lambda <- c(5, 4, 3, 2)
  mu <- c(3, 2, 1)
  a <- c(1, 1, 2, 3)
  b <- c(2, 3, 4, 5)
  skewtx <- flaggedSkewTableaux(lambda, mu, a, b)
  l <- length(lambda)
  check <- function(skewt) {
    all(vapply(1:l, function(i) {
      row <- Filter(Negate(is.na), skewt[[i]])
      all(row >= a[i]) && all(row <= b[i])
    }, logical(1L)))
  }
  expect_true(all(vapply(skewtx, check, logical(1L))))
})

test_that("Flagged skew Schur polynomial", {
  lambda <- c(4, 4, 3, 2)
  mu <- c(3, 2, 1)
  l <- length(lambda)
  n <- 4
  a <- rep(1, l)
  b <- rep(n, l)
  flaggedPoly <- flaggedSkewSchurPol(lambda, mu, a, b)
  poly <- SkewSchurPol(n, lambda, mu)
  expect_true(flaggedPoly == poly)
})
