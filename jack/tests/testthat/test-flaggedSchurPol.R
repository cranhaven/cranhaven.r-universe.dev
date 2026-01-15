test_that("Flagged tableaux", {
  lambda <- c(5, 4, 3, 2)
  a <- c(1, 1, 2, 3)
  b <- c(2, 3, 4, 5)
  ssytx <- flaggedSemiStandardYoungTableaux(lambda, a, b)
  l <- length(lambda)
  check <- function(ssyt) {
    all(vapply(1:l, function(i) {
      all(ssyt[[i]] >= a[i]) && all(ssyt[[i]] <= b[i])
    }, logical(1L)))
  }
  expect_true(all(vapply(ssytx, check, logical(1L))))
})

test_that("Flagged Schur polynomial", {
  lambda <- c(5, 4, 3, 2)
  l <- length(lambda)
  n <- 5
  a <- rep(1, l)
  b <- rep(n, l)
  flaggedPoly <- flaggedSchurPol(lambda, a, b)
  poly <- SchurPol(n, lambda)
  expect_true(flaggedPoly == poly)
})
