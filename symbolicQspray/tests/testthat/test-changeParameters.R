test_that("changeParameters", {
  f <- function(a, X, Y) {
    a^2 / (a + 1) * X^2*Y  +  (3*a - 2) / a * Y^2
  }
  a <- qlone(1)
  X <- Qlone(1)
  Y <- Qlone(2)
  Qspray <- f(a, X, Y)
  b <- a^2 + 1
  expect_true(changeParameters(Qspray, list(b)) == f(b, X, Y))
})

test_that("changeParameters in Jacobi polynomial", {
  n <- 5
  JPn   <- JacobiPolynomial(n)
  JPnm1 <- JacobiPolynomial(n-1)
  a <- qlone(1)
  b <- qlone(2)
  lhs <- derivSymbolicQspray(JPn, 1)
  rhs <- (1 + a + b + n)/2 * changeParameters(JPnm1, list(a+1, b+1))
  expect_true(lhs == rhs)
})

