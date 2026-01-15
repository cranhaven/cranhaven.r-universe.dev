test_that("changeVariables", {
  f <- function(a, X, Y) {
    a^2 / (a + 1) * X^2*Y  +  (3*a - 2) / a * Y^2
  }
  a <- qlone(1)
  X <- Qlone(1)
  Y <- Qlone(2)
  Qspray <- f(a, X, Y)
  U <- X + a
  V <- Y + a
  expect_true(changeVariables(Qspray, list(U, V)) == f(a, U, V))
})
