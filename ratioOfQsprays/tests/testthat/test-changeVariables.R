test_that("changeVariables", {
  f <- function(x, y) {
    (x^2 + 5*y - 1) / (x + 1)
  }
  x <- qlone(1)
  y <- qlone(2)
  R <- f(x, y)
  X <- x^2
  Y <- x + y + 1
  S <- changeVariables(R, list(X, Y))
  expect_true(S == f(X, Y))
})
