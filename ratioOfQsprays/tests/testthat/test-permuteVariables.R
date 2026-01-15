test_that("permuteVariables and swapVariables", {
  f <- function(x, y) {
    (x^2 + 5*y - 1) / (x + 1)
  }
  x <- qlone(1)
  y <- qlone(2)
  R <- f(x, y)
  S <- changeVariables(R, list(y, x))
  expect_true(permuteVariables(R, c(2, 1)) == S)
  expect_true(swapVariables(R, 1, 2) == S)
  z <- qlone(3)
  expect_true(permuteVariables(R, c(1, 3, 2)) == f(x, z))
  expect_true(swapVariables(R, 2, 3) == f(x, z))
})
