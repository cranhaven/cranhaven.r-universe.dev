test_that("Integral of exp(-a*x) on [0,1]", {
  f <- function(x, a) exp(-a*x)
  expected <- function(a, lower, upper) {
    (1/a)*(exp(-a*lower)-exp(-a*upper))
  }
  a <- 0.1
  l <- 0.0
  u <- 2.0
  result <- deformula.moneone(f, l, u, a=a)
  expect_equal(result$value, expected(a, l, u))
  a <- 0.2
  l <- -1.0
  u <- 2.0
  result <- deformula.moneone(f, l, u, a=a)
  expect_equal(result$value, expected(a, l, u))
  a <- 10.0
  l <- 0.0
  u <- 20.0
  result <- deformula.moneone(f, l, u, a=a)
  expect_equal(result$value, expected(a, l, u))
})

