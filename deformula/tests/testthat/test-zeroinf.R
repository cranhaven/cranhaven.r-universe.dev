test_that("Integral of exp(-a*x) on [0,Inf)", {
  f <- function(x, a) exp(-a*x)
  a <- 0.1
  result <- deformula.zeroinf(f, a=a)
  expect_equal(result$value, 1/a)
  a <- 0.2
  result <- deformula.zeroinf(f, a=a)
  expect_equal(result$value, 1/a)
})

