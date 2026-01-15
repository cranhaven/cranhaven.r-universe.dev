test_that("evaluation", {
  library(gmp)
  f <- function(x, y, z) {
    (x + 2*y^2 - 3*z^3) / (3*x^3 - 2*y^2 + z + 5) + (x/y)^2 + z + 3
  }
  roq <- f(qlone(1), qlone(2), qlone(3))
  x <- as.bigq("2")
  y <- as.bigq("3/2")
  z <- as.bigq("4/3")
  expect_true(
    evalRatioOfQsprays(roq, c(x, y, z)) == f(x, y, z)
  )
  roqfun <- as.function(roq)
  expect_true(
    as.character(f(x, y, z)) == roqfun("2", "3/2", "4/3")
  )
})
