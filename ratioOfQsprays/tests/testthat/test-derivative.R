test_that("derivative", {
  f <- function(x, y, z) {
    (x + 2*y^2 - 3*z^3) / (3*x^3 - 2*y^2 + z + 5) + (x/y)^2 + z + 3
  }
  roq <- f(qlone(1), qlone(2), qlone(3))
  droq1 <- dRatioOfQsprays(roq, c(0, 1, 2))
  droq2 <- derivRatioOfQsprays(
    derivRatioOfQsprays(roq, i = 2),
    i = 3, derivative = 2
  )
  expect_true(droq1 == droq2)
})
