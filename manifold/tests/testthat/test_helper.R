# devtools::load_all()
library(testthat)



test_that('MakeRotMat works', {
  x1 <- c(1, 0, 0)
  x2 <- c(-1, 0, 0)
  x3 <- c(-0.99, sqrt(1 - 0.99^2), 0)

  expect_warning(MakeRotMat(x1, x2), 'Rotation between podal points is arbitrary')

  R2 <- MakeRotMat(x1, x3)

})
