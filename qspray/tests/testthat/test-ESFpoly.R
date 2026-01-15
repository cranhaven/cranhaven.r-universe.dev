test_that("ESFpoly", {
  P <- ESFpoly(2, c(2,1))
  x <- qlone(1)
  y <- qlone(2)
  expect_true(P == x^2*y + x*y^2)
})