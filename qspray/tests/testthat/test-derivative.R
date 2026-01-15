test_that("derivative", {
  x <- qlone(1)
  y <- qlone(2)
  P <- 2*x + 3*x*y^4
  expect_true(derivQspray(P, 2, 2) == 36*x*y^2)
})

test_that("dQspray", {
  x <- qlone(1)
  y <- qlone(2)
  P <- 2*x + 3*x*y^4
  expect_true(dQspray(P, c(1, 1)) == 12*y^3)
})