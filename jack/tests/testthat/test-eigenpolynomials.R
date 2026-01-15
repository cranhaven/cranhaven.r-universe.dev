test_that("Laplace-Beltrami", {
  n <- 4
  lambda <- c(2, 2)
  alpha <- gmp::as.bigq(3)
  J <- JackPol(n, lambda, alpha)
  Jlb <- LaplaceBeltrami(J, alpha)
  ev <- eigenvalueLB(n, lambda, alpha)
  expect_true(Jlb == ev * J)
})

test_that("Calogero-Sutherland", {
  n <- 4
  lambda <- c(3, 1)
  alpha <- gmp::as.bigq("3/4")
  J <- JackPol(n, lambda, alpha)
  Jcs <- CalogeroSutherland(J, alpha)
  ev <- eigenvalueCS(n, lambda, alpha)
  expect_true(Jcs == ev * J)
})
