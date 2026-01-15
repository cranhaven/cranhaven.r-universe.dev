test_that("power sum polynomials are homogeneous", {
  lambda <- c(3, 2, 1)
  p <- PSFpoly(4, lambda)
  homogeneous <- isHomogeneousQspray(p)
  expect_true(homogeneous)
  expect_true(attr(homogeneous, "degree") == sum(lambda)) 
})