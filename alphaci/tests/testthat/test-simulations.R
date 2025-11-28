test_that("simulate_congeneric has the correct dimension", {
  lambda <- runif(5)
  sigma <- runif(3)
  x <- simulate_congeneric(10, k = 7, lambda = lambda, sigma = sigma)
  expect_equal(dim(x), c(10, 7))
})
