test_that("lowerT", {
  # set some parameters
  low <- 100 # lower bound
  hat <- rep(150, 6) # estimates obtained from each model
  sigmasq <- 10 # variance
  Tstar <- matrix(rep(100,600),6,100) # T statistics estimated from bootstrap samples
  weights <- rep(1/6, 6) # model weights
  B <- 100 # number of bootstrapped samples
  alpha <- 0.05 # confidence level

  # calculate the upper limit of T statistics
  res <- marp::lowerT(low, hat, sigmasq, Tstar, weights, B, alpha)

  # check result
  expect_equal(res, 0.97499999999999976)
})
