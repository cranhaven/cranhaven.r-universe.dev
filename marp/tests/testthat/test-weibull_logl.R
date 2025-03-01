test_that("weibull_logl", {
  # set some parameters
  par_hat <- c(330.801103808081, 1.80101338777944) # estimated parameters
  param <- log(par_hat) # input parameters for logl function

  # fix the random seed
  set.seed(42)

  # sample data for testing
  x <- rgamma(30, 3, 0.01)

  result <- marp::weibull_logl(param, x)
  expected_result <- 193.92377676667274
  expect_true(all.equal(result, expected_result))
})
