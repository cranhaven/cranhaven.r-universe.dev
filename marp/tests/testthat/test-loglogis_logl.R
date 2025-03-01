test_that("loglogis_logl", {
  # set some parameters
  par_hat <- c(2.6037079185931518, 247.59811806509711) # estimated parameters
  param <-  c(log(par_hat[2]),log(par_hat[1])) # input parameters for logl function

  # fix the random seed
  set.seed(42)

  # sample data for testing
  x <- rgamma(30, 3, 0.01)

  result <- marp::loglogis_logl(param, x)
  expected_result <- 195.12976531752238
  expect_true(all.equal(result, expected_result))
})
