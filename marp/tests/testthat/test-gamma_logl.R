test_that("gamma_logl", {
  # set some parameters
  par_hat <- c(2.7626793657057762, 0.0094307059277139432) # estimated parameters
  param <- log(par_hat) # input parameters for logl function

  # fix the random seed
  set.seed(42)

  # sample data for testing
  x <- rgamma(30, 3, 0.01)

  result <- marp::gamma_logl (param, x)
  expected_result <- 193.76650950646049
  expect_true(all.equal(result, expected_result))
})
