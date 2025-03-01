test_that("bpt_logl", {
  # set some parameters
  par_hat <- c(292.945125794581, 0.718247184450307) # estimated parameters
  param <-  c(log(par_hat[1]),log(par_hat[2]^2)) # input parameters for logl function

  # fix the random seed
  set.seed(42)

  # sample data for testing
  x <- rgamma(30, 3, 0.01)

  result <- marp::bpt_logl(param, x)
  expected_result <- 194.40996001614093
  expect_true(all.equal(result, expected_result))
})
