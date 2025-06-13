# Tests of simulations of ideal data
# Arseniy Khvorov
# Created 2019/10/21
# Last edit 2019/10/21

test_that("ideal data is simulated", {
  dat <- sclr_ideal_data(
    n = 1000, theta = 0, beta_0 = -5,
    covariate_list = list(
      "logHI" = list(gen_fun = function(n) rnorm(n, 2, 2), true_par = 2),
      "logNI" = list(gen_fun = function(n) rnorm(n, 2, 2), true_par = 1)
    ),
    outcome_name = "status",
    seed = 1,
    attach_true_vals = TRUE,
    attach_seed = TRUE
  )
  expect_named(dat, c("logHI", "logNI", "status"))
  expect_equal(attr(dat, "seed"), 1)
  expect_equal(attr(dat, "true_values")$true_value, c(0, -5, 2, 1))
  expect_named(sclr_ideal_data(outcome_name = "outcome"), c("logHI", "outcome"))
})

test_that("error with incorrect covariate_list", {
  covl1 <- list(
    "logHI" = list(gen_function = function(n) rnorm(n, 2, 2), true_par = 2)
  )
  covl2 <- list(
    "logHI" = list(gen_fun = function(n) rnorm(n, 2, 2), true_parameter = 2)
  )
  expect_error(sclr_ideal_data(covariate_list = covl1))
  expect_error(sclr_ideal_data(covariate_list = covl2))
})
