# Tests of the likelihood-related functions
# Arseniy Khvorov
# Created 2019/10/17
# Last edit 2019/10/17

library(sclr)

test_that("Likelihood works", {
  fit <- sclr(status ~ logHI, one_titre_data, seed = 20191102)
  expect_true(is.numeric(sclr_log_likelihood(fit)))
  sclr_log_likelihood(fit, pars = c("lambda" = 0.5, "beta_0" = 1, "beta_1" = 1))
  expect_true(is.numeric(sclr_log_likelihood(
    fit, pars = c("lambda" = 0.5, "beta_0" = 1, "beta_1" = 1)
  )))
  expect_error(
    sclr_log_likelihood(),
    "likelihood requires fit or x, y and pars"
  )
  expect_error(
    sclr_log_likelihood(list(x = fit$x, y = fit$y), pars = coef(fit)),
    "fit must be of type sclr"
  )
  empty_fit <- suppressWarnings(sclr(
      status ~ logHI, one_titre_data, 
      algorithm = "newton-raphson", nr_iter = 3
  ))
  expect_null(sclr_log_likelihood(empty_fit))
})
