# Tests of sclr function
# Arseniy Khvorov
# Created 2019/09/02
# Last edit 2019/10/16

library(sclr)

test_that("Basic usage", {
  expect_true(is_sclr(sclr(status ~ logHI, data = one_titre_data)))
})

test_that("Error with missing parameters", {
    expect_error(sclr(status ~ logHI))
    expect_error(sclr(one_titre_data))
})

test_that("Warning with no covariates", {
  expect_warning(sclr(status ~ 1, one_titre_data))
})

test_that("Error with unexpected outcome", {
    # Factor
    dat <- one_titre_data
    dat$status <- as.factor(dat$status)
    expect_error(sclr(status ~ logHI, dat))
    # 1's and 2's
    dat$status <- as.numeric(dat$status)
    expect_error(sclr(status ~ logHI, dat))
})

test_that("Returns the expected parameter names", {
  fit_my_names <- sclr(status ~ logHI, one_titre_data)
  expect_named(fit_my_names$parameters, c("theta", "beta_0", "beta_logHI"))
  est_conv_names <- sclr(
    status ~ logHI, one_titre_data, conventional_names = TRUE
  )
  expect_named(
    est_conv_names$parameters, 
    c("(Baseline)", "(Intercept)", "logHI")
  )
})

test_that("Return is stable", {
  pars <- do.call(c, lapply(1:10, function(ind) {
    fit <- sclr(status ~ logHI, one_titre_data)
    par <- fit$parameters[["beta_logHI"]]
    return(par)
  }))
  expect_equal(length(unique(round(pars, 4))), 1)
})
