# Tests of methods for the sclr class
# Arseniy Khvorov
# Created 2019/10/23
# Last edit 2019/10/23

library(sclr)

fit <- sclr(status ~ logHI, one_titre_data)

test_that("printing works", {
  expect_output(print(fit))
  expect_output(summary(fit))
})

test_that("access works", {
  expect_equal(coef(fit), fit$parameters)
  expect_equal(vcov(fit), fit$covariance_mat)
  expect_equal(confint(fit), confint.default(fit))
  empty_fit <- suppressWarnings(sclr(
    status ~ logHI, one_titre_data, 
    algorithm = "newton-raphson", nr_iter = 3
  ))
  expect_null(confint(empty_fit))
  expect_equal(model.matrix(fit), fit$x)
  expect_equal(model.frame(fit), fit$model)
  ll <- logLik(fit)
  expect_equal(class(ll), "logLik")
  expect_equal(attr(ll, "nobs"), nrow(one_titre_data))
  expect_equal(attr(ll, "df"), 3)
  ll2 <- logLik(sclr(status ~ logHI + logNI, two_titre_data))
  expect_equal(attr(ll2, "df"), 4)
  ll3 <- suppressWarnings(logLik(sclr(status ~ 1, two_titre_data)))
  expect_equal(attr(ll3, "df"), 2)
})

test_that("tidying works", {
  fit_tidy <- tidy(fit)
  expect_named(
    fit_tidy, c("term", "estimate", "std_error", "conf_low", "conf_high")
  )
  expect_true(all(!is.na(fit_tidy)))
})

test_that("prediction works", {
  prediction <- predict(fit, tibble(logHI = c(0, 3, 5)))
  pars <- coef(fit)
  
  # Point estimate
  expect_equal(prediction$prot_point_lin[[1]], pars[["beta_0"]])
  expect_equal(
    prediction$prot_point_lin[[2]], pars[["beta_0"]] + 3 * pars[["beta_logHI"]]
  )
  expect_equal(
    prediction$prot_point_lin[[3]], pars[["beta_0"]] + 5 * pars[["beta_logHI"]]
  )
  
  # Bounds
  covmat <- vcov(fit)[-1, -1]
  covmat2 <- matrix(c(1, 3, 3, 9), ncol = 2) * covmat
  covmat3 <- matrix(c(1, 5, 5, 25), ncol = 2) * covmat
  lvl <- qnorm((1 + 0.95) / 2)
  expect_equal(
    prediction$prot_l_lin[[1]], pars[["beta_0"]] - lvl * sqrt(covmat[1, 1])
  )
  expect_equal(
    prediction$prot_u_lin[[1]], pars[["beta_0"]] + lvl * sqrt(covmat[1, 1])
  )
  expect_equal(
    prediction$prot_l_lin[[2]], 
    pars[["beta_0"]] + 3 * pars[["beta_logHI"]] - lvl * sqrt(sum(covmat2))
  )
  expect_equal(
    prediction$prot_u_lin[[2]], 
    pars[["beta_0"]] + 3 * pars[["beta_logHI"]] + lvl * sqrt(sum(covmat2))
  )
  expect_equal(
    prediction$prot_l_lin[[3]], 
    pars[["beta_0"]] + 5 * pars[["beta_logHI"]] - lvl * sqrt(sum(covmat3))
  )
  expect_equal(
    prediction$prot_u_lin[[3]], 
    pars[["beta_0"]] + 5 * pars[["beta_logHI"]] + lvl * sqrt(sum(covmat3))
  )
})

test_that("predict method throws an error when used incorrectly", {
  fit2 <- sclr(status ~ logHI + logNI, two_titre_data)
  expect_error(predict(fit2, tibble(logHI = c(0, 1)))) # Not enough covariates
})
