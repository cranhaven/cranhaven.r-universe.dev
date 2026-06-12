# tests/testthat/test-ml_lm.R
library(testthat)
library(marginaleffects)

mroz$incthou <- mroz$faminc / 1000

# ------------------------------------------------------------------------------
# Model fitting tests
# ------------------------------------------------------------------------------

test_that("ml_lm fits homoskedastic Gaussian model", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  expect_s3_class(fit, "ml_lm")
  expect_s3_class(fit, "mlmodel")
  expect_null(fit$model$scale_formula)
  expect_equal(fit$model$n_used, nrow(mroz))
})

test_that("ml_lm fits heteroskedastic Gaussian model", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
               scale = ~ educ + exper,
               data = mroz)
  
  expect_s3_class(fit, "ml_lm")
  expect_s3_class(fit, "mlmodel")
  expect_true(!is.null(fit$model$scale_formula))
})

test_that("ml_lm handles constrained optimization", {
  data(mroz)
  st0 <- c(-29, 1.26, -0.0136, 1.96, 1, -0.25, 2)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
               data = mroz,
               constraints = "value::educ = 1",
               start = st0)
  
  expect_s3_class(fit, "ml_lm")
  # Check that constraint was applied (coefficient of educ should be close to 1)
  coefs <- coef(fit)
  # Use absolute tolerance for a constrained value that should be exactly 1
  expect_true(abs(coefs["value::educ"] - 1) < 0.001,
              info = "Constrained coefficient for educ should be very close to 1")
})

test_that("ml_lm respects subset argument", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
               subset = hours > 0,
               data = mroz)
  
  expect_true(fit$model$n_used < nrow(mroz))
})

# ------------------------------------------------------------------------------
# predict() tests - Updated for list return
# ------------------------------------------------------------------------------

test_that("predict.ml_lm returns list with $fit component", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  pred <- predict(fit)
  
  expect_type(pred, "list")
  expect_true("fit" %in% names(pred))
  expect_type(pred$fit, "double")
  expect_length(pred$fit, nrow(mroz))
})

test_that("predict.ml_lm supports different types", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  p_resp <- predict(fit, type = "response")
  p_link <- predict(fit, type = "link")
  p_mu   <- predict(fit, type = "mu")
  p_var  <- predict(fit, type = "variance")
  
  expect_type(p_resp$fit, "double")
  expect_type(p_link$fit, "double")
  expect_type(p_mu$fit,   "double")
  expect_type(p_var$fit,  "double")
})

test_that("predict.ml_lm works with newdata", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  newd <- head(mroz, 10)
  
  pred <- predict(fit, newdata = newd)
  expect_length(pred$fit, 10)
})

test_that("predict.ml_lm handles log(y) models with retransformation", {
  data(mroz)
  fit <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  pred <- predict(fit, type = "response")   # should be retransformation
  expect_true(all(pred$fit > 0))            # incomes should be positive
})

test_that("predict.ml_lm works on heteroskedastic models", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
               scale = ~ educ + exper,
               data = mroz)
  
  pred_var <- predict(fit, type = "variance")
  expect_type(pred_var$fit, "double")
  expect_true(all(pred_var$fit > 0))
})

# ------------------------------------------------------------------------------
# Post-estimation tests
# ------------------------------------------------------------------------------

test_that("summary.ml_lm works with different vcov types", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  s_robust <- summary(fit, vcov.type = "robust")
  s_oim    <- summary(fit, vcov.type = "oim")
  
  expect_s3_class(s_robust, "summary.mlmodel")
  expect_s3_class(s_oim,    "summary.mlmodel")
})

test_that("waldtest works on ml_lm", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  wt <- waldtest(fit, indices = 5, rhs = 1)   # educ coefficient = 1
  expect_s3_class(wt, "waldtest.mlmodel")
})

test_that("IMtest works on ml_lm", {
  data(mroz)
  fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  im_opg <- IMtest(fit, method = "opg")
  expect_s3_class(im_opg, "IMtest.mlmodel")
})

test_that("vuongtest works between linear and loglinear models", {
  data(mroz)
  lin  <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  logl <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem, data = mroz)
  
  vt <- vuongtest(logl, lin)
  expect_s3_class(vt, "vuongtest.mlmodel")
})

# ------------------------------------------------------------------------------
# Marginaleffects compatibility
# ------------------------------------------------------------------------------

test_that("ml_lm works with marginaleffects", {
  skip_if_not_installed("marginaleffects")
  fit <- ml_lm(incthou ~ age + educ, data = mroz)
  
  expect_silent({
    preds <- predictions(fit)
    slopes <- slopes(fit)
  })
  
  expect_s3_class(preds, "predictions")
  expect_s3_class(slopes, "slopes")
})