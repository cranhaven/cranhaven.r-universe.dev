# tests/testthat/test-binary.R
library(testthat)
library(marginaleffects)

data(smoke)
data(pw401k)

smoke$smokes <- smoke$cigs > 0
val_for <- smokes ~ cigpric + income + age
scal_for <- ~ educ

# ------------------------------------------------------------------------------
# Basic model fitting
# ------------------------------------------------------------------------------

test_that("ml_logit and ml_probit fit homoskedastic and heteroskedastic models", {
  log_hom <- ml_logit(val_for, data = smoke)
  log_het <- ml_logit(val_for, scale = scal_for, data = smoke)
  prob_hom <- ml_probit(val_for, data = smoke)
  prob_het <- ml_probit(val_for, scale = scal_for, data = smoke)
  
  expect_s3_class(log_hom, "ml_logit")
  expect_s3_class(log_het, "ml_logit")
  expect_s3_class(prob_hom, "ml_probit")
  expect_s3_class(prob_het, "ml_probit")
  expect_s3_class(log_hom, "mlmodel")
})

test_that("ml_logit accepts fractional responses (pw401k)", {
  fit_frac <- ml_logit(prate ~ mrate + I(mrate^2) + log(totemp) + 
                         I(log(totemp)^2) + age + I(age^2) + sole, 
                       data = pw401k)
  
  expect_s3_class(fit_frac, "ml_logit")
  expect_s3_class(fit_frac, "mlmodel")
})

test_that("ml_probit accepts fractional responses", {
  fit_frac <- ml_probit(prate ~ mrate + I(mrate^2) + log(totemp) + 
                          I(log(totemp)^2) + age + I(age^2) + sole, 
                        data = pw401k)
  
  expect_s3_class(fit_frac, "ml_probit")
  expect_s3_class(fit_frac, "mlmodel")
})

# ------------------------------------------------------------------------------
# predict() tests
# ------------------------------------------------------------------------------

test_that("predict() works for logit and probit", {
  fit_logit <- ml_logit(val_for, data = smoke)
  fit_probit <- ml_probit(val_for, data = smoke)
  
  p_logit <- predict(fit_logit)
  p_probit <- predict(fit_probit)
  
  expect_type(p_logit$fit, "double")
  expect_type(p_probit$fit, "double")
  expect_true(all(p_logit$fit >= 0 & p_logit$fit <= 1))
  expect_true(all(p_probit$fit >= 0 & p_probit$fit <= 1))
})

# ------------------------------------------------------------------------------
# Marginaleffects compatibility
# ------------------------------------------------------------------------------

test_that("ml_logit and ml_probit work with marginaleffects", {
  skip_if_not_installed("marginaleffects")
  
  fit_logit <- ml_logit(val_for, data = smoke)
  fit_probit <- ml_probit(val_for, data = smoke)
  
  expect_silent(predictions(fit_logit))
  expect_silent(predictions(fit_probit))
  expect_silent(slopes(fit_logit, variables = "age"))
  expect_silent(slopes(fit_probit, variables = "age"))
})

# ------------------------------------------------------------------------------
# Hypothesis testing
# ------------------------------------------------------------------------------

test_that("IMtest works on logit and probit", {
  fit_logit <- ml_logit(val_for, data = smoke)
  fit_probit <- ml_probit(val_for, data = smoke)
  
  expect_s3_class(IMtest(fit_logit), "IMtest.mlmodel")
  expect_s3_class(IMtest(fit_probit), "IMtest.mlmodel")
})

test_that("Wald test works", {
  fit_logit <- ml_logit(val_for, data = smoke)
  wt <- waldtest(fit_logit, indices = 2, vcov.type = "robust")
  expect_s3_class(wt, "waldtest.mlmodel")
})

test_that("Vuong test works between logit and probit", {
  fit_logit <- ml_logit(val_for, data = smoke)
  fit_probit <- ml_probit(val_for, data = smoke)
  
  vt <- vuongtest(fit_logit, fit_probit)
  expect_s3_class(vt, "vuongtest.mlmodel")
})

# ------------------------------------------------------------------------------
# Constrained optimization
# ------------------------------------------------------------------------------

test_that("ml_logit and ml_probit support constraints", {
  fit_logit  <- ml_logit(val_for, data = smoke)
  fit_probit <- ml_probit(val_for, data = smoke)
  
  fit_logit_c <- ml_logit(val_for,
                          constraints = "value::cigpric = 0",
                          data = smoke,
                          start = c(0.7, 0, 0, -0.012))
  
  fit_probit_c <- ml_probit(val_for,
                            constraints = "value::cigpric = 0",
                            data = smoke,
                            start = c(0.4, 0, 0, -0.008))
  
  expect_s3_class(fit_logit_c, "ml_logit")
  expect_s3_class(fit_probit_c, "ml_probit")
  
  # Check constrained coefficient is near zero
  expect_true(abs(coef(fit_logit_c)["value::cigpric"]) < 1e-4)
  expect_true(abs(coef(fit_probit_c)["value::cigpric"]) < 1e-4)
})

test_that("ml_logit supports constraints on scale equation (heteroskedastic)", {
  data(smoke)
  
  fit_het <- ml_logit(smokes ~ cigpric + income + age, 
                      scale = ~ educ, 
                      data = smoke)
  
  fit_het_c <- ml_logit(smokes ~ cigpric + income + age, 
                        scale = ~ educ,
                        constraints = "scale::educ = 0",
                        data = smoke,
                        start = c(-0.006, 8.5e-3, 0, -4e-4, 0))
  
  expect_s3_class(fit_het_c, "ml_logit")
  
  expect_true(abs(coef(fit_het_c)["scale::educ"] - 0) < 1e-4,
              info = "Constrained scale coefficient 'scale::educ' should be near zero")
})