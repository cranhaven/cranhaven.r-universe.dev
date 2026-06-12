# tests/testthat/test-count.R
library(testthat)
library(marginaleffects)

data(docvis)
data(pw401k)  # for possible future extensions

formula <- docvis ~ private + medicaid + age + I(age^2) + educyr + actlim + totchr
scal_for <- ~ female + bh

# ------------------------------------------------------------------------------
# Basic model fitting
# ------------------------------------------------------------------------------

test_that("ml_poisson and ml_negbin fit homoskedastic and heteroskedastic models", {
  fit_pois <- ml_poisson(formula, data = docvis)
  fit_nb1  <- ml_negbin(formula, dispersion = "NB1", data = docvis)
  fit_nb2  <- ml_negbin(formula, data = docvis)
  
  fit_nb1_het <- ml_negbin(formula, scale = scal_for, dispersion = "NB1", data = docvis)
  fit_nb2_het <- ml_negbin(formula, scale = scal_for, data = docvis)
  
  expect_s3_class(fit_pois, "ml_poisson")
  expect_s3_class(fit_nb1,  "ml_negbin")
  expect_s3_class(fit_nb2,  "ml_negbin")
  expect_s3_class(fit_nb1_het, "ml_negbin")
  expect_s3_class(fit_nb2_het, "ml_negbin")
  expect_s3_class(fit_pois, "mlmodel")
})

# ------------------------------------------------------------------------------
# predict() tests
# ------------------------------------------------------------------------------

test_that("predict() works for Poisson and Negative Binomial models", {
  fit_pois <- ml_poisson(formula, data = docvis)
  fit_nb2  <- ml_negbin(formula, data = docvis)
  
  types_to_test <- c("response", "link", "P(3)", "P(1,4)", "P(,4)", "P(1,)")
  
  for (typ in types_to_test) {
    p_pois <- predict(fit_pois, type = typ)
    p_nb   <- predict(fit_nb2,  type = typ)
    
    expect_type(p_pois$fit, "double")
    expect_type(p_nb$fit,   "double")
    expect_length(p_pois$fit, nrow(docvis))
    expect_length(p_nb$fit,   nrow(docvis))
  }
})

# ------------------------------------------------------------------------------
# Marginaleffects compatibility
# ------------------------------------------------------------------------------

test_that("Count models work with marginaleffects", {
  skip_if_not_installed("marginaleffects")
  
  fit_pois <- ml_poisson(formula, data = docvis)
  fit_nb2  <- ml_negbin(formula, data = docvis)
  
  expect_silent(predictions(fit_pois))
  expect_silent(predictions(fit_nb2))
  
  expect_silent(slopes(fit_pois, variables = "educyr"))
  expect_silent(slopes(fit_nb2,  variables = "educyr"))
})

# ------------------------------------------------------------------------------
# Hypothesis testing
# ------------------------------------------------------------------------------

test_that("IMtest and GOF tests work on count models", {
  fit_pois <- ml_poisson(formula, data = docvis)
  fit_nb2  <- ml_negbin(formula, data = docvis)
  
  expect_s3_class(IMtest(fit_pois), "IMtest.mlmodel")
  expect_s3_class(IMtest(fit_nb2),  "IMtest.mlmodel")
  
  expect_s3_class(GOFtest(fit_nb2), "GOFtest.mlmodel")
})

test_that("Wald test works on count models", {
  fit_nb2 <- ml_negbin(formula, data = docvis)
  wt <- waldtest(fit_nb2, indices = 2, vcov.type = "robust")
  expect_s3_class(wt, "waldtest.mlmodel")
})

test_that("Vuong test works between Poisson and NB2", {
  fit_pois <- ml_poisson(formula, data = docvis)
  fit_nb2  <- ml_negbin(formula, data = docvis)
  
  vt <- vuongtest(fit_pois, fit_nb2)
  expect_s3_class(vt, "vuongtest.mlmodel")
})

# ------------------------------------------------------------------------------
# Constrained optimization
# ------------------------------------------------------------------------------

test_that("Count models support constraints in mean equation", {
  fit_nb2 <- ml_negbin(formula, data = docvis)
  
  fit_nb2_c <- ml_negbin(formula,
                         constraints = "value::medicaid = 0",
                         data = docvis,
                         start = c(-10.3, 0.16, 0.1, 0.29, 0, 0.03, 0.19, 0.28, -0.44))
  
  expect_s3_class(fit_nb2_c, "ml_negbin")
  expect_true(abs(coef(fit_nb2_c)["value::medicaid"]) < 1e-4,
              info = "Constrained medicaid coefficient should be near zero")
})

test_that("Count models support constraints in scale equation", {
  fit_nb2_het <- ml_negbin(formula, scale = scal_for, data = docvis)
  
  fit_nb2_het_c <- ml_negbin(formula,
                             scale = scal_for,
                             constraints = "scale::female = 0",
                             data = docvis,
                             start = c(-9.4, 0.15, 0, 0.28, -0.002, 0.02, 0.13, 0.25, 1.5, -0.2, 0.2))
  
  expect_s3_class(fit_nb2_het_c, "ml_negbin")
  expect_true(abs(coef(fit_nb2_het_c)["scale::female"]) < 1e-4,
              info = "Constrained scale::female should be near zero")
})

# Stress test - joint constraint (may take longer)
test_that("Count models can handle joint constraints (stress test)", {
  skip_on_cran()   # skip on CRAN if slow
  
  fit_nb2_het <- ml_negbin(formula, scale = scal_for, data = docvis)
  
  fit_nb2_het_joint <- ml_negbin(formula,
                                 scale = scal_for,
                                 constraints = c("value::medicaid = 0", "scale::female = 0"),
                                 data = docvis,
                                 start = c(-9.4, 0.15, 0, 0.28, -0.002, 0.02, 0.13, 0.25, 1.5, 0, 0.2))
  
  expect_s3_class(fit_nb2_het_joint, "ml_negbin")
})