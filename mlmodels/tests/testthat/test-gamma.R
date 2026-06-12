# tests/testthat/test-count.R
library(testthat)
library(marginaleffects)

data(mroz)

# ------------------------------------------------------------------------------
# Basic model fitting
# ------------------------------------------------------------------------------

test_that("ml_gamma fits homoskedastic and heteroskedastic models", {
  gam_hom <- ml_gamma(faminc ~ hours + hushrs + age + educ, data = mroz)
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  expect_s3_class(gam_hom, "ml_gamma")
  expect_s3_class(gam_het, "ml_gamma")
  expect_s3_class(gam_hom, "mlmodel")
  expect_s3_class(gam_het, "mlmodel")
})

test_that("ml_gamma and lognormal models can be compared", {
  ln_hom <- ml_lm(log(faminc) ~ hours + hushrs + age + educ, data = mroz)
  ln_het <- ml_lm(log(faminc) ~ hours + hushrs + age + educ,
                  scale = ~ kidslt6, data = mroz)
  gam_hom <- ml_gamma(faminc ~ hours + hushrs + age + educ, data = mroz)
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  expect_s3_class(ln_hom, "ml_lm")
  expect_s3_class(gam_hom, "ml_gamma")
})

# ------------------------------------------------------------------------------
# predict() tests
# ------------------------------------------------------------------------------

test_that("predict.ml_gamma returns expected structure", {
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  p_mean <- predict(gam_het, type = "response")
  p_var  <- predict(gam_het, type = "var")
  
  expect_type(p_mean$fit, "double")
  expect_type(p_var$fit,  "double")
  expect_true(all(p_mean$fit > 0))
  expect_true(all(p_var$fit > 0))
})

test_that("predict.ml_gamma works with marginaleffects", {
  skip_if_not_installed("marginaleffects")
  
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  expect_silent(predictions(gam_het, type = "response"))
  expect_silent(slopes(gam_het, variables = "educ"))
})

# ------------------------------------------------------------------------------
# Comparison with lognormal (for vignette foundation)
# ------------------------------------------------------------------------------

test_that("gamma and lognormal mean predictions are close", {
  ln_het <- ml_lm(log(faminc) ~ hours + hushrs + age + educ,
                  scale = ~ kidslt6, data = mroz)
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  pr_ln <- predict(ln_het, type = "response", se.fit = TRUE)
  pr_gam <- predict(gam_het, type = "response", se.fit = TRUE)
  
  # Means should be reasonably close
  expect_true(mean(abs(pr_ln$fit - pr_gam$fit)) < 1500,  # adjust tolerance as needed
              info = "Gamma and lognormal mean predictions should be reasonably close")
})

# ------------------------------------------------------------------------------
# Hypothesis testing
# ------------------------------------------------------------------------------

test_that("IMtest works on gamma models", {
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  im_opg <- IMtest(gam_het, method = "opg")
  expect_s3_class(im_opg, "IMtest.mlmodel")
})

test_that("Vuong test works between gamma and lognormal", {
  ln_het <- ml_lm(log(faminc) ~ hours + hushrs + age + educ,
                  scale = ~ kidslt6, data = mroz)
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  vt <- vuongtest(ln_het, gam_het)
  expect_s3_class(vt, "vuongtest.mlmodel")
})

# ------------------------------------------------------------------------------
# Constrained optimization
# ------------------------------------------------------------------------------

test_that("ml_gamma supports constraints on scale equation", {
  gam_het <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                      scale = ~ kidslt6, data = mroz)
  
  # Constraint: scale::kidslt6 = 0  (equivalent to homoskedastic gamma)
  gam_het_c <- ml_gamma(faminc ~ hours + hushrs + age + educ,
                        scale = ~ kidslt6,
                        constraints = "scale::kidslt6 = 0",
                        data = mroz,
                        start = c(8.3, 0, 0, 0, 0.01, -0.8, 0.1))
  
  expect_s3_class(gam_het_c, "ml_gamma")
  
  # Constrained coefficient should be very close to zero
  expect_true(abs(coef(gam_het_c)["scale::kidslt6"]) < 1e-4,
              info = "Constrained scale::kidslt6 should be near zero")
})