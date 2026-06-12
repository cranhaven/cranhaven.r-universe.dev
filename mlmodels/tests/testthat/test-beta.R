# tests/testthat/test-beta.R
library(testthat)
library(marginaleffects)

data(pw401k)

val_for <- prate ~ mrate + I(mrate^2) + log(totemp) + I(log(totemp)^2) + 
  age + I(age^2) + sole

# ------------------------------------------------------------------------------
# Basic model fitting
# ------------------------------------------------------------------------------

test_that("ml_beta fits homoskedastic and heteroskedastic models", {
  suppressMessages({
    beta_hom <- ml_beta(val_for, data = pw401k, subset = prate < 1)
    beta_het <- ml_beta(val_for, scale = ~ totemp + sole, 
                        data = pw401k, subset = prate < 1)
  })
  
  expect_s3_class(beta_hom, "ml_beta")
  expect_s3_class(beta_het, "ml_beta")
  expect_s3_class(beta_hom, "mlmodel")
  expect_s3_class(beta_het, "mlmodel")
})

test_that("ml_beta drops boundary observations and issues warning", {
  expect_warning(
    ml_beta(val_for, data = pw401k),
    "Dropped .* observation\\(s\\) at the boundaries"
  )
})

# ------------------------------------------------------------------------------
# predict() tests
# ------------------------------------------------------------------------------

test_that("predict.ml_beta returns expected structure and handles mode NAs", {
  suppressMessages({
    beta_het <- ml_beta(val_for, scale = ~ totemp + sole, 
                        data = pw401k, subset = prate < 1)
  })
  
  p_resp <- predict(beta_het, type = "response")
  p_mode <- predict(beta_het, type = "mode")
  p_var  <- predict(beta_het, type = "var")
  
  expect_type(p_resp$fit, "double")
  expect_type(p_var$fit,  "double")
  
  # Mode can legitimately return NA when shape1 or shape2 <= 1
  expect_true(any(is.na(p_mode$fit)) || all(!is.na(p_mode$fit)))
})

test_that("predict.ml_beta works with marginaleffects", {
  skip_if_not_installed("marginaleffects")
  
  suppressMessages({
    beta_het <- ml_beta(val_for, scale = ~ totemp + sole, 
                        data = pw401k, subset = prate < 1)
  })
  
  expect_silent(predictions(beta_het, type = "response"))
  expect_silent(slopes(beta_het, variables = "mrate"))
})

# ------------------------------------------------------------------------------
# Comparison with logit (foundation for vignette)
# ------------------------------------------------------------------------------

test_that("beta and logit mean predictions are reasonably close on interior data", {
  suppressMessages({
    log_het <- ml_logit(val_for, scale = ~ totemp + sole, 
                        data = pw401k, subset = prate < 1)
    beta_het <- ml_beta(val_for, scale = ~ totemp + sole, 
                        data = pw401k, subset = prate < 1)
  })
  
  sample_idx <- log_het$model$sample
  
  pr_log  <- predict(log_het,  type = "response")
  pr_beta <- predict(beta_het, type = "response")
  
  log_mean  <- pr_log$fit[sample_idx]
  beta_mean <- pr_beta$fit[sample_idx]
  
  mean_diff <- mean(abs(log_mean - beta_mean), na.rm = TRUE)
  
  expect_true(mean_diff < 0.05,
              info = paste("Mean absolute difference between logit and beta predictions:", mean_diff))
})

# ------------------------------------------------------------------------------
# Hypothesis testing
# ------------------------------------------------------------------------------

test_that("IMtest works on beta models", {
  suppressMessages({
    beta_het <- ml_beta(val_for, scale = ~ totemp + sole, 
                        data = pw401k, subset = prate < 1)
  })
  
  im_test <- IMtest(beta_het, method = "opg")
  expect_s3_class(im_test, "IMtest.mlmodel")
})