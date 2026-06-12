## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mlmodels)
library(marginaleffects)

## -----------------------------------------------------------------------------
# Load example data
data(mroz)
mroz$incthou <- mroz$faminc / 1000

# Fit a homoskedastic linear model
fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, 
             data = mroz)

# View the results with robust standard errors
summary(fit, vcov.type = "robust")

## -----------------------------------------------------------------------------
# Basic prediction (expected value)
head(predict(fit, type = "response")$fit)

# Variance of the outcome
head(predict(fit, type = "variance")$fit)

# Prediction with standard errors using robust variance
pred <- predict(fit, type = "response", vcov.type = "robust", se.fit = TRUE)
head(pred$fit)      # Fitted values / expected values
head(pred$se.fit)   # Standard errors

## -----------------------------------------------------------------------------
# Heteroskedastic linear model
fit_het <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
                 scale = ~ educ + exper, 
                 data = mroz)

summary(fit_het, vcov.type = "robust")

## -----------------------------------------------------------------------------
  head(predict(fit_het, type = "variance")$fit)

## ----meffs-sigma--------------------------------------------------------------
# AMEs for the standard deviation using robust standard errors
avg_slopes(fit_het, variables = c("educ", "exper"), type = "sigma", vcov = "robust")

## ----intercepts---------------------------------------------------------------
# No intercept in the value function (wrong way)
fit_no_int <- tryCatch({
  ml_lm(incthou ~ 0 + age + I(age^2) + huswage + educ + unem,
                 scale = ~ educ + exper, 
                 data = mroz)
}, error = function(e) {
  print(e)
  invisible(NULL)
})

# No intercept in the value function (right way)
fit_no_int <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem,
                 scale = ~ educ + exper, 
                 noint_value = TRUE,
                 data = mroz)
summary(fit_no_int)

## -----------------------------------------------------------------------------
# Homoskedastic logit model
fit_logit <- ml_logit(inlf ~ age + I(age^2) + huswage + educ + unem, 
                      data = mroz)
summary(fit_logit, vcov.type = "robust")

# Heteroskedastic logit model
fit_logit_het <- ml_logit(inlf ~ age + I(age^2) + huswage + educ + unem,
                          scale = ~ educ + exper,
                          data = mroz)

summary(fit_logit_het, vcov.type = "robust")

