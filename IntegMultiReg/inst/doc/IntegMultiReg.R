## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.2
)
set.seed(1)

## ----load---------------------------------------------------------------------
library(IntegMultiReg)
data("simIMR", package = "IntegMultiReg")
sapply(simIMR$platforms, dim)

## ----load-kirc----------------------------------------------------------------
data("kircIMR", package = "IntegMultiReg")
sapply(kircIMR$platforms, dim)
kircIMR$model_subgroup_sizes

## ----kirc-fit, eval = FALSE---------------------------------------------------
# kirc_fit <- imr(
#   platform_data_list = kircIMR$platforms,
#   outcome            = kircIMR$outcome.survival,
#   cov                = kircIMR$covariates,
#   type_outcome       = "right.censored",
#   nu                 = c(-4, -3, -4),
#   sample_mcmc        = c(4000, 1000),
#   ssize              = 30,
#   seed               = 1
# )
# summary(kirc_fit)
# plot_top_features(kirc_fit, top = 12)

## ----fit----------------------------------------------------------------------
fit <- imr(
  platform_data_list = simIMR$platforms,
  outcome            = simIMR$outcome.binary,
  cov                = simIMR$covariates,
  type_outcome       = "binary",
  nu                 = c(-4, -3, -4),
  sample_mcmc        = c(1500, 500),
  ssize              = 30,
  seed               = 1
)
fit

## ----plot-sizes, fig.height = 3.6---------------------------------------------
plot_subgroup_sizes(fit)

## ----summary------------------------------------------------------------------
summary(fit, threshold = 0.5)

## ----plot-selection-----------------------------------------------------------
plot(fit, type = "selection", platform = 1)

## ----plot-top, fig.height = 4-------------------------------------------------
plot_top_features(fit, top = 8)

## ----plot-trace---------------------------------------------------------------
plot(fit, type = "trace")

## ----predict------------------------------------------------------------------
new_x <- simIMR$platforms$genomic[1:20, ]
new_p <- simIMR$platforms$proteomic[1:20, ]
pred <- predict(fit, newdata = list(new_x, new_p),
                covariates = simIMR$covariates)
head(pred[["model:011"]])

## ----cv-----------------------------------------------------------------------
cv <- cv_imr(fit, k = 5, rounds = 3)
attr(cv, "metric")
round(colMeans(cv$total_cindex), 3)

## ----other-types, eval = FALSE------------------------------------------------
# # continuous (Gaussian) outcome
# fit_c <- imr(
#   simIMR$platforms, simIMR$outcome.continuous, cov = simIMR$covariates,
#   type_outcome = "continuous", nu = c(-4, -3, -4),
#   sample_mcmc = c(1500, 500), ssize = 30, seed = 1)
# 
# # right-censored survival outcome
# fit_s <- imr(
#   simIMR$platforms, simIMR$outcome.survival, cov = simIMR$covariates,
#   type_outcome = "right.censored", nu = c(-4, -3, -4),
#   sample_mcmc = c(1500, 500), ssize = 30, seed = 1)

