## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE
)
library(nestedcv)
library(pROC)

## ----eval = FALSE-------------------------------------------------------------
# library(hsstan)
# 
# # number of cores for parallelising hsstan sampling
# # store original options in oldopt
# # at the end reset options to old configuration
# oldopt <- options(mc.cores = 2)
# 
# # load iris dataset and simulate a continuous outcome
# data(iris)
# dt <- iris[, 1:4]
# colnames(dt) <- c("marker1", "marker2", "marker3", "marker4")
# dt <- as.data.frame(apply(dt, 2, scale))
# dt$outcome.cont <- -3 + 0.5 * dt$marker1 + 2 * dt$marker2 + rnorm(nrow(dt), 0, 2)
# 
# # unpenalised covariates: always retain in the prediction model
# uvars <- "marker1"
# # penalised covariates: coefficients are drawn from hierarchical shrinkage prior
# pvars <- c("marker2", "marker3", "marker4") # penalised covariates
# # run cross-validation with univariate filter and hsstan
# res.cv.hsstan <- outercv(y = dt$outcome.cont, x = dt[, c(uvars, pvars)],
#                          model = "model.hsstan",
#                          filterFUN = lm_filter,
#                          filter_options = list(force_vars = uvars,
#                                                nfilter = 2,
#                                                p_cutoff = NULL,
#                                                rsq_cutoff = 0.9),
#                          chains = 2,  # chains parallelised via options(mc.cores)
#                          n_outer_folds = 3, cv.cores = 1,  # CV folds not parallelised
#                          unpenalized = uvars, warmup = 100, iter = 200)

## ----eval = FALSE-------------------------------------------------------------
# summary(res.cv.hsstan)
# #> Single cross-validation to measure performance
# #> Outer loop:  3-fold CV
# #> No inner loop
# #> 150 observations, 4 predictors
# #>
# #> Model:  model.hsstan
# #> Filter:  lm_filter
# #>         n.filter
# #> Fold 1         3
# #> Fold 2         3
# #> Fold 3         3
# #>
# #> Final fit:             mean   sd  2.5% 97.5% n_eff Rhat
# #> (Intercept) -3.17 0.14 -3.40 -2.89   221 0.99
# #> marker1      0.37 0.32 -0.36  0.90   209 1.00
# #> marker2      2.07 0.22  1.70  2.47   196 1.00
# #> marker3      0.11 0.31 -0.43  0.92   104 1.00
# #>
# #> Result:
# #>     RMSE   Rsquared        MAE
# #>   2.1226     0.4772     1.6971
# 
# sampler.stats(res.cv.hsstan$final_fit)
# #>         accept.stat stepsize divergences treedepth gradients warmup sample
# #> chain:1      0.9843   0.0241           0         8     14844   0.25   0.12
# #> chain:2      0.9722   0.0316           0         8     11356   0.12   0.09
# #> all          0.9783   0.0279           0         8     26200   0.37   0.21
# print(projsel(res.cv.hsstan$final_fit), digits = 4)  # adding marker2
# #>                                                      Model        KL        ELPD
# #>                                             Intercept only   0.32508  -374.99055
# #>                                           Initial submodel   0.32031  -374.72416
# #>                                                    marker2   0.00151  -325.82289
# #>                                                    marker3   0.00000  -325.88824
# #>                var       kl rel.kl.null rel.kl   elpd delta.elpd
# #> 1   Intercept only 0.325077     0.00000     NA -375.0  -49.10230
# #> 2 Initial submodel 0.320306     0.01468 0.0000 -374.7  -48.83591
# #> 3          marker2 0.001506     0.99537 0.9953 -325.8    0.06535
# #> 4          marker3 0.000000     1.00000 1.0000 -325.9    0.00000

## ----eval = FALSE-------------------------------------------------------------
# # sigmoid function
# sigmoid <- function(x) {1 / (1 + exp(-x))}
# 
# # load iris dataset and create a binary outcome
# set.seed(267)
# data(iris)
# dt <- iris[, 1:4]
# colnames(dt) <- c("marker1", "marker2", "marker3", "marker4")
# dt <- as.data.frame(apply(dt, 2, scale))
# rownames(dt) <- paste0("sample", c(1:nrow(dt)))
# dt$outcome.bin <- sigmoid(0.5 * dt$marker1 + 2 * dt$marker2) > runif(nrow(dt))
# dt$outcome.bin <- factor(dt$outcome.bin)
# 
# # unpenalised covariates: always retain in the prediction model
# uvars <- "marker1"
# # penalised covariates: coefficients are drawn from hierarchical shrinkage prior
# pvars <- c("marker2", "marker3", "marker4") # penalised covariates
# # run cross-validation with univariate filter and hsstan
# res.cv.hsstan <- outercv(y = dt$outcome.bin,
#                          x = as.matrix(dt[, c(uvars, pvars)]),
#                          model = "model.hsstan",
#                          filterFUN = ttest_filter,
#                          filter_options = list(force_vars = uvars,
#                                                nfilter = 2,
#                                                p_cutoff = NULL,
#                                                rsq_cutoff = 0.9),
#                          chains = 2,  # parallelise over chains
#                          n_outer_folds = 3, cv.cores = 1,
#                          unpenalized = uvars, warmup = 100, iter = 200)

## ----eval = FALSE-------------------------------------------------------------
# summary(res.cv.hsstan)
# #> Single cross-validation to measure performance
# #> Outer loop:  3-fold CV
# #> No inner loop
# #> 150 observations, 4 predictors
# #> FALSE  TRUE
# #>    78    72
# #>
# #> Model:  model.hsstan
# #> Filter:  ttest_filter
# #>         n.filter
# #> Fold 1         3
# #> Fold 2         3
# #> Fold 3         3
# #>
# #> Final fit:             mean   sd  2.5% 97.5% n_eff Rhat
# #> (Intercept) -0.12 0.23 -0.56  0.27   200    1
# #> marker1      0.50 0.30 -0.10  1.16   208    1
# #> marker2      1.91 0.36  1.26  2.66   263    1
# #> marker3      0.01 0.24 -0.55  0.69   171    1
# #>
# #> Result:
# #>          Reference
# #> Predicted FALSE TRUE
# #>     FALSE    56   23
# #>     TRUE     22   49
# #>
# #>               AUC            Accuracy   Balanced accuracy
# #>            0.8284              0.7000              0.6993
# 
# # examine the Bayesian model
# print(projsel(res.cv.hsstan$final_fit), digits = 4)  # adding marker2
# #>                                                      Model        KL        ELPD
# #>                                             Intercept only   0.20643  -104.26957
# #>                                           Initial submodel   0.20118  -104.17411
# #>                                                    marker2   0.00060  -73.84232
# #>                                                    marker3   0.00000  -73.93210
# #>                var        kl rel.kl.null rel.kl    elpd delta.elpd
# #> 1   Intercept only 2.064e-01     0.00000     NA -104.27  -30.33748
# #> 2 Initial submodel 2.012e-01     0.02543  0.000 -104.17  -30.24201
# #> 3          marker2 5.964e-04     0.99711  0.997  -73.84    0.08977
# #> 4          marker3 9.133e-18     1.00000  1.000  -73.93    0.00000
# options(oldopt)  # reset options

