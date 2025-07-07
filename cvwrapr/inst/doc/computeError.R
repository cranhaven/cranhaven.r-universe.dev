## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 5,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(1)
nobs <- 100; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
biny <- ifelse(y > 0, 1, 0)

## ----message=FALSE------------------------------------------------------------
library(glmnet)
library(cvwrapr)

foldid <- sample(rep(seq(5), length = nobs))
cv_fit <- kfoldcv(x, biny, family = "binomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial"),
                    predict_params = list(type = "response"),
                    foldid = foldid, keep = TRUE)
plot(cv_fit)

## -----------------------------------------------------------------------------
misclass <- computeError(cv_fit$fit.preval, biny, cv_fit$lambda, foldid, 
                         type.measure = "class", family = "binomial")
misclass$cvm

## -----------------------------------------------------------------------------
plot(misclass)

## -----------------------------------------------------------------------------
availableTypeMeasures()

## -----------------------------------------------------------------------------
library(survival)
survy <- survival::Surv(exp(y), event = rep(c(0, 1), length.out = nobs))

cv_fit <- kfoldcv(x, survy, family = "cox", type.measure = "C",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox"),
                    predict_params = list(type = "response"),
                    foldid = foldid, keep = TRUE)
plot(cv_fit)

## ----error=TRUE---------------------------------------------------------------
deviance_cvm <- computeError(cv_fit$fit.preval, survy, cv_fit$lambda, foldid, 
                             type.measure = "deviance", family = "cox")

## -----------------------------------------------------------------------------
cv_fit2 <- kfoldcv(x, survy, family = "cox", type.measure = "deviance",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "cox"),
                    predict_params = list(type = "response"),
                    foldid = foldid, keep = TRUE)
plot(cv_fit2)

## -----------------------------------------------------------------------------
deviance_cvm <- computeError(cv_fit$fit.preval, survy, cv_fit$lambda, foldid, 
                             type.measure = "deviance", family = "cox",
                             grouped = FALSE)
plot(deviance_cvm)

