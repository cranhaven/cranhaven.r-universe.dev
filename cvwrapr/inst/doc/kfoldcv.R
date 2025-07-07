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

## ----message=FALSE------------------------------------------------------------
library(glmnet)
set.seed(1)
glmnet_fit <- cv.glmnet(x, y)

## ----message=FALSE------------------------------------------------------------
library(cvwrapr)
set.seed(1)
cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict)

## -----------------------------------------------------------------------------
names(glmnet_fit)
names(cv_fit)

## -----------------------------------------------------------------------------
library(testthat)
expect_equal(glmnet_fit$lambda, cv_fit$lambda)
expect_equal(glmnet_fit$cvm, cv_fit$cvm)
expect_equal(glmnet_fit$cvsd, cv_fit$cvsd)

## -----------------------------------------------------------------------------
biny <- ifelse(y > 0, 1, 0)
weights <- rep(1:2, length.out = nobs)

set.seed(1)
glmnet_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights)

## -----------------------------------------------------------------------------
set.seed(1)
cv_fit <- kfoldcv(x, biny, family = "binomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"))

## -----------------------------------------------------------------------------
expect_equal(glmnet_fit$lambda, cv_fit$lambda)
expect_equal(glmnet_fit$cvm, cv_fit$cvm)
expect_equal(glmnet_fit$cvsd, cv_fit$cvsd)

## -----------------------------------------------------------------------------
set.seed(101)
x[sample(seq(length(x)), 4 * nobs * nvars / 5)] <- 0
y <- rowSums(x[, 1:2]) + rnorm(nobs)
foldid <- sample(rep(seq(5), length = nobs))

## ----eval=FALSE---------------------------------------------------------------
#  filter <- function(x, ...) which(colMeans(x == 0) > 0.8)
#  glmnet_fit <- cv.glmnet(x, y, foldid = foldid, exclude = filter)

## ----eval=FALSE---------------------------------------------------------------
#  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
#                    train_params = list(exclude = filter), foldid = foldid)
#  
#  expect_equal(glmnet_fit$lambda, cv_fit$lambda)
#  expect_equal(glmnet_fit$cvm, cv_fit$cvm)
#  expect_equal(glmnet_fit$cvsd, cv_fit$cvsd)

## -----------------------------------------------------------------------------
train_fun <- function(x, y) {
  exclude <- which(colMeans(x == 0) > 0.8)
  if (length(exclude) == 0) {
    model <- glmnet(x, y)
  } else {
    model <- glmnet(x[, -exclude, drop = FALSE], y)
  }
  return(list(lambda = model$lambda,
              exclude = exclude,
              model = model))
}

predict_fun <- function(object, newx, s) {
  if (length(object$exclude) == 0) {
    predict(object$model, newx = newx, s = s)
  } else {
    predict(object$model, newx = newx[, -object$exclude, drop = FALSE], s = s)
  }
}

cv_fit <- kfoldcv(x, y, train_fun = train_fun, predict_fun = predict_fun,
                  foldid = foldid)

## ----eval=FALSE---------------------------------------------------------------
#  expect_equal(glmnet_fit$lambda, cv_fit$lambda)
#  expect_equal(glmnet_fit$cvm, cv_fit$cvm)
#  expect_equal(glmnet_fit$cvsd, cv_fit$cvsd)

## -----------------------------------------------------------------------------
availableTypeMeasures()

## -----------------------------------------------------------------------------
biny <- ifelse(y > 0, 1, 0)

glmnet_fit <- cv.glmnet(x, biny, family = "binomial",
                        type.measure = "class", foldid = foldid)
cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "class",
                  train_fun = glmnet, predict_fun = predict,
                  train_params = list(family = "binomial"),
                  predict_params = list(type = "response"),
                  foldid = foldid)
  
expect_equal(glmnet_fit$lambda, cv_fit$lambda)
expect_equal(glmnet_fit$cvm, cv_fit$cvm)
expect_equal(glmnet_fit$cvsd, cv_fit$cvsd)

## -----------------------------------------------------------------------------
set.seed(1)
nobs <- 100; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)

## ----message=FALSE------------------------------------------------------------
library(pls)

# lambda represents no. of PCs
train_fun <- function(x, y, lambda) {
  df <- data.frame(x, y)
  model <- pls::pcr(y ~ ., data = df, ncomp = max(lambda))
  
  return(list(lambda = lambda, model = model))
}

predict_fun <- function(object, newx, s) {
  preds <- predict(object$model, newdata = newx, ncomp = s)
  return(array(preds,
               dim = c(nrow(newx), length(s))))
}

set.seed(2)
lambda <- 1:10
cv_fit <- kfoldcv(x, y, lambda = lambda, 
                  train_fun = train_fun, predict_fun = predict_fun)

## -----------------------------------------------------------------------------
plot(cv_fit, log.lambda = FALSE)

## ----message=FALSE------------------------------------------------------------
library(gbm)

# lambda represents # of trees
train_fun <- function(x, y, lambda) {
  df <- data.frame(x, y)
  model <- gbm::gbm(y ~ ., data = df, n.trees = max(lambda),
                    distribution = "gaussian")
  
  return(list(lambda = lambda, model = model))
}

predict_fun <- function(object, newx, s) {
  newdf <- data.frame(newx)
  predict(object$model, newdata = newdf, n.trees = s)
}

set.seed(3)
lambda <- 1:100
cv_fit <- kfoldcv(x, y, lambda = lambda, 
                  train_fun = train_fun, predict_fun = predict_fun)

## -----------------------------------------------------------------------------
plot(cv_fit, log.lambda = FALSE, xlab = "No. of trees", 
     main = "CV MSE vs. no. of trees")

## -----------------------------------------------------------------------------
# lambda represents interaction depth
train_fun <- function(x, y, lambda) {
  df <- data.frame(x, y)
  model <- lapply(lambda, function(i) gbm::gbm(y ~ ., data = df, 
                                               interaction.depth = i,
                                               distribution = "gaussian"))
  
  return(list(lambda = lambda, model = model))
}

predict_fun <- function(object, newx, s) {
  newdf <- data.frame(newx)
  preds <- lapply(object$model,
                  function(obj) predict(obj, newdata = newdf, 
                                        n.trees = obj$n.trees))
  
  return(matrix(unlist(preds), nrow = nrow(newdf)))
}

set.seed(3)
lambda <- 1:5
cv_fit <- kfoldcv(x, y, lambda = lambda, 
                  train_fun = train_fun, predict_fun = predict_fun)

plot(cv_fit, log.lambda = FALSE, xlab = "Interaction depth", 
     main = "CV MSE vs. interaction depth")

## ----message=FALSE------------------------------------------------------------
# without parallel computing
set.seed(3)
foldid <- sample(rep(1:5, length.out = nrow(x)))
cv_fit1 <- kfoldcv(x, y, foldid = foldid, train_fun = glmnet, 
                   predict_fun = predict)

# with parallel computing
library(parallel)
library(doParallel)
cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
cv_fit2 <- kfoldcv(x, y, foldid = foldid, train_fun = glmnet, 
                   predict_fun = predict, parallel = TRUE)
parallel::stopCluster(cl)

# check that the two fits are the same
expect_equal(cv_fit1$lambda, cv_fit2$lambda)
expect_equal(cv_fit1$cvm, cv_fit2$cvm)
expect_equal(cv_fit1$cvsd, cv_fit2$cvsd)

