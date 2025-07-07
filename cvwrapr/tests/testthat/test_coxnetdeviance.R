# create fake data
set.seed(1)
nobs <- 10; nvars <- 5
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
x_sparse <- Matrix::Matrix(x, sparse = TRUE)
ty <- 1:10
tcens <- rep(0:1, length.out = 10)
strata <- rep(1:3, length.out = 10)

test_that("right-censored, all deaths, no ties", {
  y <- Surv(1:3, c(1, 1, 1))
  expect_equal(glmnet::coxnet.deviance(pred = 4:2, y = y),
               cvwrapr::coxnet.deviance(pred = exp(4:2), y = y))
})

test_that("right-censored, all deaths, ties", {
  y <- Surv(c(1, 2, 2), c(1, 1, 1))
  expect_equal(glmnet::coxnet.deviance(pred = c(1, 2, 2), y = y),
               cvwrapr::coxnet.deviance(pred = exp(c(1, 2, 2)), y = y))
})

test_that("right-censored, mix of deaths and censored, no ties", {
  set.seed(1)
  y <- Surv(runif(10, min = 1, max = 10),
            sample(0:1, size = 10, replace = TRUE))
  pred <- runif(10, min = 1, max = 10)
  expect_equal(glmnet::coxnet.deviance(pred = pred, y = y),
               cvwrapr::coxnet.deviance(pred = exp(pred), y = y))
})

test_that("right-censored, mix of deaths and censored, ties", {
  set.seed(1)
  y <- Surv(c(1, 1, 1, 2, 2, 3), c(1, 0, 1, 1, 1, 0))
  pred <- runif(6)
  expect_equal(glmnet::coxnet.deviance(pred = pred, y = y),
               cvwrapr::coxnet.deviance(pred = exp(pred), y = y))
})

test_that("right-censored, weights, std.weights = TRUE (default)", {
  y <- Surv(c(1, 1, 3, 4, 4), c(1, 0, 1, 1, 1))
  weights <- 1:5
  expect_equal(glmnet::coxnet.deviance(pred = 7:3, y = y, weights = weights),
               cvwrapr::coxnet.deviance(pred = exp(7:3), y = y, weights = weights))
})

test_that("right-censored, weights, std.weights = FALSE", {
  y <- Surv(c(1, 1, 3, 4, 4), c(1, 0, 1, 1, 1))
  weights <- 1:5
  expect_equal(glmnet::coxnet.deviance(pred = c(2, 4, 3, 5, 1), y = y,
                               weights = weights, std.weights = FALSE),
               cvwrapr::coxnet.deviance(pred = exp(c(2, 4, 3, 5, 1)), y = y,
                                        weights = weights, std.weights = FALSE))
})

test_that("right-censored, no difference when strata are all same", {
  set.seed(1)
  y <- Surv(runif(50), sample(0:1, size = 50, replace = TRUE))
  y <- stratifySurv(y, rep(1, 50))
  weights <- runif(50)
  pred <- runif(50)
  expect_equal(glmnet::coxnet.deviance(pred = pred, y = y, weights = weights),
               cvwrapr::coxnet.deviance(pred = exp(pred), y = y, weights = weights))
})

test_that("right-censored, 2 strata", {
  set.seed(1)
  y <- Surv(runif(50), sample(0:1, size = 50, replace = TRUE))
  weights <- runif(50)
  pred <- runif(50)
  strata <- c(rep(1, 25), rep(2, 25))
  y <- stratifySurv(y, strata)
  expect_equal(glmnet::coxnet.deviance(pred = pred, y = y, weights = weights,
                               std.weights = FALSE),
               cvwrapr::coxnet.deviance(pred = exp(pred), y = y, weights = weights,
                                       std.weights = FALSE))
})

test_that("right-censored, 2 strata, pred matrix", {
  set.seed(1)
  y <- Surv(runif(50), sample(0:1, size = 50, replace = TRUE))
  weights <- runif(50)
  pred <- runif(50)
  pred <- matrix(rep(pred, times = 3), ncol = 3)
  strata <- c(rep(1, 25), rep(2, 25))
  y <- stratifySurv(y, strata)
  expect_equal(glmnet::coxnet.deviance(pred = pred, y = y, weights = weights,
                               std.weights = FALSE),
               cvwrapr::coxnet.deviance(pred = exp(pred), y = y, weights = weights,
                                       std.weights = FALSE))
})

test_that("right-censored, 2 strata with offset", {
  set.seed(2)
  y <- Surv(runif(50), sample(0:1, size = 50, replace = TRUE))
  weights <- runif(50)
  pred <- runif(50)
  strata <- c(rep(1, 25), rep(2, 25))
  y <- stratifySurv(y, strata)
  offset <- runif(50)
  newpred <- exp(pred + offset)
  expect_equal(glmnet::coxnet.deviance(pred = pred, y = y, weights = weights,
                               std.weights = FALSE, offset = offset),
               cvwrapr::coxnet.deviance(pred = newpred, y = y, weights = weights,
                                       std.weights = FALSE))
})

test_that("start-stop, small example", {
  y <- Surv(c(1, 2, 2, 2, 4), c(3, 3, 4, 6, 5), c(1, 1, 1, 0, 1))
  weights <- 1:5
  lsat <- - 3 * log(3) - 3 * log(3) - 5 * log(5)
  loglik <- 1*1 + 2*2 - 3*log(1*exp(1) + 2*exp(2) + 3*exp(3) + 4*exp(4)) +
    3*3 - 3*log(3*exp(3) + 4*exp(4)) +
    5*5 - 5*log(4*exp(4) + 5*exp(5))
  expect_equal(cvwrapr::coxnet.deviance(pred = exp(1:5), y = y, weights = weights,
                               std.weights = FALSE),
               2 * (lsat - loglik))
})

test_that("start-stop, pred matrix", {
  y <- Surv(c(1, 2, 2, 2, 4), c(3, 3, 4, 6, 5), c(1, 1, 1, 0, 1))
  weights <- 1:5
  lsat <- - 3 * log(3) - 3 * log(3) - 5 * log(5)
  loglik <- 1*1 + 2*2 - 3*log(1*exp(1) + 2*exp(2) + 3*exp(3) + 4*exp(4)) +
    3*3 - 3*log(3*exp(3) + 4*exp(4)) +
    5*5 - 5*log(4*exp(4) + 5*exp(5))
  pred <- matrix(rep(1:5, times = 3), ncol = 3)
  ans <- 2 * (lsat - loglik)
  expect_equal(cvwrapr::coxnet.deviance(pred = exp(pred), y = y, weights = weights,
                               std.weights = FALSE),
               rep(ans, 3))
})
