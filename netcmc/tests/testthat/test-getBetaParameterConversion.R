context("getBetaParameterConversion")

test_that("test getBetaParameterConversion can return the correct value when there is no intercept.", {
  
  set.seed(1)
  
  X = matrix(cbind(rnorm(10), rnorm(10)), 10, 2, dimnames = list(c(), c("X1", "X2")))
  betaSamples = data.frame(matrix(cbind(rnorm(10, 5), rnorm(10, -2)), 10, 2, dimnames = list(c(), c("beta1", "beta1"))))
  
  actualBetaSamples = cbind(betaSamples[, 1] / sd(X[, 1]), betaSamples[, 2] / sd(X[, 2]))
  betaSamples = getBetaParameterConversion(X, betaSamples)

  expect_true(all(betaSamples - actualBetaSamples < 1e-7))
  
})

test_that("test getBetaParameterConversion can return the correct value when there is an intercept.", {
  
  set.seed(1)
  
  X = matrix(cbind(rnorm(10), rnorm(10)), 10, 2, dimnames = list(c(), c("(Intercept)", "X2")))
  betaSamples = data.frame(matrix(cbind(rnorm(10, 5), rnorm(10, -2)), 10, 2, dimnames = list(c(), c("beta1", "beta1"))))
  
  actualBetaSamples = cbind(betaSamples[, 1] - betaSamples[, 2] * (mean(X[, 2]) / sd(X[, 2])), betaSamples[, 2] / sd(X[, 2]))
  betaSamples = getBetaParameterConversion(X, betaSamples)
  
  expect_true(all(betaSamples - actualBetaSamples < 1e-7))
  
})