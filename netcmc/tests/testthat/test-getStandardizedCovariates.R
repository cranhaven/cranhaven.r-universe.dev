context("getStandardizedCovariates")

test_that("test getStandardizedCovariates can return the expected response (y).", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "X1", "X2"))))
  formula = Y ~ X1 + X2
  
  y = getStandardizedCovariates(formula, data)$y
  y = as.numeric(y)
  actualY = c(10, 20, 30)

  expect_equal(y, actualY)
  
})

test_that("test getStandardizedCovariates can return the expected response (y) when a function is applied to it.", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "X1", "X2"))))
  formula = I(Y^2) ~ X1 + X2
  
  y = getStandardizedCovariates(formula, data)$y
  y = as.numeric(y)
  actualY = c(100, 400, 900)
  
  expect_equal(y, actualY)
  
})

test_that("test getStandardizedCovariates can return the expected design matrix (X).", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ Cov1 + Cov2
  
  X = getStandardizedCovariates(formula, data)$X
  actualX = matrix(cbind(c(1, 1, 1), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("(Intercept)", "Cov1", "Cov2")))
  
  expect_true(all(X == actualX))
  
})

test_that("test getStandardizedCovariates can return the expected design matrix (X) when there is no intercept.", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ Cov1 + Cov2 - 1
  
  X = getStandardizedCovariates(formula, data)$X
  actualX = matrix(cbind(c(0, 0.5, 1), c(0, -0.5, -1)), 3, 2, dimnames = list(c(), c("Cov1", "Cov2")))
  
  expect_true(all(X == actualX))
  
})

test_that("test getStandardizedCovariates can return the expected design matrix (X) when a function is applied to a covariate.", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ I(Cov1^2) + Cov2
  
  X = getStandardizedCovariates(formula, data)$X
  actualX = matrix(cbind(c(1, 1, 1), c(0, 0.25, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("(Intercept)", "Cov1", "Cov2")))
  
  expect_true(all(X == actualX))
  
})

test_that("test getStandardizedCovariates can return the expected design matrix (X) when one of the covariates is a factor.", {
  
  # This needs to be verified to see if it makes sense.
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(10, 50, 100), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ as.factor(Cov1) + Cov2
  
  X = getStandardizedCovariates(formula, data)$X
  actualX = matrix(cbind(c(1, 1, 1), c(0, 1, 0), c(0, 0, 1), c(0, -0.5, -1)), 3, 4, dimnames = list(c(), c("(Intercept)", "as.factor(Cov1)50", "as.factor(Cov1)100", "Cov2")))
  
  expect_true(all(X == actualX))
  
})

test_that("test getStandardizedCovariates can return the expected design matrix (X) when one of the covariates is a factor and there is no intercept.", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(10, 50, 100), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ as.factor(Cov1) + Cov2 - 1
  
  X = getStandardizedCovariates(formula, data)$X
  actualX = matrix(cbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0, -0.5, -1)), 3, 4, dimnames = list(c(), c("as.factor(Cov1)10", "as.factor(Cov1)50", "as.factor(Cov1)100", "Cov2")))
  
  expect_true(all(X == actualX))
  
})

test_that("test getStandardizedCovariates can return the expected standardized design matrix (standardizedX).", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ Cov1 + Cov2
  
  standardizedX = getStandardizedCovariates(formula, data)$standardizedX
  actualStandardizedX = matrix(cbind(c(1, 1, 1), c(-1, 0, 1), c(1, 0, -1)), 3, 3, dimnames = list(c(), c("(Intercept)", "Cov1", "Cov2")))
  
  expect_true(all(standardizedX == actualStandardizedX))
  
})

test_that("test getStandardizedCovariates can return the expected standardized design matrix (standardizedX) when one of the covariates is a factor.", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30, 40), c(10, 50, 100, 100), c(0, -0.5, -1, -1.5)), 4, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ as.factor(Cov1) + Cov2

  standardizedX = getStandardizedCovariates(formula, data)$standardizedX
  actualStandardizedX = matrix(cbind(c(1, 1, 1, 1), c(-0.5, 1.5, -0.5, -0.5), c(-0.8660254, -0.8660254, 0.8660254, 0.8660254), c(1.1618950, 0.3872983, -0.3872983, -1.1618950)), 4, 4, dimnames = list(c(), c("(Intercept)", "as.factor(Cov1)50", "as.factor(Cov1)100", "Cov2")))

  expect_true(all(standardizedX - actualStandardizedX < 1e-7))
  
})

test_that("test getStandardizedCovariates can return the expected standardized design matrix (standardizedX) when there is no intercept.", {
  
  data = data.frame(matrix(cbind(c(10, 20, 30), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "Cov1", "Cov2"))))
  formula = Y ~ Cov1 + Cov2 - 1
  
  standardizedX = getStandardizedCovariates(formula, data)$standardizedX
  actualStandardizedX = matrix(cbind(c(-1, 0, 1), c(1, 0, -1)), 3, 2, dimnames = list(c(), c("Cov1", "Cov2")))
  
  expect_true(all(standardizedX == actualStandardizedX))
  
})