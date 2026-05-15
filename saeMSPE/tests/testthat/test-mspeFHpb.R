library(testthat)
library(saeMSPE)

test_that("mspeFHpb works correctly", {
  X <- matrix(runif(10 * 3), 10, 3)
  X[,1] <- rep(1, 10) 
  D <- (1:10) / 10 + 0.5
  Y <- X %*% c(0.5, 1, 1.5) + rnorm(10, 0, sqrt(2)) + rnorm(10, 0, sqrt(D))
  
  data <- data.frame(Y = Y, X1 = X[,2], X2 = X[,3])
  formula <- Y ~ X1 + X2
  result <- mspeFHpb(formula, data, D, K = 50, method = 4)
  
  expect_true(is.list(result))
  expect_true(is.vector(result$MSPE))
  expect_true(is.vector(result$bhat))
  expect_true(is.numeric(result$Ahat)) 

})