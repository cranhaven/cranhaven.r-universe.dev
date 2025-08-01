# le pido a ChatGPT que prepare pruebas testthat para la siguiente función

library(testthat)
library(rvif)

#####################################################################################################################################

# Assuming that the rvifs() function is already loaded

test_that("Throws message if there are less than 2 variables.", {
  set.seed(2025)
  x <- matrix(rnorm(10), ncol = 1)
  expect_message(rvifs(x, intercept = FALSE), 
                 "At least 2 independent variables")
})

test_that("Detects constant variable when intercept=TRUE.", {
  set.seed(2025)
  x <- cbind(1, rnorm(10), rep(5, 10))  # third column constant
  expect_message(rvifs(x, intercept = TRUE), 
                 "There is a constant variable")
})

test_that("Detects constant variable when intercept=FALSE.", {
  set.seed(2025)
  x <- cbind(rnorm(10), rep(3, 10))  # second column constant
  expect_message(rvifs(x, intercept = FALSE), 
                 "There is a constant variable")
})

test_that("Detects perfect multicollinearity.", {
  set.seed(2025)
  x1 <- rnorm(10)
  x2 <- x1 * 2
  x <- cbind(1, x1, x2)
  expect_message(rvifs(x), 
                 "Perfect multicollinearity exists")
})

test_that("Detecta sistema casi singular", {
  set.seed(2025)
  x1 <- rnorm(10)
  x2 <- x1 + 1e-15
  x <- cbind(1, x1, x2)
  expect_message(rvifs(x, tol = 1e-10), 
                 "System is computationally singular")
})

test_that("Returns data.frame with RVIF and % columns", {
  set.seed(2025)
  x <- cbind(1, rnorm(50), rnorm(50))
  result <- rvifs(x)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("RVIF", "%"))
  expect_equal(nrow(result), 3)
})

test_that("Works with ul = FALSE", {
  set.seed(2025)
  x <- cbind(1, rnorm(30), rnorm(30))
  result <- rvifs(x, ul = FALSE)
  expect_s3_class(result, "data.frame")
})

#####################################################################################################################################

# testthat::test_file("test-rvifs.R")