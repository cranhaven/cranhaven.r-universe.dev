# le pido a ChatGPT que prepare pruebas testthat para la siguiente función

library(testthat)
library(rvif)

#####################################################################################################################################

# Assuming that the cv_vif() function is already loaded

test_that("Returns a message if there are fewer than 3 variables.", {
  x <- matrix(c(1, 1, 1, 2), ncol = 2)
  expect_message(cv_vif(x), 
                 "At least 3 independent variables are needed")
})

test_that("Detects constant variable other than the intercept.", {
  set.seed(2025)
  x <- cbind(1, rnorm(10), rep(3, 10))  # The third column is constant
  expect_message(cv_vif(x), 
                 "There is a constant variable")
})

test_that("Detects perfect multicollinearity.", {
  set.seed(2025)
  x1 <- rnorm(10)
  x2 <- 2 * x1
  x <- cbind(1, x1, x2)
  expect_message(cv_vif(x), 
                 "Perfect multicollinearity exists")
})

test_that("Detects numerical singularity.", {
  set.seed(2025)
  x1 <- rnorm(10)
  x2 <- x1 + 3e-16  # Practically linearly dependent
  x <- cbind(1, x1, x2)
  expect_message(cv_vif(x), 
                 "System is computationally singular")
})

test_that("Returns a data frame with CV and VIF columns.", {
  set.seed(2025)
  x <- cbind(1, rnorm(50), rnorm(50))
  result <- cv_vif(x)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("CV", "VIF"))
  expect_equal(nrow(result), 2)  # p = 3, remove intercept
})

#####################################################################################################################################

# testthat::test_file("test-cv_vif.R")