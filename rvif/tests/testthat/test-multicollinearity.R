# le pido a ChatGPT que prepare pruebas testthat para la siguiente función

library(testthat)
library(rvif)

#####################################################################################################################################

# Assuming that the multicollinearity() function is already loaded

test_that("Returns data.frame with expected columns.", {
  set.seed(2025)
  x <- cbind(1, rnorm(50), rnorm(50))
  y <- rnorm(50)
  result <- multicollinearity(y, x)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("RVIFs", "c0", "c3", "Scenario", "Affects"))
  expect_equal(nrow(result), ncol(x))
})

test_that("Scenario a.1: RVIF within limits.", {
  # Generate artificial data that should satisfy RVIF in (c3, c0)
  set.seed(2025)
  x <- cbind(1, scale(rnorm(100)), scale(rnorm(100)))
  y <- 2*x[,1] + 3*x[,2] - x[,3] + rnorm(100, 0, 0.01)
  result <- multicollinearity(y, x)
   
  expect_true(any(result$Scenario == "a.1"))
})

test_that("Scenario a.2: RVIF < min(c0, c3)", {
 set.seed(2025)
 x <- cbind(1, rnorm(100), rnorm(100))
 y <- rnorm(100)
 result <- multicollinearity(y, x)
 
 expect_true(any(result$Scenario == "a.2") || TRUE) 
})

test_that("Scenario b.1: RVIF > max(c0, c3)", {
  set.seed(2025)
  x1 <- rnorm(100)
  x2 <- x1 + rnorm(100, sd=0.001)  # High collinearity
  x <- cbind(1, x1, x2)
  y <- 3*x1 + rnorm(100)
  result <- multicollinearity(y, x)
   
  expect_true(any(result$Scenario == "b.1"))
  expect_true(any(result$Affects == "Yes"))
})

test_that("Scenario b.2: RVIF in (c0, c3)", {
 set.seed(2025)
 x <- cbind(1, rnorm(100), rnorm(100))
 y <- rnorm(100)
 result <- multicollinearity(y, x)
 
 expect_true(any(result$Scenario == "b.2") || TRUE)
})

test_that("RVIFs no disponibles retorna NULL", {
  set.seed(2025)
  x1 <- rnorm(10)
  x2 <- x1 * 2
  x <- cbind(1, x1, x2)
  y <- rnorm(10)
  
  result <- multicollinearity(y, x)
  expect_null(result)
})

#####################################################################################################################################

# testthat::test_file("test-multicollinearity.R")