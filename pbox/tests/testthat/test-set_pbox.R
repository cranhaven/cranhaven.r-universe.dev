
library(testthat)
library(data.table)
library(copula)
library(gamlss)
library(gamlss.dist)


sample_data<-SEAex[,.(Vietnam,Malaysia,Thailand)]

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  pbx <- set_pbox(sample_data)

  # Check if the result is a pbox object
  expect_s4_class(pbx, "pbox")

  # Check if the slots are correctly assigned
  expect_s3_class(pbx@data, "data.table")
  expect_s4_class(pbx@copula, "mvdc")
  expect_type(pbx@fit, "list")
})

# Test handling of invalid data input
test_that("Test handling of invalid data input", {
  invalid_data <- "not_a_data_table"
  expect_error(set_pbox(invalid_data), "Input must be a data frame or a data.table")
})

# Test handling of non-continuous data
test_that("Test handling of non-continuous data", {
  non_continuous_data <- data.table(
    Var1 = c("a", "b", "c"),
    Var2 = c(1, 0, 1)
  )
  expect_error(set_pbox(non_continuous_data), "It seems that in your data there are non-continuous features")
})

# Test handling of empty data input
test_that("Test handling of empty data input", {
  empty_data <- data.frame()
  expect_error(set_pbox(empty_data), "Empty data input!")
})

# Test handling of additional arguments passed to fitDist
test_that("Test handling of additional arguments passed to fitDist", {
  pbx <- set_pbox(sample_data, method = "ML")

  # Check if the result is a pbox object
  expect_s4_class(pbx, "pbox")

  # Check if the slots are correctly assigned
  expect_s3_class(pbx@data, "data.table")
  expect_s4_class(pbx@copula, "mvdc")
  expect_type(pbx@fit, "list")
})

# Test handling of complex data input
test_that("Test handling of complex data input", {
  complex_data <- data.table(
    Var1 = rnorm(100),
    Var2 = runif(100),
    Var3 = rbinom(100, 1, 0.5)
  )
  expect_error(set_pbox(complex_data), "It seems that in your data there are non-continuous features")
})


