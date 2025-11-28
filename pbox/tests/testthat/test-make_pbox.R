library(testthat)
library(data.table)
library(copula)


# Create sample data and copula for testing
sample_data <- data.table(matrix(rnorm(400), ncol = 4))
names(sample_data)<-LETTERS[1:4]
cop <- normalCopula(param = 0.5, dim = 4)
distList <- c("norm", "norm", "norm", "norm")
allDistrs <- list(
  list(mean = 0, sd = 1),
  list(mean = 0, sd = 1),
  list(mean = 0, sd = 1),
  list(mean = 0, sd = 1)
)
copSEA <- mvdc(cop, distList, allDistrs)

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  pbx <- make_pbox(data = sample_data, cop = copSEA)

  # Check if the result is a pbox object
  expect_s4_class(pbx, "pbox")

  # Verify that the slots are correctly assigned
  expect_s3_class(pbx@data, "data.table")
  expect_s4_class(pbx@copula, "mvdc")
})

# Test handling of invalid data input
test_that("Test handling of invalid data input", {
  invalid_data <- "not_a_data_frame"
  expect_error(make_pbox(data = invalid_data, cop = copSEA), "Input must be a data frame or a data.table")
})

# Test handling of invalid copula input
test_that("Test handling of invalid copula input", {
  invalid_cop <- "not_a_copula"
  expect_error(make_pbox(data = sample_data, cop = invalid_cop), "Input must be an object of class mvdc")
})

# Test with mismatched data and copula dimensions
test_that("Test with mismatched data and copula dimensions", {
  mismatched_data <- data.table(matrix(rnorm(300), ncol = 3))  # Only 3 columns
  expect_error(make_pbox(data = mismatched_data, cop = copSEA), "The number of columns in the datset and the dimension of the copula object do not match")
})

# Test with additional edge cases
test_that("Test with empty data input", {
  empty_data <- data.table()
  expect_error(make_pbox(data = empty_data, cop = copSEA), "The number of columns in the datset and the dimension of the copula object do not match")
})

# Test with correct data input but with additional columns
test_that("Test with correct data input but with additional columns", {
  extended_data <- data.table(matrix(rnorm(500), ncol = 5))  # 5 columns instead of 4
  expect_error(make_pbox(data = extended_data, cop = copSEA), "The number of columns in the datset and the dimension of the copula object do not match")
})
