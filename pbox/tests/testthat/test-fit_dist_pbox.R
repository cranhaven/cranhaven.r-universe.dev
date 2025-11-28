library(testthat)
library(gamlss)
library(data.table)
library(purrr)

# Create sample data
set.seed(123)
sample_data <- data.table(x = rnorm(100), y = runif(100),z = rnorm(100))


# Test correct functionality
test_that("Test correct functionality", {
  results <- fit_dist_pbox(sample_data)

  # Check if the results contain the expected elements
  expect_type(results, "list")
  expect_true("allDitrs" %in% names(results))
  expect_true("distTable" %in% names(results))

  # Check contents of the list elements
  expect_type(results$allDitrs, "list")
  expect_type(results$distTable, "list")

  # Test the structure of the output data table
  expect_true("DIST" %in% colnames(results$distTable))
  expect_true(ncol(results$distTable) > 1)
  expect_true(nrow(results$distTable) > 0)
})

# Test with empty data
test_that("Test with empty data", {
  empty_data <- data.table()
  expect_error(fit_dist_pbox(empty_data))
})

# Test with incorrect data types
test_that("Test with incorrect data types", {
  incorrect_data <- "not_a_dataframe"
  expect_error(fit_dist_pbox(incorrect_data))
})

# Test for correct handling of additional arguments
test_that("Test for additional arguments", {
  results_with_args <- fit_dist_pbox(sample_data, criterion = "AICc")
  expect_type(results_with_args, "list")
  expect_true("allDitrs" %in% names(results_with_args))
  expect_true("distTable" %in% names(results_with_args))
})



