library(testthat)
library(data.table)
library(stringr)

# Test correct functionality with a typical query
test_that("Test correct functionality with a typical query", {
  query <- "Vietnam:23"
  result <- q_parser(query)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result has the correct structure
  expected_cols <- c("Varnames", "Value")
  expect_named(result, expected_cols)

  # Check if the values are parsed correctly
  expect_equal(result$Varnames, "Vietnam")
  expect_equal(result$Value, 23)
})

# Test with a query containing multiple values
test_that("Test with a query containing multiple values", {
  query <- "Vietnam:c(23,24,25)"
  result <- q_parser(query)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result has the correct structure
  expected_cols <- c("Operator", "Varnames2")
  expect_named(result, expected_cols)

  # Check if the values are parsed correctly
  expect_equal(result$Operator, "Vietnam")
  expect_equal(result$Varnames2, "23,24,25")
})

# Test with a query containing floating point values
test_that("Test with a query containing floating point values", {
  query <- "Vietnam:23.5"
  result <- q_parser(query)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result has the correct structure
  expected_cols <- c("Varnames", "Value")
  expect_named(result, expected_cols)

  # Check if the values are parsed correctly
  expect_equal(result$Varnames, "Vietnam")
  expect_equal(result$Value, 23.5)
})

# Test handling of invalid query format
test_that("Test handling of invalid query format", {
  query <- "Vietnam-23"
  result <- q_parser(query)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result is empty
  expect_equal(nrow(result), 0)
})

# Test handling of empty query
test_that("Test handling of empty query", {
  query <- ""
  result <- q_parser(query)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result is empty
  expect_equal(nrow(result), 0)
})

# Test with complex query
test_that("Test with complex query", {
  query <- "Country:USA&Age:30&Income:c(40000,50000)"
  result <- q_parser(query)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result has the correct structure
  expected_cols <- c("Varnames", "Value", "Operator", "Varnames2")
  expect_named(result, expected_cols)

})
