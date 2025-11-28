library(testthat)
library(data.table)

# Sample data for testing
data <- data.table(
  Var1 = c(1, 2, 3, 4, 5),
  Var2 = c(6, 7, 8, 9, 10),
  Var3 = c(11, 12, 13, 14, 15)
)

# Sample matches data frame
matches <- data.table(
  Operator = c("mean", "median"),
  Varnames2 = c("Var1,Var2", "Var3")
)

# Sample varSet data frame
varSet <- data.table(
  Varnames = c("Var1", "Var2", "Var3"),
  Value = c(Inf,Inf,Inf)
)

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  result <- stats_calc(data, matches, varSet)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result has the correct structure
  expect_named(result, c("Varnames", "Value"))

  # Check if the values are calculated correctly
  expect_equal(result[1, Value], mean(data$Var1))
  expect_equal(result[2, Value], mean(data$Var2))
  expect_equal(result[3, Value], median(data$Var3))
})

# Test handling of unsupported operator
test_that("Test handling of unsupported operator", {
  invalid_matches <- data.table(
    Operator = c("sum"),
    Varnames2 = c("Var1")
  )
  expect_error(stats_calc(data, invalid_matches, varSet), "Unsupported operator. Only 'mean' and 'median' are supported.")
})

# Test handling of empty data input
test_that("Test handling of empty data input", {
  empty_data <- data.table()

  # Check if give error
  expect_error( stats_calc(empty_data, matches, varSet))
})

# Test handling of empty matches input
test_that("Test handling of empty matches input", {
  empty_matches <- data.table(Operator = character(), Varnames2 = character())

  # Check if give error
  expect_error( stats_calc(data, empty_matches, varSet))
})

# Test handling of empty varSet input
test_that("Test handling of empty varSet input", {
  empty_varSet <- data.table(Varnames = character(), Value = numeric())
  result <- stats_calc(data, matches, empty_varSet)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Check if the result has the correct structure
  expect_named(result, c("Varnames", "Value"))

  # Check if the result is empty
  expect_equal(nrow(result), 0)
})

