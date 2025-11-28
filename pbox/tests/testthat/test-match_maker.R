library(testthat)
library(data.table)


# Sample data for testing
varSet <- data.table(Varnames = c("Var1", "Var2", "Var3"), Value = c(Inf, Inf, Inf))
matches <- data.table(Varnames = c("Var1", "Var3"), Value = c(10, 30))
data <- data.table(Var1 = rnorm(10), Var2 = rnorm(10), Var3 = rnorm(10))

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  result <- match_maker(varSet, matches, data)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the values in varSet are updated correctly
  expect_equal(result$Value[1], 10)
  expect_equal(result$Value[3], 30)
})

# Test handling of mean operations
test_that("Test handling of additional operations", {
  matches_with_ops <- data.table(Varnames2 = c("Var2"), Operator = c("mean"))
  result <- match_maker(varSet, matches_with_ops, data)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the values in varSet are updated correctly by the operation
  expect_equal(result$Value[2], mean(data$Var2))
})

# Test handling of invalid varSet input
test_that("Test handling of invalid varSet input", {
  invalid_varSet <- "not_a_data_table"
  expect_error(match_maker(invalid_varSet, matches, data))
})

# Test handling of invalid matches input
test_that("Test handling of invalid matches input", {
  invalid_matches <- "not_a_data_table"
  expect_error(match_maker(varSet, invalid_matches, data))
})

# Test handling of invalid data input
test_that("Test handling of invalid data input", {
  invalid_data <- "not_a_data_table"
  expect_error(match_maker(varSet, matches, invalid_data))
})

# Test with empty matches input
test_that("Test with empty matches input", {
  empty_matches <- data.table(Varnames = character(), Value = numeric())
  result <- match_maker(varSet, empty_matches, data)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the values in varSet are not updated
  expect_true(all(is.infinite(result$Value)))
})

# Test with no matching variables in varSet
test_that("Test with no matching variables in varSet", {
  non_matching_matches <- data.table(Varnames = c("NonExistentVar"), Value = c(10))
  expect_error(match_maker(varSet, non_matching_matches, data))


})
