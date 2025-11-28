library(testthat)
library(copula)
library(data.table)




# Create sample data and pbox object for testing
pbx <- set_pbox(SEAex[,.(Vietnam,Malaysia)])
vecQuery <- c(31, 34)

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  result <- perProb(pbx, vecQuery)

  # Check if the result is a numeric value
  expect_type(result, "double")

  # Verify that the result is within a reasonable range
  expect_true(result >= 0 && result <= 1)
})

# Test handling of invalid pbox input
test_that("Test handling of invalid pbox input", {
  invalid_pbox <- "not_a_pbox"
  expect_error(perProb(invalid_pbox, vecQuery))
})

# Test handling of invalid vecQuery input
test_that("Test handling of invalid vecQuery input", {
  invalid_vecQuery <- "not_a_vector"
  expect_error(perProb(pbx, invalid_vecQuery))
})

# Test with different lengths of vecQuery
test_that("Test with different lengths of vecQuery", {
  vecQuery_short <- c(31)
  vecQuery_long <- c(31, 34, 35)

  # Verify that the function handles length mismatch
  expect_error(perProb(pbx, vecQuery_short))
  expect_error(perProb(pbx, vecQuery_long))
})

# Test with empty vecQuery
test_that("Test with empty vecQuery", {
  empty_vecQuery <- numeric(0)
  expect_error(perProb(pbx, empty_vecQuery))
})

# Test with edge case values in vecQuery
test_that("Test with edge case values in vecQuery", {
  edge_vecQuery <- c(-Inf, Inf)
  result <- perProb(pbx, edge_vecQuery)

  # Check if the result is a numeric value
  expect_type(result, "double")

  # Verify that the result is within a reasonable range
  expect_true(result >= 0 && result <= 1)
})
