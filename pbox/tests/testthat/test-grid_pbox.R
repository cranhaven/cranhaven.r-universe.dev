library(testthat)
library(data.table)

# Assuming that `set_pbox` and `qpbox` are defined somewhere within the package or available in the environment


data("SEAex")
pbx <- set_pbox(SEAex[,.(Vietnam, Malaysia)])

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  result <- grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the result contains the expected columns
  expected_cols <- c("Vietnam", "Malaysia", "probs")
  expect_named(result, expected_cols)

  # Check if probabilities were computed
  expect_type(result$probs, "list")
  expect_length(result$probs, nrow(result))
})

# Test handling of invalid mj parameter
test_that("Test handling of invalid mj parameter", {
  expect_error(grid_pbox(pbx, mj = 123), "Expecting 'mj' to be a vector of variable names!")
})

# Test handling of invalid co parameter
test_that("Test handling of invalid co parameter", {
  expect_error(grid_pbox(pbx, mj = c("Vietnam"), co = 123), "Expecting 'co' to be NULL or a vector of variable names!")
})

# Test handling of NULL co parameter
test_that("Test handling of NULL co parameter", {
  result <- grid_pbox(pbx, mj = c("Vietnam", "Malaysia"), co = NULL)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the result contains the expected columns
  expected_cols <- c("Vietnam", "Malaysia", "probs")
  expect_named(result, expected_cols)

  # Check if probabilities were computed
  expect_type(result$probs, "list")
  expect_length(result$probs, nrow(result))
})

# Test with different quantiles
test_that("Test with different quantiles", {
  quantiles <- seq(0, 1, 0.2)
  result <- grid_pbox(pbx, mj = c("Vietnam", "Malaysia"), probs = quantiles)

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the result contains the expected columns
  expected_cols <- c("Vietnam", "Malaysia", "probs")
  expect_named(result, expected_cols)

  # Check if probabilities were computed
  expect_type(result$probs, "list")
  expect_length(result$probs, nrow(result))
})

# Test with empty mj parameter
test_that("Test with empty mj parameter", {
  result <- grid_pbox(pbx, mj = character())

  # Check if the result is a data.table
  expect_s3_class(result, "data.table")

  # Verify that the result is empty
  expect_equal(nrow(result), 0)
})
