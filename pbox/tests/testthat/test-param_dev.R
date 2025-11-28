library(testthat)

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  result <- param_dev(31, sigma = 0.05, range = seq(-3, 3, 1))

  # Check if the result is a numeric vector
  expect_type(result, "double")

  # Verify that the deviations are applied correctly
  expected_result <- 31 + seq(-3, 3, 1) * 0.05
  expect_equal(result, expected_result)
})

# Test with different sigma values
test_that("Test with different sigma values", {
  result <- param_dev(31, sigma = 0.1, range = seq(-3, 3, 1))

  # Verify that the deviations are applied correctly with different sigma
  expected_result <- 31 + seq(-3, 3, 1) * 0.1
  expect_equal(result, expected_result)
})

# Test with different range values
test_that("Test with different range values", {
  result <- param_dev(31, sigma = 0.05, range = seq(-5, 5, 2))

  # Verify that the deviations are applied correctly with different range
  expected_result <- 31 + seq(-5, 5, 2) * 0.05
  expect_equal(result, expected_result)
})


# Test handling of non-numeric input for param
test_that("Test handling of non-numeric input for param", {
  expect_error(param_dev("not_a_number"), "non-numeric argument to binary operator")
})

# Test handling of non-numeric input for sigma
test_that("Test handling of non-numeric input for sigma", {
  expect_error(param_dev(31, sigma = "not_a_number"), "non-numeric argument to binary operator")
})

# Test handling of non-numeric input for range
test_that("Test handling of non-numeric input for range", {
  expect_error(param_dev(31, range = "not_a_vector"), "non-numeric argument to binary operator")
})

# Test with empty range
test_that("Test with empty range", {
  result <- param_dev(31, sigma = 0.05, range = numeric(0))

  # Verify that the result is the same as the original parameter
  expect_equal(result, numeric(0))
})

# Test with empty param
test_that("Test with empty param", {
  result <- param_dev(numeric(0), sigma = 0.05, range = seq(-3, 3, 1))

  # Verify that the result is the same as the original parameter
  expect_equal(result, numeric(0))
})
