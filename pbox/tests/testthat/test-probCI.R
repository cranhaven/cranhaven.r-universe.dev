library(testthat)

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  result <- probCI(probabilities, alpha = 0.05)

  # Check if the result is a matrix
  expect_type(result, "double")
  expect_equal(dim(result), c(1, 2))

  # Verify that the confidence intervals are calculated correctly
  expected_lower <- as.vector(quantile(probabilities, 0.025))
  expected_upper <- as.vector(quantile(probabilities, 0.975))
  expect_equal(result[1], expected_lower)
  expect_equal(result[2], expected_upper)
})

# Test with different alpha values
test_that("Test with different alpha values", {
  probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  result <- probCI(probabilities, alpha = 0.1)

  # Verify that the confidence intervals are calculated correctly with different alpha
  expected_lower <- as.vector(quantile(probabilities, 0.05))
  expected_upper <- as.vector(quantile(probabilities, 0.95))
  expect_equal(result[1], expected_lower)
  expect_equal(result[2], expected_upper)
})

# Test handling of single probability value
test_that("Test handling of single probability value", {
  probabilities <- c(0.3)
  result <- probCI(probabilities, alpha = 0.05)

  # Check if the result is a matrix
  expect_type(result, "double")

  # Verify that the confidence intervals are calculated correctly for a single value
  expected_lower <- as.vector(quantile(probabilities, 0.025))
  expected_upper <- as.vector(quantile(probabilities, 0.975))
  expect_equal(result[1], expected_lower)
  expect_equal(result[2], expected_upper)
})

# Test handling of empty probabilities vector
test_that("Test handling of empty probabilities vector", {
  probabilities <- numeric(0)
  result <- probCI(probabilities, alpha = 0.05)

  # Check if the result is a matrix
  expect_type(result, "double")
  expect_equal(dim(result), c(1, 2))
})

# Test handling of non-numeric input for probabilities
test_that("Test handling of non-numeric input for probabilities", {
  probabilities <- c("a", "b", "c")
  expect_error(probCI(probabilities, alpha = 0.05))
})

# Test handling of invalid alpha values
test_that("Test handling of invalid alpha values", {
  probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  expect_error(probCI(probabilities, alpha = -0.1))
  expect_error(probCI(probabilities, alpha = 1.5))
})

# Test handling of edge case values
test_that("Test handling of edge case values", {
  probabilities <- c(0, 1)
  result <- probCI(probabilities, alpha = 0.05)

  # Check if the result is a matrix
  expect_type(result, "double")

  # Verify that the confidence intervals are calculated correctly for edge case values
  expected_lower <- as.vector(quantile(probabilities, 0.025))
  expected_upper <- as.vector(quantile(probabilities, 0.975))
  expect_equal(result[1], expected_lower)
  expect_equal(result[2], expected_upper)
})
