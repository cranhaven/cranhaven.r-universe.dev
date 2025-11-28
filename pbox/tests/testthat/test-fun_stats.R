library(testthat)

# Sample data for testing
numeric_vector <- c(1, 2, 3, 4, 5)
numeric_vector_with_na <- c(1, 2, NA, 4, 5)

# Test correct functionality
test_that("Test correct functionality with numeric vector", {
  result <- fun_stats(numeric_vector)

  # Check if the result is a list with correct names
  expect_type(result, "list")
  expect_named(result, c("min", "max", "mean", "median"))

  # Verify calculation correctness
  expect_equal(result$min, min(numeric_vector, na.rm = TRUE))
  expect_equal(result$max, max(numeric_vector, na.rm = TRUE))
  expect_equal(result$mean, mean(numeric_vector, na.rm = TRUE))
  expect_equal(result$median, median(numeric_vector, na.rm = TRUE))
})

# Test handling of NA values
test_that("Test handling of NA values", {
  result_with_na <- fun_stats(numeric_vector_with_na)

  # Check if the result is a list with correct names
  expect_type(result_with_na, "list")
  expect_named(result_with_na, c("min", "max", "mean", "median"))

  # Verify calculation correctness excluding NA values
  expect_equal(result_with_na$min, min(numeric_vector_with_na, na.rm = TRUE))
  expect_equal(result_with_na$max, max(numeric_vector_with_na, na.rm = TRUE))
  expect_equal(result_with_na$mean, mean(numeric_vector_with_na, na.rm = TRUE))
  expect_equal(result_with_na$median, median(numeric_vector_with_na, na.rm = TRUE))
})



# Test with non-numeric input
test_that("Test with non-numeric input", {
  non_numeric_vector <- c("a", "b", "c")
  expect_error(fun_stats(non_numeric_vector))
})
