# Test case 1: Test with a vector of sorted values containing outliers
test_that("select_by_ks_test correctly identifies and removes outliers", {
  # Create a sorted vector with outliers (length > 10)
  sorted_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 202, 203)

  # Call the select_by_ks_test function
  result <- select_by_ks_test(sorted_values)

  # Check if the result is of type "double" (numeric)
  expect_type(result, "double")
  expect_length(result, 3)

  # Check if the function correctly removed outliers
  expect_equal(result, c(1, 10, TRUE))
})

# Test case 2: Test with a vector of sorted values without outliers
test_that("select_by_ks_test handles a vector without outliers", {
  # Create a sorted vector without outliers
  sorted_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  # Call the select_by_ks_test function
  result <- select_by_ks_test(sorted_values)

  # Check if the result is a numeric vector with length 3
  expect_type(result, "double")
  expect_length(result, 3)

  # Check if the function returns the input vector with a TRUE flag
  expect_equal(result, c(1, 10, TRUE))
})

# Test case 3: Test with a vector of length less than 10
test_that("select_by_ks_test handles a vector with length less than 10", {
  # Create a sorted vector with length less than 10
  sorted_values <- c(1, 2, 3, 4, 5, 6, 7)

  # Call the select_by_ks_test function
  result <- select_by_ks_test(sorted_values)

  # Check if the result is a numeric vector with length 3
  expect_type(result, "double")
  expect_length(result, 3)

  # Check if the function returns the appropriate values for length < 10
  expect_equal(result, c(0, 0, FALSE))
})
