# Test case 1: Test with a vector of random uniform data
test_that("ks function returns a valid p-value", {
  # Generate a vector of random uniform data
  set.seed(123)
  x <- runif(100)

  # Call the ks function
  p_value <- ks(x)

  # Check if the p-value is numeric and within the range [0, 1]
  expect_type(p_value, "double")
  expect_true(p_value >= 0 && p_value <= 1)
})

# Test case 2: Test with a vector of all zeros
test_that("ks function handles a vector of all zeros", {
  x <- rep(0, 100) # Create a vector of all zeros

  # Call the ks function
  p_value <- suppressWarnings({
    ks(x)
  })

  # Check if the p-value is numeric and equal to 1
  expect_type(p_value, "double")
  expect_equal(p_value, 0)
})
