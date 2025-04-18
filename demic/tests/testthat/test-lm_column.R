# Test case 1: Test with two vectors of equal length
test_that("lm_column returns coefficients and p-value for linear regression", {
  # Create two vectors of equal length
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)

  # Call the lm_column function
  result <- lm_column(x, y)
  # Check if the result is a numeric vector
  expect_type(result, "double")

  # Check if the coefficients and p-value are correct
  expect_equal(result[1][["y"]], 0.5) # Expected coefficient
  expect_equal(as.numeric(result[2]), 0, tolerance = 1e-20) # Expected p-value (approximately 0.5)
})
