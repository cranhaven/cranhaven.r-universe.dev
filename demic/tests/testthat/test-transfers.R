# Sample data for testing
sample_data_x <- data.frame(
  sample = c("Sample1", "Sample2", "Sample3"),
  est_ptr = c(0.5, 0.6, 0.7),
  coefficient = c(1.0, 1.1, 1.2),
  pValue = c(0.01, 0.02, 0.03),
  cor = c(0.9, -0.8, 0.7),
  correctY = c(10, 20, 30)
)

sample_data_y <- data.frame(
  sample = c("Sample2", "Sample3", "Sample4"),
  test_ptr2 = c(0.4, 0.5, 0.6),
  coefficient = c(1.1, 1.2, 1.3),
  pValue = c(0.02, 0.03, 0.04),
  cor = c(-0.8, 0.7, 0.6),
  correctY = c(20, 30, 40)
)

# Test cases for consist_transfer function
test_that("consist_transfer correctly computes mean of absolute values", {
  result <- consist_transfer(sample_data_x, sample_data_y, i = 1)

  # Expected output: The result should be a named vector with sample names as names and mean of absolute values as values.
  expected_result <- c(Sample1 = 0.75, Sample2 = 0.85, Sample3 = 0.95, Sample4 = NaN)

  expect_equal(result, expected_result)
})

test_that("consist_transfer correctly computes maximum of absolute values", {
  result <- consist_transfer(sample_data_x, sample_data_y, i = 2)

  # Expected output: The result should be a named vector with sample names as names and maximum of absolute values as values.
  expected_result <- c(Sample1 = 1.0, Sample2 = 1.1, Sample3 = 1.2, Sample4 = -1 * Inf)

  expect_equal(result, expected_result)
})

# Test cases for df_transfer function
test_that("df_transfer correctly integrates data frames", {
  result <- df_transfer(sample_data_x, sample_data_y)

  # Expected output: The result should be a data frame with the integrated information from both input data frames.
  expected_result <- data.frame(
    sample = c("Sample1", "Sample2", "Sample3", "Sample4"),
    est_ptr = c(0.5, 0.5, 0.6, 0.6),
    coefficient = c(1.0, 1.1, 1.2, 1.3),
    pValue = c(0.01, 0.02, 0.03, 0.04),
    cor = c(0.9, 0.8, 0.7, 0.6),
    correctY = c(10, 20, 30, 40)
  )
  names(expected_result) <- c("sample", "est_ptr", "coefficient", "pValue", "cor", "correctY")
  row.names(expected_result) <- c("Sample1", "Sample2", "Sample3", "Sample4")

  expect_equal(result, expected_result)
})
