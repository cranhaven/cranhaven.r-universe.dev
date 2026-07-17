# Generate test data
test_data <- matrix(c(5, 20, 10, 25), ncol = 2)
rownames(test_data) <- LETTERS[1:2]
colnames(test_data) <- 1:2
threshold <- 10
invalid_samples <- c("A-1", "B-2")

# Test case 1: Validate matrix according to 'threshold' method
test_that("validate_cells() correctly validates data based on 'threshold' validation method", {
  
  # Generate expected output
  expected_output_threshold <- data.frame(
    Position = c("A-1", "A-2", "B-1", "B-2"),
    Value = c(5, 10, 20, 25),
    Validity = c("valid", "valid", "invalid", "invalid"))
  
  # Test
  expect_equal(validate_cells(test_data, rownames(test_data), colnames(test_data),
                              validity_method = "threshold",
                              threshold = threshold), expected_output_threshold)
})

# Test case 2: Validate matrix according to 'samples' method
test_that("validate_cells() correctly validates data based on 'samples' validation method", {
  
  # Generate expected output
  expected_output_samples <- data.frame(
    Position = c("A-1", "A-2", "B-1", "B-2"),
    Value = c(5, 10, 20, 25),
    Validity = c("invalid", "valid", "valid", "invalid"))
  
  # Test
  expect_equal(validate_cells(test_data,  rownames(test_data), colnames(test_data),
                              validity_method = "samples",
                              invalid_samples = invalid_samples), expected_output_samples)
})

# Test case 1: If invalid method is selected, throw error
test_that("Error is thrown for invalid validity method", {
  
  # Test
  expect_error(validate_cells(test_data,  rownames(test_data), colnames(test_data),
                              validity_method = "something_invalid"),
               "Invalid option selected. Please choose either 'threshold' or 'samples'.")
})
