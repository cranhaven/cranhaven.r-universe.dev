test_that("apply_validation_method() correctly identifies validity in 'threshold' method", {
  
  # Test case 1: Set value below threshold as 'valid'
  expect_equal(apply_validation_method(5, 1, 1, letters[1:10], letters[1:10],
                                       validity_method = "threshold", threshold = 10), "valid")
  
  # Test case 2: Set value above threshold as 'invalid'
  expect_equal(apply_validation_method(15, 1, 1, letters[1:10], letters[1:10],
                                       validity_method = "threshold", threshold = 10), "invalid")
})

test_that("apply_validation_method() correctly identifies validity in 'samples' method", {
  
  # Generate test data
  invalid_sample <- "A-1"
  sample_not_present <- "A-100"
  
  # Test case 3: Invalid sample input should return 'invalid'
  expect_equal(apply_validation_method(5, 1, 1, LETTERS[1:8], as.character(1:12),
                                       validity_method = "samples",
                                       invalid_samples = invalid_sample), "invalid")
  
  # Test case 4: Input of sample that is not present should return 'valid'
  expect_equal(apply_validation_method(5, 1, 1, LETTERS[1:8], as.character(1:12),
                                       validity_method = "samples",
                                       invalid_samples = sample_not_present), "valid")
})

test_that("apply_validation_method correctly throws an error for invalid validity method input", {
  
  # Test case 5: If validity method is invalid, throw error
  expect_error(apply_validation_method(5, 1, 1, letters[1:10], letters[1:10],
                                       validity_method = "invalid_method"), "Invalid option selected. Please choose either 'threshold' or 'samples'.")
})
