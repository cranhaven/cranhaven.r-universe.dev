
############ Validation of Vectors containing frequency values #################

test_that("validate_freqVectorSums1", {
  # Test valid input
  valid_freqVector <- c(0.2, 0.3, 0.5)
  expect_true(validate_freqVectorSums1(valid_freqVector)$valid)

  # Test invalid input - non-numeric freqVector
  non_numeric_freqVector <- c(0.2, "invalid", 0.5)
  expect_error(validate_freqVectorSums1(non_numeric_freqVector),
               info = "funtion does not return an error for non-numeric vector input")
  non_vector_freqVector <- matrix(c(.2, .3, .5), nrow = 1)
  expect_error(validate_freqVectorSums1(non_vector_freqVector),
               info = "function does not return an error for non-vector input")

  # Test function reports non-valid state when given freqs don't sum to 1
  freqVector_notSum1 <- c(0.3, 0.3, 0.5)
  expect_false(validate_freqVectorSums1(freqVector_notSum1)$valid,
               info = "fails reporting non-valid freqs state when freqs dont sum 1")

  # Function reports valid state when given freqs sum to 1
  expect_true(validate_freqVectorSums1(valid_freqVector)$valid,
              info = "fails reporting valid freqs state when freqs sum 1")

})

test_that("validate_freqVector_elements", {
  # Test valid input
  valid_freqVector <- c(0.2, 0.3, 0.5)
  expect_no_error(validate_freqVector_elements(valid_freqVector))

  # Test invalid input - non-numeric freqVector
  non_numeric_freqVector <- c(0.2, "invalid", 0.5)
  expect_error(validate_freqVector_elements(non_numeric_freqVector),
               info = "funtion does not return an error for non-numeric vector input")
  non_vector_freqVector <- matrix(c(.2, .3, .5), nrow = 1)
  expect_error(validate_freqVector_elements(non_vector_freqVector),
               info = "function does not return an error for non-vector input")

  # Test function reports non-valid state when given freqs elements > 1 or < 0
  freqVector_invalid <- c(-0.3, 0.3, 0.5)
  expect_false(validate_freqVector_elements(freqVector_invalid)$valid,
               info = "fails reporting non-valid freqs state when elements arent between 0 and 1")

  # Function reports valid state when given freqs sum to 1
  valid_freqVector <- c(0, 0, 1)
  expect_true(validate_freqVector_elements(valid_freqVector)$valid,
              info = "fails reporting valid freqs state when elements are between 0 and 1")

})

test_that("listFreqVector_validation", {
  invalid_vector_list <- list(c(1, -1, 1, 1), "a")
  valid_vector_list <- list(c(1, -1, 1, 1), c(1, 2, 3))
  valid_listName <- "Qci"
  wrong_listName <- 1
  # Function throws an error when element of list is not a matrix
  expect_error(listFreqVector_validation(valid_vector_list, wrong_listName),
               info = "fails throwing error for non character string listName argument")
  # Function returns correct listName
  expect_equal(listFreqVector_validation(valid_vector_list, valid_listName)$listName, valid_listName,
               info = "listName output is not equal to given listName as argument")
  # Function throws an error when element of list is not a matrix
  expect_error(listFreqVector_validation(invalid_vector_list, valid_listName),
               info = "fails throwing error for non-vector list listTransitionMatrix argument")
  # Function returns list of validationStates with number of elements to input list
  expect_true(length(listFreqVector_validation(valid_vector_list, valid_listName)$validationStates) == 2,
              info = "validationStates length different form provided listFreqVector length")
})

