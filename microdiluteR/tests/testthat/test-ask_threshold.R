test_that("ask_threshold() correctly stores a numeric number provided via user prompt", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(c(1,"A"), collapse = "\n")
  write(input, f)
  
  # Call function
  result <- ask_threshold()
  
  # Test case 1: Valid input
  expect_true(is.numeric(result), "Should be numeric.")
  
  # Test case 2: Invalid input
  expect_error(ask_threshold(), "Please enter a numeric value for 'threshold'.")

  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
