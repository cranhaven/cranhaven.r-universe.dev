test_that("ask_validity_method() correctly distinguishes valid from invalid options", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(c("threshold","samples","something"), collapse = "\n")
  write(input, f)
  
  # Test case 1: Valid option 'threshold'
  expect_equal(ask_validity_method(), "threshold")
  # Test case 1: Valid option 'samples'
  expect_equal(ask_validity_method(), "samples")
  # Test case 3: Invalid user input
  expect_error(ask_validity_method(), "Invalid option selected: something. Please choose either 'threshold' or 'samples'.")
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
