# Test case 1: Invalid input
test_that("ask_invalid_samples() correctly checks invalid input", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- "A2\n"
  write(input, f)
  
  # Run test
  expect_error(ask_invalid_samples(),
               "Invalid input format. Please enter well positions separated by spaces in the format 'A-2'.")
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})

# Test case 2: Invalid input
test_that("ask_invalid_samples() correctly stores valid input as character vector", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- "A-2\n"
  write(input, f)
  
  # Run test
  expect_equal(ask_invalid_samples(), "A-2")
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
