# Test case 1
test_that("ask_treatment_list() correctly stores eight user inputs in an attributed list with parameter 'horizontal'", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(LETTERS[1:8], collapse = "\n")
  write(input, f)
  
  # Call function
  result_horizontal <- ask_treatment_list(direction = "horizontal")

  # Test cases
  expect_equal(names(result_horizontal), LETTERS[1:8])
  expect_equal(attr(result_horizontal, "axis"), "row")
  expect_equal(result_horizontal$A, "A")
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})

# Test case 2
test_that("ask_treatment_list() correctly stores twelve user inputs in an attributed list with parameter 'vertical'", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(LETTERS[1:12], collapse = "\n")
  write(input, f)
  
  # Tests
  result_vertical <- ask_treatment_list(direction = "vertical")
  expect_equal(names(result_vertical), as.character(1:12))
  expect_equal(attr(result_vertical, "axis"), "column")
  expect_equal(result_vertical$`1`, "A")
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
