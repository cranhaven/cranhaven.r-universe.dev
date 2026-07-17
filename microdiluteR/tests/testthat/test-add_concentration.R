# Define test data
set.seed(123)
test_data <- data.frame(Position = sort(c(outer(LETTERS[1:8], 1:12,
                                                     function(x, y) paste0(x, "-", y)))),
                        Value = sample(1:10, 96, T))
concentration_list_horizontal <- list(A = 1, B = 2, C = 3, D = 4,
                                      E = 5, F = 6, G = 7, H = 8)
concentration_list_vertical <- list(`1` = 1, `2` = 2, `3` = 3, `4` = 4,
                                    `5` = 5, `6` = 6, `7` = 7, `8` = 8,
                                    `9` = 9, `10` = 10, `11` = 11, `12` = 12)

test_that("add_concentration() correctly add a concentration column without user prompt", {
  
  # Call function
  result_horizontal <- add_concentration(test_data, concentration_list_horizontal, ask_concentration_list = F)
  result_vertical <- add_concentration(test_data, concentration_list_vertical, ask_concentration_list = F)
  
  # Test case 1: Check if result has three columns
  expect_true(ncol(result_horizontal) >= 3, 
              "Output does not contain three columns")
  expect_true(ncol(result_vertical) >= 3, 
              "Output does not contain three columns")
  
  # Test case 2: Check if column 'Concentration' exists
  expect_named(result_horizontal, c("Position", "Value", "Concentration"), ignore.order = T)
  expect_named(result_vertical, c("Position", "Value", "Concentration"), ignore.order = T)
  
  # Test case 3: Check if column 'Concentration' contains any NAs
  expect_false(any(is.na(result_horizontal$Concentration)),
               "Column 'Concentration' contains NA values")
  expect_false(any(is.na(result_vertical$Concentration)),
               "Column 'Concentration' contains NA values")
  
  # Test case 4: Check if column 'Concentration' contains numerical values
  expect_false(any(!is.numeric(result_horizontal$Concentration)),
               "Column 'Concentration' contains non-numeric values")
  expect_false(any(!is.numeric(result_vertical$Concentration)),
               "Column 'Concentration' contains non-numeric values")
})


test_that("add_concentration() correctly handles user prompts for horizontal axes", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(1:8, collapse = "\n")
  write(input, f)

  # Add concentration levels to well positions via horizontal axes
  expect_equal(add_concentration(test_data, concentration_list = NULL,
                                 ask_concentration_list = T,
                                 direction = "horizontal"),
               add_concentration(test_data, concentration_list = concentration_list_horizontal,
                                 ask_concentration_list = F))

  # Reset connection
  options(mypkg.connection = stdin())
  # Close the file
  close(f)
})

test_that("add_concentration() correctly handles user prompts for vertical axes", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(1:12, collapse = "\n")
  write(input, f)
  
  # Add concentration levels to well positions via vertical axes
  expect_equal(add_concentration(test_data, concentration_list = NULL,
                                 ask_concentration_list = T,
                                 direction = "vertical"),
               add_concentration(test_data, concentration_list = concentration_list_vertical,
                                 ask_concentration_list = F))
  
  # Reset connection
  options(mypkg.connection = stdin())
  # Close the file
  close(f)
})