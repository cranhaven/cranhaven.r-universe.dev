# Define test data
monotonic_data <- data.frame(
  Timepoint = rep(1:4, each = 3),
  Value = 1:12,
  Position = rep(letters[1:3], times = 4),
  Validity = "valid"
)
nonmonotonic_data <- data.frame(
  Timepoint = rep(1:4, each = 3),
  Value = c(4, 2, 3, 4, 5, 5, 4, 3, 2, 4, 1, 2),
  Position = rep(letters[1:3], times = 4),
  Validity = "valid"
)
grouped_nonmonotonic_data <- data.frame(
  Timepoint = rep(rep(1:4, each = 3),2),
  Value = rep(c(4, 2, 3, 4, 5, 5, 4, 3, 2, 4, 1, 2),2),
  Position = rep(rep(letters[1:3], times = 4),2),
  Validity = "valid",
  Group = rep(LETTERS[1:2], each = 12)
)

# Define test cases
test_that("check_well_positions() correctly handles different scenarios of sample re-validation", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(rep("n", 10),
                 collapse = "\n")
  write(input, f)
  
  # Test case 1: Basic input
  expect_s3_class(check_well_positions(monotonic_data), "data.frame")
  expect_true(is.list(check_well_positions(nonmonotonic_data)), "Output should be a list")
  
  # Test case 2: Validity assignment
  result <- check_well_positions(nonmonotonic_data)
  expect_equal(unique(result$non_monotonic_subset$Validity), "invalid")
  expect_equal(unique(result$modified_input_data$Validity), c("valid","invalid"))
  
  # Test Case 3: Grouping variable handling
  expect_true(is.list(check_well_positions(grouped_nonmonotonic_data,
                                 grouping = c("Group","Position"))), "Output should be a list")
  
  # Reset connection
  options(mypkg.connection = stdin())
  # Close the file
  close(f)
})
