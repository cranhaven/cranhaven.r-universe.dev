# Generate test data
test_data <- data.frame(
  Group = rep(LETTERS[1:3], each = 2),
  Experiment = rep(paste0("Exp", 1:3), each = 2),
  Position = rep(paste0(LETTERS[1:3], 1), each = 2),
  Value = c(5, 10, -3, 8, 8, 3),
  Timepoint = rep(paste0("T", 0:1), times = 3),
  Validity = c(rep("valid", 3), "invalid", rep("valid", 2))
)

test_that("subtract_T0() modifies data correctly", {
  
  # Call function
  modified_data <- subtract_T0(test_data)
  
  # Test case 1: Check if Timepoint "T0" rows are removed
  expect_false("T0" %in% modified_data$Timepoint)
  
  # Test case 2: Check if negative values are replaced with 0
  expect_equal(all(modified_data$Value >= 0), TRUE)
  
  # Test case 3: Check if rows with invalid Validity are filtered out
  expect_false("invalid" %in% modified_data$Validity)
  
  # Test case 4: Test modifying data with different grouping variables
  modified_data <- subtract_T0(test_data, grouping = c("Group", "Experiment"))
  
  # Test case 5: Check if the grouping variables are preserved
  expect_true(unique(c("Group", "Experiment") %in% names(modified_data)))
})
