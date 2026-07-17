file_list <- c("BMA_grp1_T0", "bma_grp2_T0")

# Test case 1
test_that("ask_group_list() correctly stores user inputs for group names'", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(c("Group A", "Group B", "Group C"), collapse = "\n")
  write(input, f)
  
  # Test case 1: Single group
  result_one_group <- ask_group_list(file_list[1])
  expect_equal(result_one_group, list(grp1 = "Group A"))
  
  # Test case 1: Multiple groups
  result_multiple_groups <- ask_group_list(file_list)
  expect_equal(result_multiple_groups, list(grp1 = "Group B",
                                            grp2 = "Group C"))
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
