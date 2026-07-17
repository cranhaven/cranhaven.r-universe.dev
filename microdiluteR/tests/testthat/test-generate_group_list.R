# Define test data
group_names <- LETTERS[1:2]
file_list <- c("BMA_grp1_T0", "bma_grp2_T1")
file_as_list <- list("BMA_grp1_T0", "BMA_grp2_T0")
file_no_identifiers <- c("BMA_T0", "bma_T1")

test_that("generate_group_list() correctly generates a list of group names", {
  # Test case 1: Valid input
  expect_equal(generate_group_list(group_names, file_list),
               list(grp1 = "A", grp2 = "B"))
  expect_equal(generate_group_list(group_names, file_as_list),
               list(grp1 = "A", grp2 = "B"))
  
  # Test 2: Invalid input
  expect_error(generate_group_list(group_names, file_no_identifiers),
               "Number of group IDs must match the number of identifiers used in file names.")
})