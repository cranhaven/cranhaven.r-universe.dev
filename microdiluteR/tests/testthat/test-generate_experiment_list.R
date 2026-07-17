# Define test data
experiment_names <- LETTERS[1:2]
file_list <- c("BMA_exp1_T0", "bma_exp2_T1")
file_as_list <- list("BMA_exp1_T0", "BMA_exp1_T0")
file_no_identifiers <- c("BMA_T0", "bma_T1")

test_that("generate_experiment_list() correctly generates a list of experiment names", {
  # Test case 1: Valid input
  expect_equal(generate_experiment_list(experiment_names, file_list),
               list(exp1 = "A", exp2 = "B"))

  # Test 2: Invalid input
  expect_error(generate_experiment_list(experiment_names, file_as_list),
               "Number of experiment names must match the number of identifiers used in file names.")
  expect_error(generate_experiment_list(experiment_names, file_no_identifiers),
               "Number of experiment names must match the number of identifiers used in file names.")
})