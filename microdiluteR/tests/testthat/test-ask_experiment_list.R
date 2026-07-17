file_list <- c("BMA_exp1_T0", "bma_exp2_T0")

# Test case 1
test_that("ask_experiment_list() correctly stores user inputs for experiment names'", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(c("Experiment A", "Experiment B", "Experiment C"), collapse = "\n")
  write(input, f)
  
  # Test case 1: Single experiment
  result_one_experiment <- ask_experiment_list(file_list[1])
  expect_equal(result_one_experiment, list(exp1 = "Experiment A"))
  
  # Test case 1: Multiple experiments
  result_multiple_experiments <- ask_experiment_list(file_list)
  expect_equal(result_multiple_experiments, list(exp1 = "Experiment B",
                                                 exp2 = "Experiment C"))
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
