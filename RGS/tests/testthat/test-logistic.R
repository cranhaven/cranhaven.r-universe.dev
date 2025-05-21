library(testthat)

# Test logistic regression output
test_that("Test rgs_for_logistic for logisti regression", {
  # Read data
  data_path <- system.file("extdata", "logistic_data.csv", package = "RGS")
  my_data <- read.csv(data_path)

  # Define variables
  u_variables <- c("u1", "u2")
  covariates <- c("l1", "c1")

  dd <<- datadist(my_data)
  options(datadist = "dd")

  # Run the function
  best_combination <- rgs_for_logistic(my_data, u_variables, covariates)

  # Create the expected output dataframe
  expected_output <- data.frame(
    row.names = as.integer(c(4269)),
    left_cutoff_u1 = c(-0.3834946),
    right_cutoff_u1 = c(0.4222876),
    left_cutoff_u2 = c(-0.4855637),
    right_cutoff_u2 = c(0.4689046),
    AIC = c(131.5878)
  )

  # Verify that the output results match the expected values
  expect_equal(best_combination, expected_output, tolerance = 1e-3, "Output should match expected values.")

  # Verify that the index matches
  expect_equal(row.names(best_combination)[1], row.names(expected_output)[1], "Index should match.")
})
