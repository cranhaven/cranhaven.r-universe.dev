library(testthat)

# Test for Cox regression using rgs_for_cox function
test_that("Test rgs_for_cox for Cox regression", {
  # Read data
  data_path <- system.file("extdata", "cox_data.csv", package = "RGS")
  my_data <- read.csv(data_path)

  # Define variables
  u_variables <- c("u1", "u2")
  covariates <- c("l1", "c1")

  dd <<- datadist(my_data)
  options(datadist = "dd")

  # Run the function
  best_combination <- rgs_for_cox(my_data, u_variables, covariates)

  # Create expected output dataframe
  expected_output <- data.frame(
    row.names = as.integer(c(7795)),
    left_cutoff_u1 = c(-0.9858731),
    right_cutoff_u1 = c(1.001609),
    left_cutoff_u2 = c(-0.6598135),
    right_cutoff_u2 = c(0.843893),
    AIC = c(1411.916)
  )

  # Verify that the output results match the expected values
  expect_equal(best_combination, expected_output, tolerance = 1e-3, "Output should match expected values.")

  # Verify that the index matches
  expect_equal(row.names(best_combination)[1], row.names(expected_output)[1], "Index should match.")
})
