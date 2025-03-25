library(testthat)
library(OHCSpackage)

test_that("prepare_core_housing_needs_tables() works correctly", {
  input <- system.file("extdata/corehousing/CoreHousingNeeds.csv", package = "OHCSpackage")
  temp_dir <- tempdir()
  output <- file.path(temp_dir, "outputs", "combined_housing_out.csv")

  prepare_core_housing_needs_tables(input, output, write_output = TRUE)
  # Test that output file is created
  expect_true(file.exists(output))
  # Clean up
  file.remove(input)
  file.remove(output)
})
