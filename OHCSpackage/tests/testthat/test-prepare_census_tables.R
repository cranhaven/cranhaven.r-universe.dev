library(testthat)
library(OHCSpackage)

test_that("prepare_census_tables() works correctly", {

  # Test that output directory is created
  temp_dir <- tempdir()
  output_path <- paste0(temp_dir, "\\", "outputs\\")

    prepare_census_tables(system.file("extdata/census_data", package = "OHCSpackage"), output_path, write_output = TRUE)
    expect_true(dir.exists(temp_dir))

  # Test that output files are created
  expect_true(file.exists(file.path(file_path, "Age_characteristics.xlsx")))
  expect_true(file.exists(file.path(file_path, "Household_Characteristics.xlsx")))
})


