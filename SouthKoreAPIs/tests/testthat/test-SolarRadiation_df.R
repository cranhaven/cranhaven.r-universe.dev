# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SolarRadiation_df

library(testthat)

# Test 1: Confirm the object is a data frame (not a tibble)
test_that("SolarRadiation_df is a data.frame", {
  expect_s3_class(SolarRadiation_df, "data.frame")
  expect_false("tbl_df" %in% class(SolarRadiation_df))
})

# Test 2: Confirm it has exactly 4 columns
test_that("SolarRadiation_df has 4 columns", {
  expect_equal(length(SolarRadiation_df), 4)
})

# Test 3: Confirm it has exactly 696 rows
test_that("SolarRadiation_df has 696 rows", {
  expect_equal(nrow(SolarRadiation_df), 696)
})

# Test 4: Confirm column names are correct and in order
test_that("SolarRadiation_df has correct column names", {
  expect_named(SolarRadiation_df, c("Date", "Seoul", "Daegu", "Busan"))
})

# Test 5: Confirm column types are correct
test_that("SolarRadiation_df columns have correct types", {
  expect_s3_class(SolarRadiation_df$Date, "POSIXct")
  expect_type(SolarRadiation_df$Seoul, "double")
  expect_type(SolarRadiation_df$Daegu, "double")
  expect_type(SolarRadiation_df$Busan, "double")
})
