# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SouthKoreaBirths_tbl_df

library(testthat)

# Test 1: Confirm the object is a tibble
test_that("SouthKoreaBirths_tbl_df is a tibble", {
  expect_s3_class(SouthKoreaBirths_tbl_df, "tbl_df")
  expect_s3_class(SouthKoreaBirths_tbl_df, "tbl")
  expect_s3_class(SouthKoreaBirths_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 7 columns
test_that("SouthKoreaBirths_tbl_df has 7 columns", {
  expect_equal(length(SouthKoreaBirths_tbl_df), 7)
})

# Test 3: Confirm it has exactly 1872 rows
test_that("SouthKoreaBirths_tbl_df has 1872 rows", {
  expect_equal(nrow(SouthKoreaBirths_tbl_df), 1872)
})

# Test 4: Confirm column names are correct and in order
test_that("SouthKoreaBirths_tbl_df has correct column names", {
  expect_named(SouthKoreaBirths_tbl_df, c(
    "age", "region", "time", "births", "popn", "gdp_pc_2023", "dens_2020"
  ))
})

# Test 5: Confirm column types are correct
test_that("SouthKoreaBirths_tbl_df columns have correct types", {
  expect_type(SouthKoreaBirths_tbl_df$age, "character")
  expect_type(SouthKoreaBirths_tbl_df$region, "integer")     # factor internally integer
  expect_type(SouthKoreaBirths_tbl_df$time, "integer")
  expect_type(SouthKoreaBirths_tbl_df$births, "integer")
  expect_type(SouthKoreaBirths_tbl_df$popn, "integer")
  expect_type(SouthKoreaBirths_tbl_df$gdp_pc_2023, "double")
  expect_type(SouthKoreaBirths_tbl_df$dens_2020, "character")
})
