# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# demographicsKR_tbl_df

library(testthat)

# Test 1: Confirm the object is a tibble/data.frame (including spec_tbl_df)
test_that("demographicsKR_tbl_df is a tibble with spec_tbl_df class", {
  expect_s3_class(demographicsKR_tbl_df, "spec_tbl_df")
  expect_s3_class(demographicsKR_tbl_df, "tbl_df")
  expect_s3_class(demographicsKR_tbl_df, "tbl")
  expect_s3_class(demographicsKR_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 12 columns
test_that("demographicsKR_tbl_df has 12 columns", {
  expect_equal(length(demographicsKR_tbl_df), 12)
})

# Test 3: Confirm it has exactly 4860 rows
test_that("demographicsKR_tbl_df has 4860 rows", {
  expect_equal(nrow(demographicsKR_tbl_df), 4860)
})

# Test 4: Confirm column names are correct and in order
test_that("demographicsKR_tbl_df has correct column names", {
  expect_named(demographicsKR_tbl_df, c(
    "Date", "Region", "Birth", "Birth_rate", "Death", "Death_rate",
    "Divorce", "Divorce_rate", "Marriage", "Marriage_rate",
    "Natural_growth", "Natural_growth_rate"
  ))
})

# Test 5: Confirm column types are correct
test_that("demographicsKR_tbl_df columns have correct types", {
  expect_type(demographicsKR_tbl_df$Date, "character")
  expect_type(demographicsKR_tbl_df$Region, "character")

  numeric_cols <- c(
    "Birth", "Birth_rate", "Death", "Death_rate", "Divorce", "Divorce_rate",
    "Marriage", "Marriage_rate", "Natural_growth", "Natural_growth_rate"
  )
  for (col in numeric_cols) {
    expect_type(demographicsKR_tbl_df[[col]], "double")
  }
})
