# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SeoulH3Data_tbl_df

library(testthat)

# Test 1: Confirm the object is a tibble
test_that("SeoulH3Data_tbl_df is a tibble", {
  expect_s3_class(SeoulH3Data_tbl_df, "tbl_df")
  expect_s3_class(SeoulH3Data_tbl_df, "tbl")
  expect_s3_class(SeoulH3Data_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 2 columns
test_that("SeoulH3Data_tbl_df has 2 columns", {
  expect_equal(length(SeoulH3Data_tbl_df), 2)
})

# Test 3: Confirm it has exactly 1329 rows
test_that("SeoulH3Data_tbl_df has 1329 rows", {
  expect_equal(nrow(SeoulH3Data_tbl_df), 1329)
})

# Test 4: Confirm column names are correct and in order
test_that("SeoulH3Data_tbl_df has correct column names", {
  expect_named(SeoulH3Data_tbl_df, c("name", "value"))
})

# Test 5: Confirm column types are correct
test_that("SeoulH3Data_tbl_df columns have correct types", {
  expect_type(SeoulH3Data_tbl_df$name, "character")
  expect_type(SeoulH3Data_tbl_df$value, "double")
})
