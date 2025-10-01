# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# NFIColumnNames_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("NFIColumnNames_df is a data.frame", {
  expect_s3_class(NFIColumnNames_df, "data.frame")
  expect_false("tbl_df" %in% class(NFIColumnNames_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 3 columns
test_that("NFIColumnNames_df has 3 columns", {
  expect_equal(length(NFIColumnNames_df), 3)
})

# Test 3: Confirm it has exactly 174 rows
test_that("NFIColumnNames_df has 174 rows", {
  expect_equal(nrow(NFIColumnNames_df), 174)
})

# Test 4: Confirm column names are correct and in order
test_that("NFIColumnNames_df has correct column names", {
  expect_named(NFIColumnNames_df, c("Korean_Column_Name", "English_Name", "Column_Name"))
})

# Test 5: Confirm all columns are character type
test_that("NFIColumnNames_df columns have correct types", {
  expect_type(NFIColumnNames_df$Korean_Column_Name, "character")
  expect_type(NFIColumnNames_df$English_Name, "character")
  expect_type(NFIColumnNames_df$Column_Name, "character")
})
