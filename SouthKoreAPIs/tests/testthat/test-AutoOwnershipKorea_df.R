# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# AutoOwnershipKorea_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("AutoOwnershipKorea_df is a data.frame", {
  expect_s3_class(AutoOwnershipKorea_df, "data.frame")
  expect_false("tbl_df" %in% class(AutoOwnershipKorea_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 5 columns
test_that("AutoOwnershipKorea_df has 5 columns", {
  expect_equal(length(AutoOwnershipKorea_df), 5)
})

# Test 3: Confirm it has exactly 10 rows
test_that("AutoOwnershipKorea_df has 10 rows", {
  expect_equal(nrow(AutoOwnershipKorea_df), 10)
})

# Test 4: Confirm column names are correct and in order
test_that("AutoOwnershipKorea_df has correct column names", {
  expect_named(AutoOwnershipKorea_df, c("Year", "AO", "GNP", "CP", "OP"))
})

# Test 5: Confirm column types are correct
test_that("AutoOwnershipKorea_df columns have correct types", {
  expect_type(AutoOwnershipKorea_df$Year, "double")
  expect_type(AutoOwnershipKorea_df$AO, "double")
  expect_type(AutoOwnershipKorea_df$GNP, "double")
  expect_type(AutoOwnershipKorea_df$CP, "double")
  expect_type(AutoOwnershipKorea_df$OP, "double")
})
