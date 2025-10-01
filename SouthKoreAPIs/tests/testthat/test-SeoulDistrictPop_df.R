# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SeoulDistrictPop_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("SeoulDistrictPop_df is a data.frame", {
  expect_s3_class(SeoulDistrictPop_df, "data.frame")
  expect_false("tbl_df" %in% class(SeoulDistrictPop_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 5 columns
test_that("SeoulDistrictPop_df has 5 columns", {
  expect_equal(length(SeoulDistrictPop_df), 5)
})

# Test 3: Confirm it has exactly 25 rows
test_that("SeoulDistrictPop_df has 25 rows", {
  expect_equal(nrow(SeoulDistrictPop_df), 25)
})

# Test 4: Confirm column names are correct and in order
test_that("SeoulDistrictPop_df has correct column names", {
  expect_named(SeoulDistrictPop_df, c(
    "District", "City", "Pop.2012", "Area", "Founded"
  ))
})

# Test 5: Confirm column types are correct
test_that("SeoulDistrictPop_df columns have correct types", {
  expect_type(SeoulDistrictPop_df$District, "character")
  expect_type(SeoulDistrictPop_df$City, "character")
  expect_type(SeoulDistrictPop_df$`Pop.2012`, "integer")
  expect_type(SeoulDistrictPop_df$Area, "double")
  expect_type(SeoulDistrictPop_df$Founded, "character")
})
