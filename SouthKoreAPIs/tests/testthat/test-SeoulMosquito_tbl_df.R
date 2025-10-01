# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SeoulMosquito_tbl_df

library(testthat)

# Test 1: Confirm the object is a tibble with spec_tbl_df class
test_that("SeoulMosquito_tbl_df is a spec_tbl_df tibble", {
  expect_s3_class(SeoulMosquito_tbl_df, "spec_tbl_df")
  expect_s3_class(SeoulMosquito_tbl_df, "tbl_df")
  expect_s3_class(SeoulMosquito_tbl_df, "tbl")
  expect_s3_class(SeoulMosquito_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 6 columns
test_that("SeoulMosquito_tbl_df has 6 columns", {
  expect_equal(length(SeoulMosquito_tbl_df), 6)
})

# Test 3: Confirm it has exactly 1342 rows
test_that("SeoulMosquito_tbl_df has 1342 rows", {
  expect_equal(nrow(SeoulMosquito_tbl_df), 1342)
})

# Test 4: Confirm column names are correct and in order
test_that("SeoulMosquito_tbl_df has correct column names", {
  expect_named(SeoulMosquito_tbl_df, c(
    "date", "mosquito_Indicator", "rain(mm)", "mean_T(℃)", "min_T(℃)", "max_T(℃)"
  ))
})

# Test 5: Confirm column types are correct
test_that("SeoulMosquito_tbl_df columns have correct types", {
  expect_s3_class(SeoulMosquito_tbl_df$date, "Date")
  expect_type(SeoulMosquito_tbl_df$mosquito_Indicator, "double")
  expect_type(SeoulMosquito_tbl_df$`rain(mm)`, "double")
  expect_type(SeoulMosquito_tbl_df$`mean_T(℃)`, "double")
  expect_type(SeoulMosquito_tbl_df$`min_T(℃)`, "double")
  expect_type(SeoulMosquito_tbl_df$`max_T(℃)`, "double")
})
