# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# KPopIdols_tbl_df

library(testthat)

test_that("KPopIdols_tbl_df is a spec_tbl_df tibble", {
  expect_s3_class(KPopIdols_tbl_df, "spec_tbl_df")
  expect_s3_class(KPopIdols_tbl_df, "tbl_df")
  expect_s3_class(KPopIdols_tbl_df, "tbl")
  expect_s3_class(KPopIdols_tbl_df, "data.frame")
})

test_that("KPopIdols_tbl_df has 12 columns", {
  expect_equal(length(KPopIdols_tbl_df), 12)
})

test_that("KPopIdols_tbl_df has 1666 rows", {
  expect_equal(nrow(KPopIdols_tbl_df), 1666)
})

test_that("KPopIdols_tbl_df has correct column names", {
  expect_named(KPopIdols_tbl_df, c(
    "Stage Name Stage Name", "Full Name Full Name", "Korean Name Korean Name",
    "K. Stage Name K. Stage Name", "Date of Birth Date of Birth", "Group Group",
    "Country Country", "Height Height", "Weight Weight",
    "Birthplace Birthplace", "Gender Gender", "Instagram Instagram"
  ))
})

test_that("KPopIdols_tbl_df columns have correct types", {
  expect_type(KPopIdols_tbl_df$`Stage Name Stage Name`, "character")
  expect_type(KPopIdols_tbl_df$`Full Name Full Name`, "character")
  expect_type(KPopIdols_tbl_df$`Korean Name Korean Name`, "character")
  expect_type(KPopIdols_tbl_df$`K. Stage Name K. Stage Name`, "character")
  expect_type(KPopIdols_tbl_df$`Date of Birth Date of Birth`, "character")
  expect_type(KPopIdols_tbl_df$`Group Group`, "character")
  expect_type(KPopIdols_tbl_df$`Country Country`, "character")
  expect_type(KPopIdols_tbl_df$`Height Height`, "double")
  expect_type(KPopIdols_tbl_df$`Weight Weight`, "double")
  expect_type(KPopIdols_tbl_df$`Birthplace Birthplace`, "character")
  expect_type(KPopIdols_tbl_df$`Gender Gender`, "character")
  expect_type(KPopIdols_tbl_df$`Instagram Instagram`, "character")
})
