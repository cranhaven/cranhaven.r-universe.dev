# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# KoreanSocialSurvey_tbl_df

library(testthat)

# Test 1: Confirm the object is a tibble
test_that("KoreanSocialSurvey_tbl_df is a tibble", {
  expect_s3_class(KoreanSocialSurvey_tbl_df, "tbl_df")
  expect_s3_class(KoreanSocialSurvey_tbl_df, "tbl")
  expect_s3_class(KoreanSocialSurvey_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 13 columns
test_that("KoreanSocialSurvey_tbl_df has 13 columns", {
  expect_equal(length(KoreanSocialSurvey_tbl_df), 13)
})

# Test 3: Confirm it has exactly 1123 rows
test_that("KoreanSocialSurvey_tbl_df has 1123 rows", {
  expect_equal(nrow(KoreanSocialSurvey_tbl_df), 1123)
})

# Test 4: Confirm column names are correct and in order
test_that("KoreanSocialSurvey_tbl_df has correct column names", {
  expect_named(KoreanSocialSurvey_tbl_df, c(
    "year", "respid", "age", "female", "employed", "unived",
    "netuse", "ideo", "si_gbh", "satisfin", "fp_mord", "fpcat", "cntryaffq"
  ))
})

# Test 5: Confirm column types are correct
test_that("KoreanSocialSurvey_tbl_df columns have correct types", {
  expect_type(KoreanSocialSurvey_tbl_df$year, "double")
  expect_type(KoreanSocialSurvey_tbl_df$respid, "double")
  expect_type(KoreanSocialSurvey_tbl_df$age, "double")
  expect_type(KoreanSocialSurvey_tbl_df$female, "double")
  expect_type(KoreanSocialSurvey_tbl_df$employed, "double")
  expect_type(KoreanSocialSurvey_tbl_df$unived, "double")
  expect_type(KoreanSocialSurvey_tbl_df$netuse, "double")
  expect_type(KoreanSocialSurvey_tbl_df$ideo, "double")
  expect_type(KoreanSocialSurvey_tbl_df$si_gbh, "double")
  expect_type(KoreanSocialSurvey_tbl_df$satisfin, "double")
  expect_type(KoreanSocialSurvey_tbl_df$fp_mord, "double")
  expect_type(KoreanSocialSurvey_tbl_df$fpcat, "character")
  expect_type(KoreanSocialSurvey_tbl_df$cntryaffq, "character")
})
