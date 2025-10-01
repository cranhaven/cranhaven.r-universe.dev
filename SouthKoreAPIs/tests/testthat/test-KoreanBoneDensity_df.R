# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# KoreanBoneDensity_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("KoreanBoneDensity_df is a data.frame", {
  expect_s3_class(KoreanBoneDensity_df, "data.frame")
  expect_false("tbl_df" %in% class(KoreanBoneDensity_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 7 columns
test_that("KoreanBoneDensity_df has 7 columns", {
  expect_equal(length(KoreanBoneDensity_df), 7)
})

# Test 3: Confirm it has exactly 969 rows
test_that("KoreanBoneDensity_df has 969 rows", {
  expect_equal(nrow(KoreanBoneDensity_df), 969)
})

# Test 4: Confirm column names are correct and in order
test_that("KoreanBoneDensity_df has correct column names", {
  expect_named(KoreanBoneDensity_df, c(
    "Sex", "Age", "Height", "Weight", "LumbarBMD", "HipBMD", "NeckBMD"
  ))
})

# Test 5: Confirm column types are correct
test_that("KoreanBoneDensity_df columns have correct types", {
  expect_type(KoreanBoneDensity_df$Sex, "integer")     # factor internally integer
  expect_type(KoreanBoneDensity_df$Age, "integer")
  expect_type(KoreanBoneDensity_df$Height, "double")   # numeric is double in R
  expect_type(KoreanBoneDensity_df$Weight, "double")
  expect_type(KoreanBoneDensity_df$LumbarBMD, "double")
  expect_type(KoreanBoneDensity_df$HipBMD, "double")
  expect_type(KoreanBoneDensity_df$NeckBMD, "double")
})
