# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# HeptathlonSeoul1988_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("HeptathlonSeoul1988_df is a data.frame", {
  expect_s3_class(HeptathlonSeoul1988_df, "data.frame")
  expect_false("tbl_df" %in% class(HeptathlonSeoul1988_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 8 columns
test_that("HeptathlonSeoul1988_df has 8 columns", {
  expect_equal(length(HeptathlonSeoul1988_df), 8)
})

# Test 3: Confirm it has exactly 25 rows
test_that("HeptathlonSeoul1988_df has 25 rows", {
  expect_equal(nrow(HeptathlonSeoul1988_df), 25)
})

# Test 4: Confirm column names are correct and in order
test_that("HeptathlonSeoul1988_df has correct column names", {
  expect_named(HeptathlonSeoul1988_df, c(
    "hurdles", "highjump", "shot", "run200m", "longjump", "javelin", "run800m", "score"
  ))
})

# Test 5: Confirm column types are correct
test_that("HeptathlonSeoul1988_df columns have correct types", {
  expect_type(HeptathlonSeoul1988_df$hurdles, "double")
  expect_type(HeptathlonSeoul1988_df$highjump, "double")
  expect_type(HeptathlonSeoul1988_df$shot, "double")
  expect_type(HeptathlonSeoul1988_df$run200m, "double")
  expect_type(HeptathlonSeoul1988_df$longjump, "double")
  expect_type(HeptathlonSeoul1988_df$javelin, "double")
  expect_type(HeptathlonSeoul1988_df$run800m, "double")
  expect_type(HeptathlonSeoul1988_df$score, "integer")
})
