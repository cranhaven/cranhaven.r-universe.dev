# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# KoreanElection2017_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("KoreanElection2017_df is a data.frame", {
  expect_s3_class(KoreanElection2017_df, "data.frame")
  expect_false("tbl_df" %in% class(KoreanElection2017_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 9 columns
test_that("KoreanElection2017_df has 9 columns", {
  expect_equal(length(KoreanElection2017_df), 9)
})

# Test 3: Confirm it has exactly 1250 rows
test_that("KoreanElection2017_df has 1250 rows", {
  expect_equal(nrow(KoreanElection2017_df), 1250)
})

# Test 4: Confirm column names are correct and in order
test_that("KoreanElection2017_df has correct column names", {
  expect_named(KoreanElection2017_df, c(
    "PrecinctCode", "CityCode", "CandidateName", "AveAge",
    "AveYearEdu", "AveHousePrice", "AveInsurance", "VoteRate", "NumVote"
  ))
})

# Test 5: Confirm column types are correct
test_that("KoreanElection2017_df columns have correct types", {
  expect_type(KoreanElection2017_df$PrecinctCode, "integer")
  expect_type(KoreanElection2017_df$CityCode, "integer")
  expect_type(KoreanElection2017_df$CandidateName, "integer")
  expect_type(KoreanElection2017_df$AveAge, "double")
  expect_type(KoreanElection2017_df$AveYearEdu, "double")
  expect_type(KoreanElection2017_df$AveHousePrice, "double")
  expect_type(KoreanElection2017_df$AveInsurance, "integer")
  expect_type(KoreanElection2017_df$VoteRate, "double")
  expect_type(KoreanElection2017_df$NumVote, "integer")
})
