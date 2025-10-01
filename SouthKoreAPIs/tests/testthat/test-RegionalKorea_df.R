# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# RegionalKorea_df

library(testthat)

# Test 1: Confirm the object is a data.frame (not a tibble)
test_that("RegionalKorea_df is a data.frame", {
  expect_s3_class(RegionalKorea_df, "data.frame")
  expect_false("tbl_df" %in% class(RegionalKorea_df))  # Ensure it's not a tibble
})

# Test 2: Confirm it has exactly 23 columns
test_that("RegionalKorea_df has 23 columns", {
  expect_equal(length(RegionalKorea_df), 23)
})

# Test 3: Confirm it has exactly 268 rows
test_that("RegionalKorea_df has 268 rows", {
  expect_equal(nrow(RegionalKorea_df), 268)
})

# Test 4: Confirm column names are correct and in order
test_that("RegionalKorea_df has correct column names", {
  expect_named(RegionalKorea_df, c(
    "id", "metro", "region", "type", "grdp", "regpop", "popgrowth", "eq5d",
    "deaths", "drink", "hdrink", "smoke", "aged", "divorce", "medrate", "gcomp",
    "vehipc", "accpv", "dumppc", "stratio", "deathrate", "pctmale", "accpc"
  ))
})

# Test 5: Confirm column types are correct
test_that("RegionalKorea_df columns have correct types", {
  expect_type(RegionalKorea_df$id, "integer")
  expect_type(RegionalKorea_df$metro, "character")
  expect_type(RegionalKorea_df$region, "character")
  expect_type(RegionalKorea_df$type, "integer")
  expect_type(RegionalKorea_df$grdp, "double")
  expect_type(RegionalKorea_df$regpop, "double")
  expect_type(RegionalKorea_df$popgrowth, "double")
  expect_type(RegionalKorea_df$eq5d, "double")
  expect_type(RegionalKorea_df$deaths, "double")
  expect_type(RegionalKorea_df$drink, "double")
  expect_type(RegionalKorea_df$hdrink, "double")
  expect_type(RegionalKorea_df$smoke, "double")
  expect_type(RegionalKorea_df$aged, "double")
  expect_type(RegionalKorea_df$divorce, "double")
  expect_type(RegionalKorea_df$medrate, "double")
  expect_type(RegionalKorea_df$gcomp, "double")
  expect_type(RegionalKorea_df$vehipc, "double")
  expect_type(RegionalKorea_df$accpv, "double")
  expect_type(RegionalKorea_df$dumppc, "double")
  expect_type(RegionalKorea_df$stratio, "double")
  expect_type(RegionalKorea_df$deathrate, "double")
  expect_type(RegionalKorea_df$pctmale, "double")
  expect_type(RegionalKorea_df$accpc, "double")
})
