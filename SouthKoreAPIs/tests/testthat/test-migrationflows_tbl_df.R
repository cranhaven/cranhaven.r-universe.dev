# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# migrationflows_tbl_df


library(testthat)

# Test 1: Confirm the object is a tibble (tbl_df)
test_that("migrationflows_tbl_df is a tibble", {
  expect_s3_class(migrationflows_tbl_df, "tbl_df")
  expect_s3_class(migrationflows_tbl_df, "tbl")
  expect_s3_class(migrationflows_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 20 columns
test_that("migrationflows_tbl_df has 20 columns", {
  expect_equal(length(migrationflows_tbl_df), 20)
})

# Test 3: Confirm it has exactly 2601 rows
test_that("migrationflows_tbl_df has 2601 rows", {
  expect_equal(nrow(migrationflows_tbl_df), 2601)
})

# Test 4: Confirm column names are correct and in order
test_that("migrationflows_tbl_df has correct column names", {
  expect_named(migrationflows_tbl_df, c(
    "orig", "dest", "year", "flow", "dist_cent", "dist_min", "dist_pw", "contig",
    "orig_pop", "dest_pop", "orig_area", "dest_area", "orig_gdp_pc", "orig_ginc_pc",
    "orig_iinc_pc", "orig_pconsum_pc", "dest_gdp_pc", "dest_ginc_pc", "dest_iinc_pc", "dest_pconsum_pc"
  ))
})

# Test 5: Confirm column types are correct
test_that("migrationflows_tbl_df columns have correct types", {
  expect_type(migrationflows_tbl_df$orig, "character")
  expect_type(migrationflows_tbl_df$dest, "character")
  expect_type(migrationflows_tbl_df$year, "integer")
  expect_type(migrationflows_tbl_df$flow, "integer")
  expect_type(migrationflows_tbl_df$dist_cent, "double")
  expect_type(migrationflows_tbl_df$dist_min, "double")
  expect_type(migrationflows_tbl_df$dist_pw, "double")
  expect_type(migrationflows_tbl_df$contig, "logical")
  expect_type(migrationflows_tbl_df$orig_pop, "double")
  expect_type(migrationflows_tbl_df$dest_pop, "double")

  # For 'orig_area' and 'dest_area', these are units class; underlying type is numeric (double)
  expect_type(as.numeric(migrationflows_tbl_df$orig_area), "double")
  expect_type(as.numeric(migrationflows_tbl_df$dest_area), "double")

  expect_type(migrationflows_tbl_df$orig_gdp_pc, "double")
  expect_type(migrationflows_tbl_df$orig_ginc_pc, "double")
  expect_type(migrationflows_tbl_df$orig_iinc_pc, "double")
  expect_type(migrationflows_tbl_df$orig_pconsum_pc, "double")
  expect_type(migrationflows_tbl_df$dest_gdp_pc, "double")
  expect_type(migrationflows_tbl_df$dest_ginc_pc, "double")
  expect_type(migrationflows_tbl_df$dest_iinc_pc, "double")
  expect_type(migrationflows_tbl_df$dest_pconsum_pc, "double")
})
