# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SouthKoreaCovid19_tbl_df

library(testthat)

# Test 1: Confirm the object is a tibble
test_that("SouthKoreaCovid19_tbl_df is a tibble", {
  expect_s3_class(SouthKoreaCovid19_tbl_df, "tbl_df")
  expect_s3_class(SouthKoreaCovid19_tbl_df, "tbl")
  expect_s3_class(SouthKoreaCovid19_tbl_df, "data.frame")
})

# Test 2: Confirm it has exactly 11 columns
test_that("SouthKoreaCovid19_tbl_df has 11 columns", {
  expect_equal(length(SouthKoreaCovid19_tbl_df), 11)
})

# Test 3: Confirm it has exactly 244 rows
test_that("SouthKoreaCovid19_tbl_df has 244 rows", {
  expect_equal(nrow(SouthKoreaCovid19_tbl_df), 244)
})

# Test 4: Confirm column names are correct and in order
test_that("SouthKoreaCovid19_tbl_df has correct column names", {
  expect_named(SouthKoreaCovid19_tbl_df, c(
    "n_covid1", "Morbidity", "high_sch_p", "Healthcare_access", "diff_sd",
    "Crowding", "Migration", "Health_behavior", "x", "y", "ln_total"
  ))
})

# Test 5: Confirm column types are correct (all numeric/double)
test_that("SouthKoreaCovid19_tbl_df columns have correct types", {
  expect_type(SouthKoreaCovid19_tbl_df$n_covid1, "double")
  expect_type(SouthKoreaCovid19_tbl_df$Morbidity, "double")
  expect_type(SouthKoreaCovid19_tbl_df$high_sch_p, "double")
  expect_type(SouthKoreaCovid19_tbl_df$Healthcare_access, "double")
  expect_type(SouthKoreaCovid19_tbl_df$diff_sd, "double")
  expect_type(SouthKoreaCovid19_tbl_df$Crowding, "double")
  expect_type(SouthKoreaCovid19_tbl_df$Migration, "double")
  expect_type(SouthKoreaCovid19_tbl_df$Health_behavior, "double")
  expect_type(SouthKoreaCovid19_tbl_df$x, "double")
  expect_type(SouthKoreaCovid19_tbl_df$y, "double")
  expect_type(SouthKoreaCovid19_tbl_df$ln_total, "double")
})
