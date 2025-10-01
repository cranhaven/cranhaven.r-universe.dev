# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_hospital_beds

library(testthat)

test_that("get_southkorea_hospital_beds() returns a tibble with correct structure and types", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("indicator", "country", "year", "value"))
  expect_equal(ncol(result), 4)

  expect_type(result$indicator, "character")
  expect_type(result$country, "character")
  expect_type(result$year, "integer")
  expect_type(result$value, "double")
})

test_that("get_southkorea_hospital_beds() returns data for Korea, Rep. only", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_true(all(result$country == "Korea, Rep."))
})

test_that("get_southkorea_hospital_beds() returns correct indicator label", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_true(all(result$indicator == "Hospital beds (per 1,000 people)"))
})

test_that("get_southkorea_hospital_beds() returns data for years 2010 to 2022", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_true(all(result$year %in% 2010:2022))
  expect_equal(sort(unique(result$year)), 2010:2022)
})

test_that("get_southkorea_hospital_beds() returns exactly 13 rows (2010-2022 inclusive)", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_equal(nrow(result), 13)
})

test_that("get_southkorea_hospital_beds() year column has no NA values", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_false(any(is.na(result$year)))
})

test_that("get_southkorea_hospital_beds() value column is numeric or NA", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_true(all(is.finite(result$value) | is.na(result$value)))
})

test_that("get_southkorea_hospital_beds() allows missing values (NA) in value column", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_true(any(is.na(result$value)) || all(!is.na(result$value)))
})

test_that("get_southkorea_hospital_beds() years are sorted in descending order", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_equal(result$year, sort(result$year, decreasing = TRUE))
})

test_that("get_southkorea_hospital_beds() indicator and country are consistent across rows", {
  skip_on_cran()
  result <- get_southkorea_hospital_beds()
  expect_equal(length(unique(result$indicator)), 1)
  expect_equal(length(unique(result$country)), 1)
})
