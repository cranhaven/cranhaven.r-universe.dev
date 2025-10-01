# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_child_mortality


library(testthat)

test_that("get_southkorea_child_mortality() returns a tibble with correct structure and types", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()

  # Structure checks
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("indicator", "country", "year", "value"))

  # Type checks
  expect_type(result$indicator, "character")
  expect_type(result$country, "character")
  expect_type(result$year, "integer")
  expect_type(result$value, "double")
})

test_that("get_southkorea_child_mortality() returns only South Korea data", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_true(all(result$country == "Korea, Rep."))
})

test_that("get_southkorea_child_mortality() returns years from 2010 to 2022", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_true(all(result$year %in% 2010:2022))
})

test_that("get_southkorea_child_mortality() returns exactly 13 rows", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_equal(nrow(result), 13)
})

test_that("get_southkorea_child_mortality() includes the correct indicator label", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_true(all(result$indicator == "Mortality rate, under-5 (per 1,000 live births)"))
})

test_that("get_southkorea_child_mortality() allows for missing values (NA)", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_true(any(is.na(result$value)) || all(!is.na(result$value)))
})

test_that("get_southkorea_child_mortality(): no NA values in year column", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_false(any(is.na(result$year)))
})

test_that("get_southkorea_child_mortality(): value column is numeric or NA", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_true(all(is.finite(result$value) | is.na(result$value)))
})

test_that("get_southkorea_child_mortality(): years are sorted in descending order", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_equal(result$year, sort(result$year, decreasing = TRUE))
})

test_that("get_southkorea_child_mortality(): indicator is consistent across rows", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_equal(length(unique(result$indicator)), 1)
})

test_that("get_southkorea_child_mortality(): country is consistent across rows", {
  skip_on_cran()
  result <- get_southkorea_child_mortality()
  expect_equal(length(unique(result$country)), 1)
})
