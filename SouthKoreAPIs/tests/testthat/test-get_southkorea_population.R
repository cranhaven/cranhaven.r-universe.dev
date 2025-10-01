# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_population

library(testthat)

test_that("get_southkorea_population() returns a tibble with correct structure and types", {
  skip_on_cran()
  result <- get_southkorea_population()

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("indicator", "country", "year", "value", "value_label"))
  expect_equal(ncol(result), 5)

  expect_type(result$indicator, "character")
  expect_type(result$country, "character")
  expect_type(result$year, "integer")
  expect_type(result$value, "integer")
  expect_type(result$value_label, "character")
})

test_that("get_southkorea_population() returns data only for Korea, Rep.", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_true(all(result$country == "Korea, Rep."))
})

test_that("get_southkorea_population() returns correct indicator label", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_true(all(result$indicator == "Population, total"))
})

test_that("get_southkorea_population() returns data for years 2010 to 2022", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_true(all(result$year %in% 2010:2022))
  expect_equal(sort(unique(result$year)), 2010:2022)
})

test_that("get_southkorea_population() returns exactly 13 rows (one per year 2010-2022)", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_equal(nrow(result), 13)
})

test_that("get_southkorea_population() year column has no missing values", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_false(any(is.na(result$year)))
})

test_that("get_southkorea_population() value column has no missing values and is numeric", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_false(any(is.na(result$value)))
  expect_true(is.numeric(result$value))
})

test_that("get_southkorea_population() value_label matches formatted value column", {
  skip_on_cran()
  result <- get_southkorea_population()
  # Remove commas and convert to numeric to compare
  numeric_label <- as.numeric(gsub(",", "", result$value_label))
  expect_equal(numeric_label, result$value)
})

test_that("get_southkorea_population() years are sorted in descending order", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_equal(result$year, sort(result$year, decreasing = TRUE))
})

test_that("get_southkorea_population() indicator and country columns have consistent values", {
  skip_on_cran()
  result <- get_southkorea_population()
  expect_equal(length(unique(result$indicator)), 1)
  expect_equal(length(unique(result$country)), 1)
})
