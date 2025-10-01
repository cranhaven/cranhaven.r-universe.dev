# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_unemployment


library(testthat)

test_that("get_southkorea_unemployment() returns a tibble with correct structure and types", {
  skip_on_cran()
  result <- get_southkorea_unemployment()

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("indicator", "country", "year", "value"))
  expect_equal(ncol(result), 4)

  expect_type(result$indicator, "character")
  expect_type(result$country, "character")
  expect_type(result$year, "integer")
  expect_type(result$value, "double")  # numeric (double) is expected for rates

})

test_that("get_southkorea_unemployment() returns data only for Korea, Rep.", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_true(all(result$country == "Korea, Rep."))
})

test_that("get_southkorea_unemployment() returns correct indicator label", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expected_indicator <- "Unemployment, total (% of total labor force) (modeled ILO estimate)"
  expect_true(all(result$indicator == expected_indicator))
})

test_that("get_southkorea_unemployment() returns data for years 2010 to 2022", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_true(all(result$year %in% 2010:2022))
  expect_equal(sort(unique(result$year)), 2010:2022)
})

test_that("get_southkorea_unemployment() returns exactly 13 rows (one per year 2010-2022)", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_equal(nrow(result), 13)
})

test_that("get_southkorea_unemployment() year column has no missing values", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_false(any(is.na(result$year)))
})

test_that("get_southkorea_unemployment() value column has no missing values and is numeric", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_false(any(is.na(result$value)))
  expect_true(is.numeric(result$value))
})

test_that("get_southkorea_unemployment() years are sorted in descending order", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_equal(result$year, sort(result$year, decreasing = TRUE))
})

test_that("get_southkorea_unemployment() indicator and country columns have consistent values", {
  skip_on_cran()
  result <- get_southkorea_unemployment()
  expect_equal(length(unique(result$indicator)), 1)
  expect_equal(length(unique(result$country)), 1)
})
