# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_holidays

library(testthat)

test_that("get_southkorea_holidays() returns tibble with correct structure and types", {
  skip_on_cran()
  result <- get_southkorea_holidays(2025)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("date", "local_name", "name"))
  expect_equal(ncol(result), 3)

  expect_s3_class(result$date, "Date")
  expect_type(result$local_name, "character")
  expect_type(result$name, "character")

  expect_gt(nrow(result), 0) # should return some holidays
})

test_that("get_southkorea_holidays() dates belong to the requested year", {
  skip_on_cran()
  year <- 2025
  result <- get_southkorea_holidays(year)
  expect_true(all(format(result$date, "%Y") == as.character(year)))
})

test_that("get_southkorea_holidays() errors on invalid year inputs", {
  expect_error(get_southkorea_holidays("not_a_year"))
  expect_error(get_southkorea_holidays(999))   # Likely invalid year (too far past)
  expect_error(get_southkorea_holidays(3000))  # Future year, possibly no data
})

test_that("get_southkorea_holidays() returns consistent columns across calls", {
  skip_on_cran()
  res1 <- get_southkorea_holidays(2024)
  res2 <- get_southkorea_holidays(2025)

  expect_named(res1, c("date", "local_name", "name"))
  expect_named(res2, c("date", "local_name", "name"))
})

test_that("get_southkorea_holidays() returns a reasonable number of holidays", {
  skip_on_cran()
  result <- get_southkorea_holidays(2025)
  # Typical number of holidays for South Korea is about 15 for recent years
  expect_true(nrow(result) >= 10)
  expect_true(nrow(result) <= 25)
})
