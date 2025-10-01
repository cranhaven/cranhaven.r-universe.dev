# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# KOSPI200_list

library(testthat)

# Test 1: Confirm the object is a list with exactly 2 elements
test_that("KOSPI200_list is a list with length 2", {
  expect_type(KOSPI200_list, "list")
  expect_length(KOSPI200_list, 2)
})

# Test 2: Confirm the names of the list elements are correct
test_that("KOSPI200_list has correct names", {
  expect_named(KOSPI200_list, c("date", "index"))
})

# Test 3: Confirm the "date" element is a Date vector of length 896
test_that("KOSPI200_list$date is a Date vector of length 896", {
  expect_s3_class(KOSPI200_list$date, "Date")
  expect_length(KOSPI200_list$date, 896)
})

# Test 4: Confirm the "index" element is a numeric vector of length 896
test_that("KOSPI200_list$index is a numeric vector of length 896", {
  expect_type(KOSPI200_list$index, "double")
  expect_length(KOSPI200_list$index, 896)
})
