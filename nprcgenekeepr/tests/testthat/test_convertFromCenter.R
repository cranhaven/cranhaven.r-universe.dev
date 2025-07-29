#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("convertFromCenter")
library(testthat)
original <- c(
  "y", "yes", "Y", "Yes", "YES", "n", "N", "No", "NO", "no",
  "t", "T", "True", "true", "TRUE", "f", "F", "false", "False",
  "FALSE"
)
status <- convertFromCenter(original)
test_that("convertFromCenter makes correct transformations", {
  expect_type(status, "logical")
  expect_true(status[1L])
  expect_true(status[2L])
  expect_true(status[3L])
  expect_true(status[4L])
  expect_true(status[5L])
  expect_false(status[6L])
  expect_false(status[7L])
  expect_false(status[8L])
  expect_false(status[9L])
  expect_false(status[10L])
  expect_true(status[11L])
  expect_true(status[12L])
  expect_true(status[13L])
  expect_true(status[14L])
  expect_true(status[15L])
  expect_false(status[16L])
  expect_false(status[17L])
  expect_false(status[18L])
  expect_false(status[19L])
  expect_false(status[20L])
})
test_that("convertFromCenter() detects input error", {
  original <- c("y", "&")
  expect_error(convertFromCenter(original), "fromCenter field has ambiguous")
})
