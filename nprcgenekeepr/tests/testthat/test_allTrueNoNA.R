#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("allTrueNoNA")
library(testthat)

test_that("allTrueNoNA judges all vectors with one or more NA value as FALSE", {
  expect_true(allTrueNoNA(c(TRUE, TRUE, TRUE)))
  expect_false(allTrueNoNA(c(NA, TRUE, TRUE)))
  expect_false(allTrueNoNA(c(NA, FALSE, TRUE)))
  expect_false(allTrueNoNA(c(NA, FALSE, FALSE)))
  expect_false(allTrueNoNA(c(FALSE, FALSE, FALSE)))
})
