#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("rankSubjects")
library(testthat)
## reportGV() unit test is weak.
rpt <- rankSubjects(nprcgenekeepr::finalRpt)
test_that("rankSubjects ranks subject correctly", {
  expect_identical(nrow(rpt[[2L]]), 68L)
  expect_identical(rpt[[1L]][1L, "value"], "High Value")
  expect_identical(rpt[[3L]][1L, "value"], "Low Value")
  expect_identical(rpt[[3L]][1L, "rank"], 190L)
  expect_identical(rpt[["lowMk"]][68L, "rank"], 189L)
})
