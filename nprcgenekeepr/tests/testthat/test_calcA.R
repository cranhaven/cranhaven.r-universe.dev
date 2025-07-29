#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("calcA")
library(testthat)
data("ped1Alleles")

test_that("calcA forms dataframe with correct calculations", {
  rare <- calcA(ped1Alleles, threshold = 3L, byID = FALSE)
  expect_equal(sum(rare[, 1L]), 318L)
  expect_equal(sum(rare[, 2L]), 325L)
  expect_equal(sum(rare[, 3L]), 313L)
  expect_equal(sum(rare[, 4L]), 328L)
})
