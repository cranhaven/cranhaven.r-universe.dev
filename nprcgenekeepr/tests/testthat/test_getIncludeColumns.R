#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getIncludeColumns")
library(testthat)
columns <- getIncludeColumns()
test_that("getIncludeColumns returns all the right columns", {
  expect_identical(columns, c(
    "id", "sex", "age", "birth", "exit", "population",
    "condition", "origin", "first_name", "second_name"
  ))
})
