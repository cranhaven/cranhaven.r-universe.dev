#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getPossibleCols")
library(testthat)
cols <- c(
  "id", "sire", "dam", "sex", "gen", "birth", "exit", "age",
  "ancestry", "population", "origin", "status", "condition",
  "spf", "vasxOvx", "pedNum", "first", "second", "first_name",
  "second_name", "recordStatus"
)
cols <- c(
  "id", "sire", "dam", "sex", "gen", "birth", "exit", "death",
  "age", "ancestry", "population", "origin", "status", "condition",
  "departure", "spf", "vasxOvx", "pedNum", "first", "second",
  "first_name",
  "second_name", "recordStatus"
)
test_that("getPossibleCols returns the right columns", {
  expect_identical(getPossibleCols(), cols)
})
