#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getMinParentAge")

test_that("getMinParentAge throws an error with no input defined", {
  expect_error(
    getMinParentAge(),
    "cannot coerce type 'closure' to vector of type 'double'"
  )
})
test_that(paste0(
  "getMinParentAge throws an error with input improperly ",
  "defined in ui.R"
), {
  expect_error(
    getMinParentAge(input = list(minParentAge = 0.0)),
    "cannot coerce type 'closure' to vector of type 'double'"
  )
})
