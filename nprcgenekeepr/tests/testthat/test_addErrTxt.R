#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("addErrTxt")

test_that("addErrTxt adds correct text", {
  expect_identical(
    addErrTxt(
      "", "egoid to id", "column changed is", "columns changed are"
    ),
    "column changed is: egoid to id.\n"
  )
  expect_identical(
    addErrTxt(
      "", "egoid and sireid to id and sire", "column changed is",
      "columns changed are"
    ),
    "columns changed are: egoid and sireid to id and sire.\n"
  )
  expect_identical(
    addErrTxt(
      "", c("id", "sire"),
      "missing column is",
      "missing columns are"
    ),
    "missing columns are: id and sire.\n"
  )
  expect_identical(
    addErrTxt(
      "", c("1", "2", "3", "4", "5", "6", "7"),
      "row having an invalid date is",
      "rows (up to the first 5) having an invalid date are"
    ),
    "rows (up to the first 5) having an invalid date are: 1, 2, 3, 4, and 5.\n"
  )
})
