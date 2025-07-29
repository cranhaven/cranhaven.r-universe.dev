#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("insertSeparators")
library(testthat)
test_that(
  "insert separators at the right time and in the correct way",
  {
    expect_identical(
      nprcgenekeepr:::insertSeparators(c(
        "19991002",
        "20100228"
      ))[[1L]],
      "1999-10-02"
    )
    expect_identical(
      nprcgenekeepr:::insertSeparators(c(
        "19991002",
        "20100228"
      ))[[2L]],
      "2010-02-28"
    )
    expect_identical(
      nprcgenekeepr:::insertSeparators(c(
        "1999/10/02",
        "2010/02/28"
      ))[[2L]],
      "2010/02/28"
    )
  }
)
