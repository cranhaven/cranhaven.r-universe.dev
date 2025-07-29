#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getLkDirectRelatives")

test_that(
  "getLkDirectRelatives throws an error with no LabKey session connection",
  {
    expect_warning(getLkDirectRelatives(), "The file should be named:")
  }
)
