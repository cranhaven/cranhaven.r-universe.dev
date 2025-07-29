#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("checkRequiredCols")
library(stringi)
requiredCols <- getRequiredCols()
test_that("checkRequiredCols detects missing cols", {
  cols <- stri_c(
    "id,sire,siretype,dam,damtype,sex,numberofparentsknown,birth,",
    "arrivalatcenter,death,departure,status,ancestry,fromcenter?,",
    "origin"
  )
  expect_true(all(requiredCols %in% checkRequiredCols(cols,
    reportErrors = TRUE
  )))
})
