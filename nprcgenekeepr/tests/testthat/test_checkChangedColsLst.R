#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("checkChangedColsLst")
library(nprcgenekeepr)
library(lubridate)
pedOne <- data.frame(
  ego_id = c(
    "s1", "d1", "s2", "d2", "o1", "o2", "o3",
    "o4"
  ),
  `si re` = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam_id = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
  birth_date = mdy(
    paste0(
      sample(1L:12L, 8L, replace = TRUE), "-",
      sample(1L:28L, 8L, replace = TRUE), "-",
      sample(seq(0L, 15L, by = 3L), 8L, replace = TRUE) +
        2000L
    )
  ),
  stringsAsFactors = FALSE, check.names = FALSE
)

test_that("checkChangedColsLst identifies absence of column changes", {
  errorLst <- getEmptyErrorLst()
  expect_false(checkChangedColsLst(errorLst$changedCols))
})
test_that("checkChangedColsLst identifies presence of column changes", {
  errorLst <- qcStudbook(pedOne, reportErrors = TRUE, reportChanges = TRUE)
  expect_true(checkChangedColsLst(errorLst$changedCols))
})
