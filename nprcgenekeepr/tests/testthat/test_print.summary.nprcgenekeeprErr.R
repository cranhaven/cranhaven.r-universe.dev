#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("print.summary.nprcgenekeeprErr")
library(testthat)
library(lubridate)

set_seed(10L)
pedOne <- data.frame(
  ego_id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
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
test_that("print.summary.nprcgenekeeprErr prints expected output", {
  pedClean <- qcStudbook(pedOne, minParentAge = NULL)
  expect_success(expect_output(summary(qcStudbook(
    pedClean,
    minParentAge = NULL, reportErrors = TRUE
  )), NA))
  expect_failure(
    expect_output(
      print(summary(qcStudbook(pedOne,
        reportErrors = TRUE
      ))),
      NA
    ),
    "produced output"
  )
})
