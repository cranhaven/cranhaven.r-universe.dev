#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("fillBins")
library(testthat)
library(lubridate)
library(stringi)
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
pedOne$age <- (mdy("06/01/2018") - as.Date(pedOne$birth)) / dyears(1L)
test_that("fillBins adds correct number to each bin", {
  lower_ages <- seq(0L, 20L, by = 5L)
  upper_ages <- NULL
  expect_identical(fillBins(pedOne, lower_ages)$males, c(0L, 0L, 2L, 1L, 0L))
  expect_identical(fillBins(pedOne, lower_ages)$females, c(2L, 2L, 0L, 1L, 0L))
})
