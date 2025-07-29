#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("chooseDate")
library(testthat)
library(lubridate)
set_seed(10L)
someDates <- mdy(paste0(
  sample(1L:12L, 10L, replace = TRUE), "-",
  sample(1L:28L, 10L, replace = TRUE), "-",
  sample(seq(0L, 15L, by = 3L), 10L, replace = TRUE) + 2000L
))
test_that("chooseDate picks the correct date", {
  expect_equal(
    chooseDate(someDates[1L], NA, earlier = TRUE),
    someDates[1L]
  )
  expect_equal(
    chooseDate(NA, someDates[2L], earlier = TRUE),
    someDates[2L]
  )
  expect_equal(
    chooseDate(someDates[1L], someDates[2L], earlier = TRUE),
    someDates[2L]
  )
  expect_equal(
    chooseDate(someDates[1L], someDates[2L], earlier = FALSE),
    someDates[1L]
  )
  expect_equal(
    chooseDate(someDates[2L], someDates[1L], earlier = TRUE),
    someDates[2L]
  )
  expect_equal(
    chooseDate(someDates[2L], someDates[1L], earlier = FALSE),
    someDates[1L]
  )
  expect_equal(
    chooseDate(someDates[2L], someDates[2L], earlier = TRUE),
    someDates[2L]
  )
})
