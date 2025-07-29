#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("calcAge")

suppressMessages(library(lubridate))
set_seed(10L)
exit <- mdy(paste0(
  sample(1L:12L, 10L, replace = TRUE), "-",
  sample(1L:28L, 10L, replace = TRUE), "-",
  sample(seq(0L, 15L, by = 3L), 10L, replace = TRUE) + 2000L
))
birth <- exit - days(sample(0L:7500L, size = 10L, replace = TRUE))
exit[c(2L, 4L)] <- NA
todays_age <- round((as.double(Sys.Date() - birth[c(2L, 4L)]) / 365.25), 1L)
ages <- calcAge(birth, exit)
test_that("calcAge calculates ages correctly", {
  expect_equal(ages[c(2L, 4L)], todays_age)
  expect_equal(ages[c(1L, 3L, 5L:10L)], c(
    11.0, 3.5, 8.7, 15.4, 16.9, 19.6, 14.1,
    10.3
  ))
})
test_that("calcAge returns empty vector if empty vector provided", {
  emptyAges <- calcAge(as.Date(numeric(0L), origin = "1-1-1970"), Date(0L))
  expect_equal(emptyAges, as.Date(numeric(0L), origin = "1-1-1970"))
})
