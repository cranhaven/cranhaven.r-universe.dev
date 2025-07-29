#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("setExit")
library(testthat)
library(lubridate)
set_seed(10L)
death <- mdy(paste0(
  sample(1L:12L, 10L, replace = TRUE), "-",
  sample(1L:28L, 10L, replace = TRUE), "-",
  sample(seq(0L, 15L, by = 3L), 10L, replace = TRUE) + 2000L
))
departure <- as.Date(rep(NA, 10L), origin = as.Date("1970-01-01"))
departure[c(1L, 3L, 6L)] <- as.Date(death[c(1L, 3L, 6L)],
  origin = as.Date("1970-01-01")
)
death[c(1L, 3L, 5L)] <- NA
death[6L] <- death[6L] + days(1L)
ped <- data.frame(
  id = paste0(100L + 1L:10L),
  birth = mdy(paste0(
    sample(1L:12L, 10L, replace = TRUE), "-",
    sample(1L:28L, 10L, replace = TRUE), "-",
    sample(seq(0L, 20L, by = 3L), 10L, replace = TRUE) + 1980L
  )),
  death = death,
  departure = departure,
  stringsAsFactors = FALSE
)
ped_1 <- setExit(ped)
ped_2 <- setExit(ped[, -3L])
ped_3 <- setExit(ped[, -4L])
ped_4 <- setExit(ped[, c(-3L, -4L)])
test_that("setExit picks the correct date", {
  expect_true(all(is.na(ped_4$exit)))
  expect_identical(format(ped_3$exit[[2L]], format = "%Y-%m-%d"), "2009-04-16")
  expect_true(all(is.na(ped_4$exit[c(1L, 3L, 5L)])))
  expect_identical(format(ped_2$exit[[1L]], format = "%Y-%m-%d"), "2015-07-19")
  expect_identical(format(ped_2$exit[[3L]], format = "%Y-%m-%d"), "2012-06-04")
  expect_true(all(is.na(ped_2$exit[c(2L, 4L:5L, 7L:10L)])))
  expect_identical(format(ped_1$exit[[6L]], format = "%Y-%m-%d"), "2012-03-13")
  expect_identical(format(ped_1$exit[[7L]], format = "%Y-%m-%d"), "2015-04-02")
  expect_true(!any(is.na(ped_1$exit[c(1L:4L, 6L:10L)])))
})
test_that("setExit returns same empty dataframe as provided if nrow() == 0", {
  emptyDf <- data.frame(
    id = integer(0L),
    birth = as.Date(numeric(0L), origin = "1-1-1970"),
    death = as.Date(numeric(0L), origin = "1-1-1970"),
    departure = as.Date(numeric(0L), origin = "1-1-1970"),
    stringsAsFactors = FALSE
  )
  expect_identical(setExit(emptyDf), emptyDf)
})
