#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getPyramidAgeDist")
library(testthat)
library(lubridate)
library(stringi)

qcPed <- nprcgenekeepr::qcPed
ped <- qcPed[, c("id", "sire", "dam", "sex", "birth", "exit")]
ped <- getPyramidAgeDist(ped)
test_that("getPyramidAgeDist classifies and ages animals correctly", {
  expect_equal(as.numeric(table(ped$status)[[1L]]), 46L)
  expect_true(ped$age[ped$id == "YIAD2N"] > 19.0 &
    ped$age[ped$id == "YIAD2N"] < 20.0)
})
ped <- getPyramidAgeDist()
test_that("getPyramidAgeDist gets qcPed by default", {
  expect_equal(as.numeric(table(ped$status)[[1L]]), 46L)
  expect_true(ped$age[ped$id == "YIAD2N"] > 19.0 &
    ped$age[ped$id == "YIAD2N"] < 20.0)
})
ped <- qcPed[, c("id", "sire", "dam", "sex", "birth", "exit")]
charDatePed <- ped
charDatePed$birth <- format(charDatePed$birth, format = "%Y-%m-%d")
ped <- getPyramidAgeDist(charDatePed)
test_that("getPyramidAgeDist converts character based birth date", {
  expect_equal(as.numeric(table(ped$status)[[1L]]), 46L)
  expect_true(ped$age[ped$id == "YIAD2N"] > 19.0 &
    ped$age[ped$id == "YIAD2N"] < 20.0)
})
ped <- qcPed[, c("id", "sire", "dam", "sex", "birth", "exit")]
charDatePed <- ped
charDatePed$exit <- format(charDatePed$exit, format = "%Y-%m-%d")
charDatePed$exit[176L] <- "9999999999"
ped <- getPyramidAgeDist(charDatePed)
test_that(stri_c(
  "getPyramidAgeDist converts 9999999999 exit_date to NA and ",
  "makes status DECEASED"
), {
  expect_identical(ped$status[ped$id == charDatePed$id[176L]], "DECEASED")
  expect_true(is.na(ped$exit[ped$id == charDatePed$id[176L]]))
})
pedOne <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
  birth = mdy(
    paste0(
      sample(1L:12L, 8L, replace = TRUE), "-",
      sample(1L:28L, 8L, replace = TRUE), "-",
      sample(seq(0L, 15L, by = 3L), 8L, replace = TRUE) +
        2000L
    )
  ),
  stringsAsFactors = FALSE, check.names = FALSE
)
pedOne$exit <- pedOne$birth + days(5000L)
pedOne$age <- (now() - as.POSIXct(pedOne$birth)) / dyears(1L)

ped <- pedOne
ped$birth <- rep(TRUE, nrow(ped))
test_that("getPyramidAgeDist detect birth column of wrong type ", {
  expect_error(
    getPyramidAgeDist(ped),
    stri_c(
      "birth column must be of class 'Date', ",
      "'POSIXct', or 'character'"
    )
  )
})
ped <- pedOne
ped$exit <- rep(TRUE, nrow(ped))
test_that("getPyramidAgeDist detect exit column of wrong type ", {
  expect_error(
    getPyramidAgeDist(ped),
    stri_c(
      "exit_date column must be of class 'Date', ",
      "'POSIXct', or 'character'"
    )
  )
})
