#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("addIdRecords")
library(testthat)
uPedOne <- data.frame(
  id = c(NA, "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedOne <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
uPedOne <- uPedOne[!is.na(uPedOne$id), ]
test_that("addIdRecords adds parents correctly", {
  newPed <- addIdRecords(ids = "s1", pedOne, uPedOne)
  expect_identical(nrow(uPedOne) + 1L, nrow(newPed)) # one added
  expect_true(is.na(newPed$sire[newPed$id == "s1"])) # did not add bad data
  expect_true(is.na(newPed$dam[newPed$id == "s1"])) # did not add bad data
})
test_that("addIdRecords handles ids == NA correctly", {
  newPed <- addIdRecords(ids = NA, pedOne, uPedOne)
  expect_identical(nrow(uPedOne), nrow(newPed)) # no change
  newPed <- addIdRecords(ids = c(NA, "s1"), pedOne, uPedOne)
  expect_identical(nrow(uPedOne) + 1L, nrow(newPed)) # one added
})
