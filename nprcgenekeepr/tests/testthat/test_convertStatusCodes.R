#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("convertStatusCodes")
library(testthat)
original <- c(
  "A", "alive", "Alive", "1", "S", "Sale", "sold", "shipped",
  "D", "d", "dead", "died", "deceased", "2",
  "shiped", "3", "U", "4", "unknown", NA,
  "Unknown", "H", "hermaphrodite", "U", "Unknown", "4"
)
status <- convertStatusCodes(original)
test_that("convertStatusCodes makes correct transformations", {
  expect_true(is.factor(status))
  status <- as.character(status)
  expect_equal(status[1L], "ALIVE")
  expect_equal(status[2L], "ALIVE")
  expect_equal(status[3L], "ALIVE")
  expect_equal(status[4L], "ALIVE")
  expect_equal(status[5L], "SHIPPED")
  expect_equal(status[6L], "SHIPPED")
  expect_equal(status[7L], "SHIPPED")
  expect_equal(status[8L], "SHIPPED")
  expect_equal(status[9L], "DECEASED")
  expect_equal(status[10L], "DECEASED")
  expect_equal(status[11L], "DECEASED")
  expect_equal(status[12L], "DECEASED")
  expect_equal(status[13L], "DECEASED")
  expect_equal(status[14L], "DECEASED")
  expect_equal(status[15L], "SHIPPED")
  expect_equal(status[16L], "SHIPPED")
  expect_equal(status[17L], "UNKNOWN")
  expect_equal(status[18L], "UNKNOWN")
  expect_equal(status[19L], "UNKNOWN")
  expect_equal(status[20L], "UNKNOWN")
  expect_equal(status[21L], "UNKNOWN")
  expect_equal(status[22L], NA_character_)
  expect_equal(status[23L], NA_character_)
  expect_equal(status[24L], "UNKNOWN")
  expect_equal(status[25L], "UNKNOWN")
  expect_equal(status[26L], "UNKNOWN")
})
