#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("obfuscateId")
library(testthat)
library(stringi)

set_seed(10L)
test_that("obfuscateId creates new ID with expected size", {
  id <- c("abc123", "george", "autumn")
  obfuscatedId <- obfuscateId(id, 6L)
  expect_length(obfuscatedId, 3L)
  expect_true(all(stri_length(obfuscatedId) == 6L))
  expect_length(id, length(unique(obfuscatedId)))
})
# this test is weak
test_that("obfuscateId does not create duplicates", { # this test is weak
  id <- stri_c(1L:10000L)
  obfuscatedId <- obfuscateId(id, 5L)
  expect_length(obfuscatedId, 10000L)
  expect_true(all(stri_length(obfuscatedId) == 5L))
  expect_length(id, length(unique(obfuscatedId)))
})

test_that("obfuscateId fails when duplicates cannot be avoided", {
  id <- stri_c(1L:10000L)
  expect_error(obfuscateId(id, size = 2L))
})
test_that("obfuscateId replaces unknown ID with unknown IDs (start with 'U'", {
  id <- c("U0001", "U123", "u001", "abc")
  alias <- obfuscateId(id, size = 4L)
  expect_true(all(stri_detect_regex(alias[1L:3L], "^U")))
  expect_false(stri_detect_regex(alias[4L], "^U"))
})
