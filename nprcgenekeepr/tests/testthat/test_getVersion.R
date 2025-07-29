#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getVersion")
library(stringi)
version1 <- getVersion()
version2 <- getVersion(date = FALSE)
test_that(paste0(
  "getVersion by default returns a version with date:",
  version1
), {
  expect_true(stri_detect_regex(version1, # version
    pattern = "^[0-9]{1,2}([.][0-9]{1,2})"
  ))
  expect_true(stri_detect_fixed(version1, pattern = "("))
  expect_length(stri_split_fixed(version1, pattern = "(")[[1L]], 2L)
  expect_true(stri_detect_regex(version1, pattern = "[0-9]{4}"))
})
test_that(paste0("getVersion returns a version without date:", version2), {
  expect_true(stri_detect_regex(version2, # version
    pattern = "^[0-9]{1,2}([.][0-9]{1,2})"
  ))
  expect_length(stri_split_fixed(version2, pattern = "(")[[1L]], 1L)
  expect_false(stri_detect_fixed(version2, pattern = "("))
})
