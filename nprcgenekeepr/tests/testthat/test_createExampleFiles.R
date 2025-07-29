#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("createExampleFiles")
library(testthat)
files <- suppressMessages(createExampleFiles())
test_that("createExampleFiles creates all files", {
  expect_true(all(file.exists(files)))
})
