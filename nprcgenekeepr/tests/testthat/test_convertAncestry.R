#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("convertAncestry")

original <- c("china", "india", "hybridized", NA, "human", "gorilla")
ancestry <- convertAncestry(original)
test_that("convertAncestry makes correct transformations", {
  expect_true(is.factor(ancestry))
  ancestry <- as.character(ancestry)
  expect_equal(ancestry[1L], "CHINESE")
  expect_equal(ancestry[2L], "INDIAN")
  expect_equal(ancestry[3L], "HYBRID")
  expect_equal(ancestry[4L], "UNKNOWN")
  expect_equal(ancestry[5L], "OTHER")
  expect_equal(ancestry[6L], "OTHER")
})
