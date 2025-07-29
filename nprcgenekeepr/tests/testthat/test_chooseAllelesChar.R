#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("chooseAllelesChar")
library(testthat)
test_that("chooseAllelesChar returns a vector of characters of the right size
          and not the same vector as left", {
  expect_false(all(chooseAllelesChar(LETTERS, letters) == LETTERS))
  expect_false(all(chooseAllelesChar(LETTERS, letters) == letters))
  expect_length(chooseAllelesChar(LETTERS, letters),
    length(LETTERS)
  )
})
