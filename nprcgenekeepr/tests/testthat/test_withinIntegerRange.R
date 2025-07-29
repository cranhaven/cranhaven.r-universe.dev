#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("withinIntegerRange")

test_that("withinIntegerRange forces value to integer within range", {
  expect_identical(withinIntegerRange(), 0L)
  expect_identical(withinIntegerRange(LETTERS, 0L, 10L), rep(0L, 26L))
  expect_identical(withinIntegerRange(2.6, 1L, 5L), 2L)
  expect_identical(withinIntegerRange(2.6, 0L, 2L), 2L)
  expect_identical(withinIntegerRange(c(0L, 2.6, -1L), 0L, 2L), c(0L, 2L, 0L))
  expect_identical(withinIntegerRange(c(0L, 2.6, -1L, NA), 0L, 2L), c(0L, 2L, 0L, 0L))
  expect_identical(
    withinIntegerRange(c(0L, 2.6, -1L, NA), 0L, 2L, na = "max"),
    c(0L, 2L, 0L, 2L)
  )
  expect_identical(
    withinIntegerRange(c(0L, 2.6, -1L, NA), 0L, 2L, na = "min"),
    c(0L, 2L, 0L, 0L)
  )
  expect_identical(withinIntegerRange(NA, 0L, 10L, na = "max"), 10L)
  expect_identical(withinIntegerRange(, 0L, 10L, na = "max"), 0L)
  expect_identical(withinIntegerRange(NULL, 0L, 10L, na = "max"), 0L)
})
test_that(paste0(
  "withinIntegerRange forces value to integer within range; ",
  "bad na value is correctly replaced"
), {
  expect_identical(
    withinIntegerRange(c(0L, 2.6, -1L, NA), 0L, 2L,
      na = "bad_entry"
    ),
    c(0L, 2L, 0L, 0L)
  )
})
