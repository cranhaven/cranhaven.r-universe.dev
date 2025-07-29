#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr

test_that("getIndianOriginStatus returns the correct values", {
  origin <- c("INDIAN", "INDIAN", "INDIAN", "INDIAN", "INDIAN")
  status <- getIndianOriginStatus(origin)
  expect_identical(
    status,
    list(ancestry = list(
      chinese = 0L,
      indian = 5L,
      hybrid = 0L,
      borderline = 0L,
      japanese = 0L,
      unknown = 0L,
      other = 0L
    ), color = "green", colorIndex = 3L)
  )
  expect_equal(sum(unlist(status$ancestry)), 5L)
  origin <- c("INDIAN", "HYBRID", "INDIAN", "INDIAN", "INDIAN")
  status <- getIndianOriginStatus(origin)
  expect_equal(
    status,
    list(ancestry = list(
      chinese = 0L,
      indian = 4L,
      hybrid = 1L,
      borderline = 0L,
      japanese = 0L,
      unknown = 0L,
      other = 0L
    ), color = "red", colorIndex = 1L)
  )
  expect_equal(sum(unlist(status$ancestry)), 5L)
  origin <- c("INDIAN", "BORDERLINE_HYBRID", "INDIAN", "INDIAN", "INDIAN")
  status <- getIndianOriginStatus(origin)
  expect_equal(
    status,
    list(ancestry = list(
      chinese = 0L,
      indian = 4L,
      hybrid = 0L,
      borderline = 1L,
      japanese = 0L,
      unknown = 0L,
      other = 0L
    ), color = "yellow", colorIndex = 2L)
  )
  expect_identical(sum(unlist(status$ancestry)), 5L)
  origin <- c("INDIAN", "CHINESE", "INDIAN", "INDIAN", "INDIAN")
  status <- getIndianOriginStatus(origin)
  expect_identical(
    status,
    list(ancestry = list(
      chinese = 1L,
      indian = 4L,
      hybrid = 0L,
      borderline = 0L,
      japanese = 0L,
      unknown = 0L,
      other = 0L
    ), color = "red", colorIndex = 1L)
  )
})
