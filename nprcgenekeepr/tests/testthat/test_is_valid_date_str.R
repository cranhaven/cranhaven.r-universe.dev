#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("test_is_valid_date_str")
library(testthat)

dates <- c(
  "13-21-1995", "20-13-98", "5-28-1014", "1-21-15", "2-13-2098",
  "25-28-2014", "11111111", NA, "99999999", "19821021", "12345678",
  "7-8-2006", "2-7-2015", "3-1-2003", "11-21-2006", "6-7-2003",
  "9-5-2009", "10-1-2006", "12-14-2006", "9-3-2006", "7-23-2009"
)
test_that("is_valid_date_str returns correct logical values", {
  expect_identical(
    is_valid_date_str(dates, format = "%m-%d-%Y"),
    c(
      FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE
    )
  )
  expect_identical(
    is_valid_date_str(dates, format = "%m-%d-%Y", optional = TRUE),
    c(
      NA, NA, TRUE, TRUE, TRUE, NA, TRUE, NA, NA, TRUE, NA, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    )
  )
  expect_identical(
    is_valid_date_str(dates, format = "%m-%d-%Y", optional = FALSE),
    c(
      FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE
    )
  )
  expect_identical(
    is_valid_date_str(c(19821021L, 20140806L),
      format = "%m-%d-%Y",
      optional = FALSE
    ),
    c(FALSE, FALSE)
  )
})
