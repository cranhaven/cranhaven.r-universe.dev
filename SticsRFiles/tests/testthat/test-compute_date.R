
context("Convert day numbers into calendar dates from a start year")

test_that("a-single-day", {
  day <- 290
  start_year <- 1994
  expected <- as.Date("1994-10-17")
  observed <- compute_date_from_day(day = day, start_year = start_year)
  expect_equal(observed, expected)
})

test_that("several-days_same-year", {
  day <- c(290, 700)
  start_year <- 1994
  expected <- as.Date(c("1994-10-17", "1995-12-01"))
  observed <- compute_date_from_day(day = day, start_year = start_year)
  expect_equal(observed, expected)
})
