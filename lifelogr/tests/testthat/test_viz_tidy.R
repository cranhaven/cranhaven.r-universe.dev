##
## Code in this file written entirely by layu
##
library(lifelogr)
context("Tidy Data functions")

test_that("get_hr_zones produces the right structure", {
  expect_is(get_hr_zones(EX), "list")
  expect_named(get_hr_zones(EX), c("peak", "cardio", "fat_burn"), ignore.order = TRUE)
})

test_that("get_hr_zones produces the right output", {
  expect_equal(get_hr_zones(EX)$peak[1], 162)
  expect_equal(get_hr_zones(EX)$peak[2], 191)
  expect_error(get_hr_zones(), 'argument "person" is missing, with no default')
})

test_that("tidy_multi_meas_data produces output of the right class", {
  expect_s3_class(tidy_multi_meas_data(data = tibble::tibble(
    date = lubridate::ymd("1970-01-01", "1970-01-02", "1970-01-03"),
    x = c(7.5, 8.0, 7.9),
    y = c(7.4, 7.0, 7.7))),
    "tbl")
  expect_s3_class(tidy_multi_meas_data(data = data.frame(
    date = lubridate::ymd("1970-01-01", "1970-01-02", "1970-01-03"),
    x = c(7.5, 8.0, 7.9),
    y = c(7.4, 7.0, 7.7))),
    "data.frame")
})

test_that("tidy_mutli_meas_data produces the right error messages", {
  expect_error(tidy_multi_meas_data(data = data.frame(
    x = 1:3, y = letters[1:3], z = LETTERS[2:4])),
    "'date' must be a named column of 'data'")
  expect_error(tidy_multi_meas_data(),
               'argument "data" is missing, with no default')
})

test_that("tidy_mutli_meas_data produces the right output", {
  a <- data.frame(
    date = seq(lubridate::ymd("2017-03-17"), lubridate::ymd("2017-03-19"), 
               by = "day"), 
    y = letters[1:3], 
    z = LETTERS[2:4], stringsAsFactors = FALSE)
  expect_equal(nrow(tidy_multi_meas_data(data = a)), 6)
  expect_equal(ncol(tidy_multi_meas_data(data = a)), 3)
  expect_named(tidy_multi_meas_data(data = a), c("date", "measures", "value"), 
               ignore.order = TRUE)
})

test_that("agg_sleep_weekday produces output of the right class", {
  expect_s3_class(agg_sleep_weekday(EX), "tbl")
})

test_that("agg_sleep_weekday produces the right error messages", {
  expect_error(agg_sleep_weekday(), 
               'argument "person" is missing, with no default')
})

test_that("agg_sleep_weekday produces the right output", {
  expect_equal(nrow(agg_sleep_weekday(EX)), 14)
  expect_equal(ncol(agg_sleep_weekday(EX)), 3)
  expect_named(agg_sleep_weekday(EX), c("day_of_week", "measure", "hours"))
})