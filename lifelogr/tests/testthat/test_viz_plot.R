##
## Entire file layu code
##
library(lifelogr)
context("Plot Functions Output")

test_that("plot_sleep produces the right error messages", {
  expect_error(plot_sleep(EX, "weekday"), 
               '"plot_type" must be one of "all", "by_weekday", "by_start_end_time", "by_datetime", "by_restless_prop", "by_restless_min", or "by_quality"')
})

test_that("plot_daily produces the right error messages", {
  expect_error(plot_daily(EX, "step"), 
               '"measure_var" must be one of "all", "steps", "floors", "distance", "calories", "mins_very", "rest_hr"')
})

test_that("plot_intraday produces the right error messages", {
  expect_error(plot_intraday(EX, "step"), 
               '"measure_var" must be one of "all", "steps", "floors", "distance", "caloriesBurned", "activeMin", "bpm", "weight"')
})

test_that("plot_sleep_start_end produces the right error messages", {
  expect_error(plot_sleep_start_end(EX, "week"),
               "'color_var' must be 'day_type' for weekend/weekday or 'day_of_week' for day of the week")
})

test_that("plot_distance produces the right error messages", {
  expect_error(plot_distance(EX, "KM"),
               "'unit' must be 'mi' or 'km'")
})

test_that("plot_i_distance produces the right error messages", {
  expect_error(plot_i_distance(EX, unit = "pound"),
               "unit must be 'lb' or 'kg'")
})

test_that("viz_daily functions produce no output", {
  expect_output(plot_daily(EX, "steps"), NA)
  expect_output(plot_d(EX, "steps"), NA)
  expect_output(plot_steps(EX), NA)
  expect_output(plot_floors(EX), NA)
  expect_output(plot_distance(EX), NA)
  expect_output(plot_distance(EX, unit = "km"), NA)
  expect_output(plot_cal(EX), NA)
  expect_output(plot_mins_very(EX), NA)
  expect_output(plot_rest_hr(EX), NA)
})

test_that("viz_sleep functions produce no output", {
  expect_output(plot_sleep(EX, "by_weekday"), NA)
  expect_output(plot_sleep_weekday(EX), NA)
  expect_output(plot_sleep_start_end(EX), NA)
  expect_output(plot_sleep_over_time(EX), NA)
  expect_output(plot_sleep_restless_prop(EX), NA)
  expect_output(plot_sleep_restless_min(EX), NA)
  expect_output(plot_sleep_quality(EX), NA)
})

test_that("viz_intraday functions produce no output", {
  expect_output(plot_i(EX, "steps"), NA)
  expect_output(plot_i(EX, "distance", FALSE), NA)
  expect_output(plot_intraday(EX, "bpm"), NA)
  expect_output(plot_i_distance(EX), NA)
  expect_output(plot_i_steps(EX), NA)
  expect_output(plot_i_floors(EX), NA)
  expect_output(plot_i_cal(EX), NA)
  expect_output(plot_i_active_min(EX), NA)
  expect_output(plot_i_hr(EX), NA)
  expect_output(plot_i_weight(EX), NA)
})

test_that("plot_i_hr_datetime produces a ggplot object", {
  expect_is(plot_i_hr_datetime(EX), "ggplot")
})