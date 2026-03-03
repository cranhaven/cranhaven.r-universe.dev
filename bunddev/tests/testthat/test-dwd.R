test_that("dwd helpers return tibbles", {
  skip_if_offline()
  skip_on_cran()

  crowd <- dwd_crowd_reports()
  expect_s3_class(crowd, "tbl_df")
  if (nrow(crowd) > 0) {
    expect_true("timestamp_time" %in% names(crowd))
  }

  warnings <- dwd_warnings_nowcast()
  expect_s3_class(warnings, "tbl_df")
  if (nrow(warnings) > 0) {
    expect_true(all(c("start_time", "end_time") %in% names(warnings)))
  }

  stations <- dwd_station_overview("10865", flatten = TRUE, flatten_mode = "json")
  expect_s3_class(stations, "tbl_df")
  if (nrow(stations) > 0) {
    expect_true("forecast_start_time" %in% names(stations))
  }
})
