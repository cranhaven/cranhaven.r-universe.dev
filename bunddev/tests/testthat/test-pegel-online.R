test_that("pegel_online endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  stations <- pegel_online_stations(params = list(limit = 1), flatten = TRUE)
  expect_s3_class(stations, "tbl_df")
  if (nrow(stations) > 0) {
    expect_true(all(c("uuid", "shortname", "water") %in% names(stations)))
  }

  waters <- pegel_online_waters(params = list(limit = 1), flatten = TRUE)
  expect_s3_class(waters, "tbl_df")

  if (nrow(stations) > 0) {
    ts <- pegel_online_timeseries(stations$uuid[[1]], "W")
    expect_s3_class(ts, "tbl_df")
  }
})
