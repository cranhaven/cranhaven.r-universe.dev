test_that("autobahn helpers return tibbles", {
  skip_if_offline()
  skip_on_cran()

  roads <- autobahn_roads()
  expect_s3_class(roads, "tbl_df")

  if (nrow(roads) == 0) {
    skip("No roads returned from Autobahn API.")
  }

  road_id <- roads$road_id[[1]]
  roadworks <- autobahn_roadworks(road_id, flatten = TRUE, flatten_mode = "json")
  warnings <- autobahn_warnings(road_id, flatten = TRUE, flatten_mode = "json")
  webcams <- autobahn_webcams(road_id, flatten = TRUE, flatten_mode = "json")
  closures <- autobahn_closures(road_id, flatten = TRUE, flatten_mode = "json")
  charging <- autobahn_charging_stations(road_id, flatten = TRUE, flatten_mode = "json")
  parking <- autobahn_parking_lorries(road_id, flatten = TRUE, flatten_mode = "json")

  expect_s3_class(roadworks, "tbl_df")
  expect_s3_class(warnings, "tbl_df")
  expect_s3_class(webcams, "tbl_df")
  expect_s3_class(closures, "tbl_df")
  expect_s3_class(charging, "tbl_df")
  expect_s3_class(parking, "tbl_df")
  expect_true("start_time" %in% names(roadworks))

  if (nrow(roadworks) > 0) {
    details <- autobahn_roadwork_details(
      roadworks$identifier[[1]],
      flatten = TRUE,
      flatten_mode = "json"
    )
    expect_s3_class(details, "tbl_df")
  }

  if (nrow(warnings) > 0) {
    details <- autobahn_warning_details(
      warnings$identifier[[1]],
      flatten = TRUE,
      flatten_mode = "json"
    )
    expect_s3_class(details, "tbl_df")
  }

  if (nrow(webcams) > 0) {
    details <- autobahn_webcam_details(
      webcams$identifier[[1]],
      flatten = TRUE,
      flatten_mode = "json"
    )
    expect_s3_class(details, "tbl_df")
  }

  if (nrow(closures) > 0) {
    details <- autobahn_closure_details(
      closures$identifier[[1]],
      flatten = TRUE,
      flatten_mode = "json"
    )
    expect_s3_class(details, "tbl_df")
  }

  if (nrow(charging) > 0) {
    details <- autobahn_charging_station_details(
      charging$identifier[[1]],
      flatten = TRUE,
      flatten_mode = "json"
    )
    expect_s3_class(details, "tbl_df")
  }

  if (nrow(parking) > 0) {
    details <- autobahn_parking_lorry_details(
      parking$identifier[[1]],
      flatten = TRUE,
      flatten_mode = "json"
    )
    expect_s3_class(details, "tbl_df")
  }
})
