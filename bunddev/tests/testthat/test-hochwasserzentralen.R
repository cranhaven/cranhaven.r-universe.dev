test_that("hochwasserzentralen endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  bundeslaender <- hochwasserzentralen_bundeslaender()
  expect_s3_class(bundeslaender, "tbl_df")
  if (nrow(bundeslaender) > 0) {
    expect_true("ID" %in% names(bundeslaender))
  }

  if (nrow(bundeslaender) > 0) {
    info <- hochwasserzentralen_bundesland_info(bundeslaender$ID[[1]])
    expect_s3_class(info, "tbl_df")
  }

  lagepegel <- hochwasserzentralen_lagepegel()
  expect_s3_class(lagepegel, "tbl_df")
  if (nrow(lagepegel) > 0 && "PGNR" %in% names(lagepegel)) {
    expect_true(all(c("PGNR", "LAT", "LON") %in% names(lagepegel)))
    pegel_info <- hochwasserzentralen_pegel_info(lagepegel$PGNR[[1]])
    expect_s3_class(pegel_info, "tbl_df")
  }

  geojson <- hochwasserzentralen_bundesland_geojson("20211130")
  expect_s3_class(geojson, "tbl_df")
  if (nrow(geojson) > 0) {
    expect_true("features" %in% names(geojson))
  }
})
