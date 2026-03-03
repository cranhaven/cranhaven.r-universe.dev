test_that("nina endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  mapdata <- nina_mapdata_mowas()
  expect_s3_class(mapdata, "tbl_df")

  if (nrow(mapdata) > 0) {
    detail <- nina_warning_json(mapdata$id[[1]])
    expect_s3_class(detail, "tbl_df")
  }

  logos <- nina_logos()
  expect_s3_class(logos, "tbl_df")
})
