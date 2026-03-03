test_that("mudab endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  stations <- mudab_project_stations(range = list(from = 0, count = 1))
  expect_s3_class(stations, "tbl_df")

  params <- mudab_parameters(range = list(from = 0, count = 1))
  expect_s3_class(params, "tbl_df")
})
