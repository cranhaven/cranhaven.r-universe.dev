test_that("dashboard_deutschland endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  entries <- dashboard_deutschland_get()
  expect_s3_class(entries, "tbl_df")
  if (nrow(entries) > 0) {
    expect_true(all(c("id", "name", "category") %in% names(entries)))
  }

  indicators <- dashboard_deutschland_indicators("tile_1667811574092")
  expect_s3_class(indicators, "tbl_df")

  geo <- dashboard_deutschland_geo()
  expect_s3_class(geo, "tbl_df")
  if (nrow(geo) > 0) {
    expect_true("features" %in% names(geo))
  }
})
