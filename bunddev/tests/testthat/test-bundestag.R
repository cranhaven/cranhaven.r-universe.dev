test_that("bundestag endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  speaker <- bundestag_speaker()
  expect_s3_class(speaker, "tbl_df")

  conferences <- bundestag_conferences()
  expect_s3_class(conferences, "tbl_df")

  committees <- bundestag_ausschuesse()
  expect_s3_class(committees, "tbl_df")
})
