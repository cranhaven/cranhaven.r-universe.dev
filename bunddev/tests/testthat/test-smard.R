test_that("smard helpers return tibbles", {
  skip_if_offline()
  skip_on_cran()

  indices <- suppressWarnings(smard_indices(410, region = "DE", resolution = "hour"))
  expect_s3_class(indices, "tbl_df")

  if (nrow(indices) == 0) {
    skip("No SMARD indices returned.")
  }

  timestamp <- indices$timestamp[[1]]
  series <- suppressWarnings(
    smard_timeseries(410, region = "DE", resolution = "hour", timestamp = timestamp)
  )
  expect_s3_class(series, "tbl_df")
  expect_true("time" %in% names(series))
})
