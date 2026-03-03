test_that("diga endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()
  skip_if(Sys.getenv("DIGA_BEARER_TOKEN") == "", "DIGA_BEARER_TOKEN not set")

  results <- diga_device_definitions()
  expect_s3_class(results, "tbl_df")
})
