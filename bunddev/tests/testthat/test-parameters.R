test_that("bunddev_parameters returns parameters", {
  skip_if_offline()
  skip_on_cran()

  params <- bunddev_parameters("tagesschau")
  expect_s3_class(params, "tbl_df")
  expect_true(all(c("method", "path", "name", "location") %in% names(params)))

  values <- bunddev_parameter_values(smard_timeseries, "resolution")
  expect_true(length(values) > 0)
})
