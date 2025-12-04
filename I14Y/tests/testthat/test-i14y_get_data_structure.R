test_that("i14y_get_data_structure() returns a data.frame", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  df <- i14y_get_data_structure(
    identifier = "SpiGes_Erhebung_Administratives"
  )
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
})
