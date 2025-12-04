test_that("i14y_get_nomenclature_level() returns a data.frame", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  df <- i14y_get_nomenclature_level(
    identifier = "HCL_NOGA",
    level = 1,
    language = "de"
  )
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
})
