test_that("i14y_search_nomenclature() returns a data.frame", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  df <- i14y_search_nomenclature(
    identifier = "HCL_NOGA",
    query = "agriculture",
    language = "fr"
  )
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
})
