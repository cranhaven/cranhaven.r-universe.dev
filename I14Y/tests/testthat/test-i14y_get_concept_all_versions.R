test_that("i14y_get_concept_all_versions() returns a data.frame", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  df <- i14y_get_concept_all_versions(
    id = "08d94604-e058-62a2-aa25-53f84b974201", # DV_NOGA_DIVISION
    language = "de"
  )
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
})
