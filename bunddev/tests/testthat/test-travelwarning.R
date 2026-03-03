test_that("travelwarning endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  warnings <- tryCatch(
    travelwarning_warnings(),
    error = function(e) skip(paste("API unavailable:", conditionMessage(e)))
  )
  expect_s3_class(warnings, "tbl_df")

  if (nrow(warnings) > 0) {
    detail <- travelwarning_warning(warnings$content_id[[1]])
    expect_s3_class(detail, "tbl_df")
  }

  reps <- travelwarning_representatives_germany()
  expect_s3_class(reps, "tbl_df")
})
