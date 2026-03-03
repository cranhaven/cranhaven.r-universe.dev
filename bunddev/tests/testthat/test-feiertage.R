test_that("feiertage list returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- feiertage_list(jahr = 2024)

  expect_s3_class(results, "tbl_df")
  if (nrow(results) > 0) {
    expect_true(all(c("region", "holiday", "date", "note", "date_time") %in% names(results)))
  }
})
