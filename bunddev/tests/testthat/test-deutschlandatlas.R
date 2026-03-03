test_that("deutschlandatlas query returns tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- deutschlandatlas_query(
    table = "p_apo_f_ZA2022",
    params = list(
      where = "1=1",
      outFields = "*",
      f = "json",
      returnGeometry = "false",
      resultRecordCount = 1
    )
  )

  expect_s3_class(results, "tbl_df")
})
