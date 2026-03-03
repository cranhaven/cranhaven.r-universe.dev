test_that("regionalatlas_query returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- regionalatlas_query(table = "ai002_1_5")

  expect_s3_class(results, "tbl_df")
  expect_true(nrow(results) > 0)
})
