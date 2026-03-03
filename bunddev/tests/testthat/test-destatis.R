test_that("destatis endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  tables <- destatis_catalogue_tables()
  expect_s3_class(tables, "tbl_df")
})
