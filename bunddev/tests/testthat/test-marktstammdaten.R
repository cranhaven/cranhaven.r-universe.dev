test_that("marktstammdaten endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  filters <- marktstammdaten_filters_stromerzeugung()
  expect_s3_class(filters, "tbl_df")

  data <- marktstammdaten_stromerzeugung(params = list(page = 1, pageSize = 1))
  expect_s3_class(data, "tbl_df")
})
