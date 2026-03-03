test_that("bunddev_endpoints returns endpoints", {
  skip_if_offline()
  # skip_on_cran()

  endpoints <- bunddev_endpoints("abfallnavi")

  expect_s3_class(endpoints, "tbl_df")
  expect_true(all(c("method", "path", "operation_id", "summary") %in% names(endpoints)))
  expect_true(nrow(endpoints) > 0)
})
