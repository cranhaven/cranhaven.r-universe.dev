test_that("psm_stand returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- psm_stand()

  expect_s3_class(results, "tbl_df")
})

test_that("psm_mittel returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- psm_mittel(params = list(limit = 5))

  expect_s3_class(results, "tbl_df")
})

test_that("psm_wirkstoffe returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- psm_wirkstoffe(params = list(limit = 5))

  expect_s3_class(results, "tbl_df")
})
