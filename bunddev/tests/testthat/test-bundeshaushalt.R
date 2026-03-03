test_that("bundeshaushalt budget data returns tibble", {
  skip_if_offline()
  skip_on_cran()

  data <- bundeshaushalt_budget_data(
    params = list(year = 2021, account = "expenses"),
    flatten = TRUE
  )
  expect_s3_class(data, "tbl_df")
  if (nrow(data) > 0) {
    expect_true(all(c("account", "unit", "year") %in% names(data)))
  }
})
