test_that("lebensmittelwarnung warnings returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  Sys.setenv(LEBENSMITTELWARNUNG_API_KEY = "baystmuv-vi-1.0 os=ios, key=9d9e8972-ff15-4943-8fea-117b5a973c61")
  bunddev_auth_set("lebensmittelwarnung", type = "api_key", env_var = "LEBENSMITTELWARNUNG_API_KEY")

  results <- lebensmittelwarnung_warnings(
    food = list(rows = 1),
    products = list(rows = 0),
    flatten = TRUE,
    flatten_mode = "json"
  )

  expect_s3_class(results, "tbl_df")
  if (nrow(results) > 0) {
    expect_true(all(c("title", "published_date", "published_date_time") %in% names(results)))
  }
})
