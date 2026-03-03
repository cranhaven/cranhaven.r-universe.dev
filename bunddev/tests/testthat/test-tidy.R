test_that("bunddev_call_tidy returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  Sys.setenv(BEWERBERBOERSE_API_KEY = "jobboerse-bewerbersuche-ui")
  bunddev_auth_set("bewerberboerse", type = "api_key", env_var = "BEWERBERBOERSE_API_KEY")

  result <- bewerberboerse_search(
    params = list(was = "data", size = 5),
    flatten = TRUE,
    flatten_mode = "json"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("refnr", "berufe") %in% names(result)))
})
