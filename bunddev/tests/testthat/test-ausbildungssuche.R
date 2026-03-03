test_that("ausbildungssuche endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  if (Sys.getenv("AUSBILDUNGSSUCHE_API_KEY") == "") {
    Sys.setenv(AUSBILDUNGSSUCHE_API_KEY = "infosysbub-absuche")
  }

  bunddev_auth_set("ausbildungssuche", type = "api_key", env_var = "AUSBILDUNGSSUCHE_API_KEY")

  results <- ausbildungssuche_search(params = list(size = 1))
  expect_s3_class(results, "tbl_df")

  if (nrow(results) > 0) {
    details <- ausbildungssuche_details(results$id[[1]])
    expect_s3_class(details, "tbl_df")
  }
})
