test_that("ddb endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  api_key <- Sys.getenv("DDB_API_KEY")
  if (api_key == "") {
    skip("DDB_API_KEY not set")
  }
  bunddev_auth_set("ddb", type = "api_key", env_var = "DDB_API_KEY",
                   scheme = "OAuth oauth_consumer_key=\"%s\"")

  results <- ddb_search(query = "berlin", params = list(rows = 1))
  expect_s3_class(results, "tbl_df")

  sectors <- ddb_institution_sectors()
  expect_s3_class(sectors, "tbl_df")
})
