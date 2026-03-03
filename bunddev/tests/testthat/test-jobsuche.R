test_that("jobsuche search returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  Sys.setenv(JOBBOERSE_API_KEY = "jobboerse-jobsuche")
  bunddev_auth_set("jobsuche", type = "api_key", env_var = "JOBBOERSE_API_KEY")

  results <- jobsuche_search(
    params = list(was = "data", size = 5),
    flatten = TRUE,
    flatten_mode = "json"
  )

  expect_s3_class(results, "tbl_df")
  if (nrow(results) > 0) {
    expect_true(all(c(
      "aktuelle_veroeffentlichungsdatum_time",
      "eintrittsdatum_time",
      "modifikations_timestamp_time"
    ) %in% names(results)))
  }
})
