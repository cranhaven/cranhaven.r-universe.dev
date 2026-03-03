test_that("bundestag lobbyregister search returns tibble", {
  skip_if_offline()
  skip_on_cran()

  results <- bundestag_lobbyregister_search(q = "energie")
  expect_s3_class(results, "tbl_df")
})
