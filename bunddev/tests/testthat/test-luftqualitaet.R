test_that("luftqualitaet metadata endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  components <- luftqualitaet_components(params = list(lang = "de"))
  expect_s3_class(components, "tbl_df")

  networks <- luftqualitaet_networks(params = list(lang = "de"))
  expect_s3_class(networks, "tbl_df")
})
