test_that("tagesschau helpers return tibbles", {
  skip_if_offline()
  skip_on_cran()

  homepage <- tagesschau_homepage(flatten = TRUE, flatten_mode = "json")
  expect_s3_class(homepage, "tbl_df")
  expect_true("date_time" %in% names(homepage))

  news <- tagesschau_news(flatten = TRUE, flatten_mode = "json")
  expect_s3_class(news, "tbl_df")
  expect_true("date_time" %in% names(news))

  channels <- tagesschau_channels(flatten = TRUE, flatten_mode = "json")
  expect_s3_class(channels, "tbl_df")
  expect_true("date_time" %in% names(channels))

  unnest_news <- tagesschau_news(flatten = TRUE, flatten_mode = "unnest")
  expect_s3_class(unnest_news, "tbl_df")
})
