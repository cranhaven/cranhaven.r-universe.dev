test_that("metadata works", {
  df1 <- data.frame(x = 1:2)
  expect_error(get_metadata_field(df1))
  expect_error(get_metadata(df1))

  df2 <- nisra_df(x = 1:2, meta = list(type = "test"))
  expect_equal(get_metadata_field(df2, "type"), "test")
  expect_equal(get_metadata(df2), new_nisra_meta(list(type = "test")))
})
