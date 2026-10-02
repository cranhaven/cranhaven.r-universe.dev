test_that("get_map_data() fetches data properly and provides `kommuner` in the proper format with cache = TRUE", {
  skip_on_cran()
  Sys.setenv(TZ = "UTC")
  expect_snapshot(get_map_data(type = "kommuner", cache = TRUE))
  expect_snapshot(get_map_data(type = "kommuner", cache = FALSE))
  expect_snapshot(get_map_data(type = "kommuner"))
  expect_snapshot(get_map_data(type = "kommuner"))
  expect_snapshot(get_map_data(type = "kommuner"))
})
