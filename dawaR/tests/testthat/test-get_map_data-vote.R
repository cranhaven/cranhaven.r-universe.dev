test_that("get_map_data() fetches data properly and provides `afstemningsomraader` in the proper format with cache = TRUE", {
  skip_on_cran()
  Sys.setenv(TZ = "UTC")
  expect_snapshot(get_map_data(type = "afstemningsomraader", cache = TRUE))
  expect_snapshot(get_map_data(type = "afstemningsomraader", cache = FALSE))
  expect_snapshot(get_map_data(type = "afstemningsomraader"))
  expect_snapshot(get_map_data(type = "afstemningsomraader"))
  expect_snapshot(get_map_data(type = "afstemningsomraader"))
})
