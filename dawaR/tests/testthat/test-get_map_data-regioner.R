test_that("get_map_data() fetches data properly and provides `regioner` in the proper format with cache = TRUE", {
  skip_on_cran()
  Sys.setenv(TZ = "UTC")
  expect_snapshot(get_map_data(type = "regioner", cache = TRUE))
  expect_snapshot(get_map_data(type = "regioner"))
  expect_snapshot(get_map_data(type = "regioner", cache = FALSE))
  expect_snapshot(get_map_data(type = "regioner"))
  expect_snapshot(get_map_data(type = "regioner"))
})
