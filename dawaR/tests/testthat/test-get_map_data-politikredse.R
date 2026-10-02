test_that("get_map_data() fetches data properly and provides `politikredse` in the proper format with cache = TRUE", {
  skip_on_cran()
  Sys.setenv(TZ = "UTC")
  expect_snapshot(get_map_data(type = "politikredse", cache = TRUE))
  expect_snapshot(get_map_data(type = "politikredse"))
  expect_snapshot(get_map_data(type = "politikredse", cache = FALSE))
  expect_snapshot(get_map_data(type = "politikredse"))
  expect_snapshot(get_map_data(type = "politikredse"))
})
