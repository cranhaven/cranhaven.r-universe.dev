test_that("Get map data errs correctly", {
  expect_snapshot_error(get_map_data("fisk"))
  expect_error(get_map_data("fisk"))
})
