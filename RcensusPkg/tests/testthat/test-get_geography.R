test_that("get_geographyt() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
})

test_that("get_geography()", {
  expect_snapshot(RcensusPkg::get_geography(
    dataset = "acs/acs1/profile",
    vintage = 2019
  ))
})
