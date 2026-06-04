# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("get_idb_data() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
})

test_that("get_idb_data() 1year", {
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot(
    head(RcensusPkg::get_idb_data(
      dataset = "1year",
      years = c(2023, 2024),
      countries = c("BW", "NO")
    ))
  )
})

test_that("get_idb_data() 5year", {
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot(
    head(RcensusPkg::get_idb_data(
      dataset = "5year",
      years = 2023,
      group = TRUE,
      countries = "US",
      wide_to_long = TRUE
    ))
  )
})
