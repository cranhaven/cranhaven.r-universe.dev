# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("wide_to_long() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
})

test_that("wide_to_long()", {
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    B19001_1yr_wide_dt <- RcensusPkg::get_vintage_data(
      dataset = "acs/acs1",
      vintage = 2016,
      group = "B19001",
      region = "state"
    )

    head(RcensusPkg::wide_to_long(
      dt = B19001_1yr_wide_dt,
      id_v = c("NAME","GEOID")
    ))
  })
})
