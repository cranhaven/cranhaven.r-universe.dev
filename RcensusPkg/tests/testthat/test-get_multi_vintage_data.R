# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("get_multi_vintage_data() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
})

test_that("get_multi_vintage_data()", {
  expect_true(requireNamespace("usmap", quietly = TRUE))
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    deschutes_fips <- usmap::fips("OR","Deschutes")
    state <- substr(deschutes_fips,1,2)
    county <- substr(deschutes_fips,3,5)

    head(RcensusPkg::get_multi_vintage_data(
      dataset = "acs/acs1",
      vintage_v = 2005:2019,
      vars = c("B25077_001E", "B25077_001M"),
      region = paste0("county:", county),
      regionin = paste0("state:", state)
    ))
  })
})
