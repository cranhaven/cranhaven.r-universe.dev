# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("get_vintage_data() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
})

test_that("get_vintage_data() vars", {
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    head(
      RcensusPkg::get_vintage_data(
        dataset = "acs/acs5",
        vars = c("B01001A_017E", "B01001A_001E"),
        vintage = 2021,
        region = "state:*"
      )
    )
  })
})

test_that("get_vintage_data() group", {
  expect_true(requireNamespace("usmap", quietly = TRUE))
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    holmes_ohio_fips <- usmap::fips(state = "Ohio", county = "Holmes")
    ohio_fips <- substr(holmes_ohio_fips, 1, 2)
    holmes_fips <- substr(holmes_ohio_fips, 3, 5)
    head(
      RcensusPkg::get_vintage_data(
        dataset = "dec/dhc",
        vintage = 2020,
        group = "H12I",
        wide_to_long = TRUE,
        region = paste0("county:", holmes_fips),
        regionin = paste0("state:", ohio_fips),
      )
    )
  })
})
