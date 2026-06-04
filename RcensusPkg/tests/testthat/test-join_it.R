# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("join_it() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
})

test_that("join_it()", {
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    # Get the 2020 median household income data by tract for DC
    dc_fips <- usmap::fips(state = "dc")
    dc_B19013_dt <- RcensusPkg::get_vintage_data(
      dataset = "acs/acs5",
      vintage = 2020,
      vars = "B19013_001E",
      region = "tract",
      regionin = paste0("state:", dc_fips)
    )

    # Get the simple feature DC tract geometries and join the data dataframe "dc_B19013_dt"
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    dc_tracts_sf <- RcensusPkg::tiger_tracts_sf(
      state = dc_fips,
      output_dir = output_dir,
      general = TRUE,
      delete_files = FALSE
    )

    # Join the data with simple feature object
    head(RcensusPkg::join_it(
      df_1 = dc_B19013_dt,
      df_2 = dc_tracts_sf,
      key_1 = "GEOID",
      key_2 = "GEOID",
      return_sf = TRUE
    ))
  })
})
