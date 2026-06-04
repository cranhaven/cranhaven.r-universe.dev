test_that("tiger_water_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_water_sf() area", {
  expect_snapshot({
    state_county_fips <- usmap::fips(state = "Ohio", county = "Geauga")
    state_fips <- substr(state_county_fips,1,2)
    county_fips <- substr(state_county_fips,3,5)

    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    geauga_area_water_sf <- RcensusPkg::tiger_water_sf(
      state = state_fips,
      county = county_fips,
      output_dir = output_dir,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(geauga_area_water_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_water_sf() area", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
