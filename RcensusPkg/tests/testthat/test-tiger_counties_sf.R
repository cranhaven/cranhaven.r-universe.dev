test_that("tiger_counties_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_counties_sf() Ohio", {
  expect_snapshot({
    # Determine the fips code for Ohio (returns "39")
    ohio_fips <- usmap::fips(state = "ohio")

    # Create an expression to filter out just Ohio counties
    #   from the simple feature dataframe
    express <- parse(text = paste0("STATEFP == ", '"', ohio_fips, '"'))

    # Get the Ohio county's generalized geometries
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    ohio_counties_sf <- RcensusPkg::tiger_counties_sf(
      output_dir = output_dir,
      general = TRUE,
      express = express,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(sf = ohio_counties_sf)
  })
  expect_true(!is.null(ohio_counties_sf))
  expect_true(!is.null(sf::st_geometry(ohio_counties_sf)))
  expect_equal(nrow(ohio_counties_sf), 88)
  vdiffr::expect_doppelganger("tiger_counties_sf() Ohio", a_plot)
})
