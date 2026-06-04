test_that("tiger_roads_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_roads_sf()", {
  expect_snapshot({
    oh_fips <- usmap::fips(state = "ohio")

    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    ohio_roads_sf <- RcensusPkg::tiger_roads_sf(
      state = oh_fips,
      entity = "state_roads",
      output_dir = output_dir,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(ohio_roads_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("ohio_roads_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
