test_that("tiger_states_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_states_sf()", {
  expect_snapshot({
    florida_fips <- usmap::fips(state = "florida")
    express <- parse(text = paste0("STATEFP == ", '"', florida_fips, '"'))

    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    florida_general_sf <- RcensusPkg::tiger_states_sf(
      general = TRUE,
      express = express,
      output_dir = output_dir,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(florida_general_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_states_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
