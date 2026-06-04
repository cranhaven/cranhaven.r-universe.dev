test_that("tiger_county_subsection_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_county_subsection_sf()", {
  expect_snapshot({
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    ohio_hc_fips <- usmap::fips(state = "ohio", county = "holmes")
    ohio_fips <- substr(ohio_hc_fips,1,2)
    hc_fips <- substr(ohio_hc_fips,3,5)

    express <- parse(text = paste0("COUNTYFP == ", '"', hc_fips, '"'))

    hc_ctysub_sf <- RcensusPkg::tiger_county_subsection_sf(
      state = ohio_fips,
      vintage = 2020,
      general = TRUE,
      express = express,
      output_dir = output_dir,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(hc_ctysub_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_county_subsection_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
