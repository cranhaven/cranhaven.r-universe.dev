test_that("tiger_places_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_places_sf()", {
  expect_snapshot({
    kentucky_fips <- usmap::fips(state = "kentucky")

    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    major_places_express <- expression(NAME %in% c(
      "Bardstown",
      "Bowling Green",
      "Louisville",
      "Versailles",
      "Owensboro",
      "Frankfort",
      "Elizabethtown",
      "Danville"
    ))

    kentucky_places_sf <- RcensusPkg::tiger_places_sf(
      state = kentucky_fips,
      express = major_places_express,
      general = TRUE,
      output_dir = output_dir,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(kentucky_places_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_places_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
