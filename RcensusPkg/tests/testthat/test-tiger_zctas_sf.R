test_that("tiger_zctas_sf() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("sf", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("tiger_zctas_sf()", {
  expect_snapshot({
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    express <- parse(text = 'ZCTA5CE20 == "02420"')

    mun_zcta_sf <- RcensusPkg::tiger_zctas_sf(
      vintage = 2020,
      general = TRUE,
      express = express,
      output_dir = output_dir,
      do_progress = FALSE,
      delete_files = TRUE
    )
    a_plot <- RplotterPkg::create_sf_plot(mun_zcta_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_zctas_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
