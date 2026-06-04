# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("plot_us_data() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
  expect_true(requireNamespace("downloader", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("gtable", quietly = TRUE))
  expect_true(requireNamespace("ggplotify", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_us_data() default", {
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    a_plot <- RcensusPkg::plot_us_data(
      title = "A Default Mapping of US States",
      output_dir = output_dir,
      delete_files = FALSE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_us_data() default", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("plot_us_data() discrete",{
  testthat::skip_if(Sys.getenv("CENSUS_KEY") == "", message = "Census Bureau API key required")
  expect_snapshot({
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    a_plot <- RcensusPkg::plot_us_data(
      df = RcensusPkg::vote2020,
      title = "US Presidential Vote 2020",
      states_col = "State",
      value_col = "Party",
      output_dir = output_dir,
      delete_files = FALSE,
      scale_breaks = c("R","D"),
      scale_limits = c("R","D"),
      scale_values = c("red","blue"),
      scale_labels = c("Republican","Democrat"),
      sf_color = "white"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_us_data() discrete", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
