test_that("CFS_mapping", {
  # loading processed data
  dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
  cols.meta = c("uid_tree", "uid_site", "longitude", "latitude", "species")
  dt.mapping <- dt.samples_trt$tr_all_wide[
    , c(..cols.meta, as.character(1991:1995)), with = FALSE
  ]
  mapping_results <- CFS_mapping(dt.mapping, year.span = c(1991,1993))

  testthat::expect_s3_class(mapping_results, "cfs_gif")
})

test_that("plot_mapping", {
  # loading processed data
  dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
  cols.meta = c("uid_tree", "uid_site", "longitude", "latitude", "species")
  dt.mapping <- dt.samples_trt$tr_all_wide[
    , c(..cols.meta, as.character(1991:1995)), with = FALSE
  ]
  mapping_results <- CFS_mapping(dt.mapping, year.span = c(1991,1993))
  png_list <- plot_mapping(
    mapping_results = mapping_results,
    dir.out = "mapping_test",
    png.text = list(
      text_top  = "Ring width measurment - ",
      text_bott = "Source: demo-samples",
      text_side = "ring width (mm)"
    )
  )
  # structure tests
  expect_true(is.list(png_list))
  lapply(png_list, function(p) expect_true(is.list(p)))
})


