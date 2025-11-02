test_that("the arguments are of the right class", {
  # Load the R objects to be used in tests
  x <- watersheds
  y <- DEM
  dummy <- watersheds_df
  #Test errors
  expect_error(check_arguments(dummy, y), "X has to be a spatial object", fixed=TRUE)
  expect_error(check_arguments(x, dummy), "y has to be a raster object", fixed=TRUE)
})

