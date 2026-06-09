test_that("sky_grid_segmentation() works", {
  z <- zenith_image(1000, lens())
  a <- azimuth_image(z)
  expect_equal(max(sky_grid_segmentation(z, a, 10)[], na.rm = TRUE),
               36009)
})
