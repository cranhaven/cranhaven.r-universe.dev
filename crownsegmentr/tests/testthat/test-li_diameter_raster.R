test_that("li_diameter_raster works", {
  test_point_cloud_file_path <- system.file(
    "extdata", "MixedConifer.laz",
    package = "lidR"
  )
  test_point_cloud_header <- lidR::readLASheader(test_point_cloud_file_path)
  test_point_cloud <- lidR::clip_rectangle(
    lidR::readLAS(test_point_cloud_file_path),
    xleft = test_point_cloud_header@PHB$`Min X`,
    ybottom = test_point_cloud_header@PHB$`Min Y`,
    xright = test_point_cloud_header@PHB$`Min X` + 50,
    ytop = test_point_cloud_header@PHB$`Min Y` + 50
  )
  range(test_point_cloud@data$Z)

  testthat::expect_silent({
    li_raster1 <- li_diameter_raster(test_point_cloud)

    assert_that_raster_fits_point_cloud(
      li_raster1,
      test_point_cloud
    )
  })

  # test with modified parameters
  testthat::expect_silent({
    li_raster2 <- li_diameter_raster(test_point_cloud,
      crown_diameter_constant = 10,
      limits = c(0.3, 0.4),
      smoothing_radius = 0
    )
  })

  # test if function can be called from within segment_tree_crowns
  segmented_point_cloud1 <- segment_tree_crowns(
    test_point_cloud,
    "li2012",
    0.4
  )
  segmented_point_cloud2 <- segment_tree_crowns(
    test_point_cloud,
    li_raster1,
    0.4
  )
  testthat::expect_equal(segmented_point_cloud1, segmented_point_cloud2)
})
