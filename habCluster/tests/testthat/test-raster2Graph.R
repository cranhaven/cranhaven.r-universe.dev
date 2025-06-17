
test_that("raster2Graph method", {
  require(stars)
  # read data
  hsi.file = system.file("extdata","wolf3_int.tif",package="habCluster")
  wolf = read_stars(hsi.file)

  g = raster2Graph(wolf, 80000)

  # check results are right objects
  testthat::expect_s3_class(g$raster,"stars")
  testthat::expect_s3_class(g$graph ,"igraph")
})
