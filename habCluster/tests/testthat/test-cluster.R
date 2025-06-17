
test_that("cluster method", {
  library(habCluster)
  require(stars)
  require(sf)
  # read data
  hsi.file = system.file("extdata","wolf3_int.tif",package="habCluster")
  wolf = read_stars(hsi.file)

  clst = cluster(wolf, method = cluster_leiden, cellsize = 80000)

  # check results are right objects
  testthat::expect_s3_class(clst$boundary,"sf")
  testthat::expect_s3_class(clst$communities ,"communities")

})
