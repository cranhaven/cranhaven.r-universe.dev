#contourPoints test

#load test data
hull <- sf::read_sf(system.file("extdata", "example_contour.shp", package = 'rLakeHabitat')) %>%
  dplyr::mutate(z = 1)

#create incorrect data type
wrong <- as.data.frame(matrix(NA, nrow=1,ncol=3))
colnames(wrong) <- c('x', 'y', 'z')
wrong[1,1:3] <- "1"

wrongvect <- list(wrong)

#input check
test_that("contourPoints input data check", {
  expect_error(contourPoints(wrongvect, depths = "z", geometry = "geometry", density = 10), info = "input object must be a polygon or multipolygon shapefile")
  expect_error(contourPoints(hull, depths = 1, geometry = "geometry", density = 10), info = "depths must be a character giving the latitude column name")
  expect_error(contourPoints(hull, depths = "z", geometry = 1, density = 10), info = "geometry must be a character giving the depth column name")
  expect_error(contourPoints(hull, depths = "no", geometry = "geometry", density = 10), info = "The value of 'depths' does not appear to be a valid column name")
  expect_error(contourPoints(hull, depths = "z", geometry = "no", density = 10), info = "The value of 'geometry' does not appear to be a valid column name")
  expect_error(contourPoints(wrong, depths = "z", geometry = "geometry", density = 10), info = "data in depths column is not formatted as numeric")
  expect_error(contourPoints(hull, depths = "z", geometry = "z", density = 10), info = "data in geometry column is not formatted as sfc")
  expect_error(contourPoints(hull, depths = "z", geometry = "geometry", density = no), info = "density must be a numeric value")
})

#output check
test_that("contourPoints output check", {
  expect_s3_class(contourPoints(hull, depths = "z", geometry = "geometry", density = 50), class = "data.frame")
})
