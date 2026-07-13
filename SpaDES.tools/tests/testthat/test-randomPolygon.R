test_that("randomPolygon: does not work properly", {
  # no need for `raster` testing as this is using randomPolygon
  testInit("terra")

  set.seed(1234) ## TODO: some seeds produce failing area test below!!
  # latLong <- crs("epsg:4326")
  latLong <- terra::crs("+proj=longlat +datum=WGS84 +no_defs")

  area <- 1e4
  center <- cbind(-110, 59)
  poly1 <- randomPolygon(center, area = area)
  if (interactive()) {
    terra::plot(poly1)
  }

  poly1InUTM <- project(poly1, utmCRS(poly1))
  ## check that polygon area approximately matches that given by hectares
  polyArea <- expanse(poly1InUTM)

  ## scale the test to area. This means in current area of 1e4, polygon area should be more than 2500; reasonable
  expect_true(base::abs(base::abs(polyArea - area)) <  (area / 4))  ## TODO: why is this area/4?

  ## check that polygon center is approximately centered on x
  centerSP <- vect(center, crs = latLong)
  centerSP_UTM <- project(centerSP, crs(poly1InUTM))
  polyCenter <- terra::centroids(poly1InUTM)

  ## scale the test to area. This means in current area of 1e4, that centroid is 100 m from center; reasonable
  expect_true(terra::distance(centerSP_UTM, polyCenter) < (area/100))
})
