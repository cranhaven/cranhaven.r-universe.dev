# create binary raster
r <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r)<- runif(n = (108*108))
r[r < 0.5] <- NA
r[r > 0.5] <- 1
# create shp
shp <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(shp)<- runif(n = (108*108))
# ratioOverlap
out <- ratioOverlap(r = r, shp = shp, rasMask = r)

## TESTS
test_that("output type checks", {
  expect_type(out, "list")
  expect_is(out$maskedRange, "RasterLayer")
  expect_is(out$ratio, c("matrix", "array"))
  if(!is.null(out$correlation)){
    expect_is(out$correlation, "numeric")
  }
})
