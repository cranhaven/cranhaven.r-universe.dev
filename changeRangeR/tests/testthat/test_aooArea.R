# create raster
r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r1)<- runif(n = (108*108))
r1[r1 < 0.5] <- NA
r1[!is.na(r1)] <- 1
# calculate aooArea
out <- AOOarea(r = r1)

## TESTS
test_that("output type checks", {
  expect_type(out, "list")
  expect_is(out$area, "data.frame")
  expect_is(out$aooRaster, "RasterLayer")
  if(!is.null(out$aooPixels)){
    expect_is(out$aooPixels, "RasterLayer")
  }
})
