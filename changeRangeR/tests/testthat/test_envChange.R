# create rStack
r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r1)<- runif(n = (108*108))
r2 <-  raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r2)<- runif(n = (108*108))
rStack <-  raster::stack(r1,r2)
# create binaryRange
binaryRange <- raster::crop(r1, raster::extent(c(-50, 50, 0, 90)))
binaryRange <- raster::extend(binaryRange, r1)
binaryRange[!is.na(binaryRange)] <- 1
# set threshold
threshold <- 0.5
# set bound
bound <- "upper"
# Run function
out <- envChange(rStack = rStack, binaryRange = binaryRange, threshold = threshold, bound = bound)


## TESTS
test_that("output type checks", {
  expect_type(out, "list")
  expect_is(out$Area, c("matrix", "array"))
  expect_is(out$masks, "RasterStack")
})
