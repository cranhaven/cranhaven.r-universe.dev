r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r1)<- runif(n = (108*108))
r1[r1 < 0.5] <- NA
r1[!is.na(r1)] <- 1
out <- buildRMM(rmm = NULL, binaryRange = r1)


## TEST
test_that("output type checks", {
  expect_type(out, "list")
  expect_is(out$postprocess$inputs$binaryRange, "RasterLayer")
  }
)
