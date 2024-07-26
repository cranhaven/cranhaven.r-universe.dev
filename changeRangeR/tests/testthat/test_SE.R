# create binary raster
r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r1)<- runif(n = (108*108))
r1[r1 < 0.5] <- NA
r1[r1 > 0.5] <- 1
r2 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r2)<- runif(n = (108*108))
r2[r2 < 0.5] <- NA
r2[r2 > 0.5] <- 1
r3 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r3)<- runif(n = (108*108))
r3[r3 < 0.5] <- NA
r3[r3 > 0.5] <- 1
rStack <- raster::stack(r1, r2, r3)
# calculate SE
out <- SpeciesEndemism(rStack)

## TESTS
test_that("output type checks", {
  expect_s4_class(out, "RasterLayer")
})
