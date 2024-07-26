# generate occurrences
ras1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(ras1)<- runif(n = (108*108))
occs <- dismo::randomPoints(ras1, 4)
# create mcp
out <- mcp(occs)

## TESTS
test_that("output type checks", {
  expect_is(out, "SpatialPolygons")
})
