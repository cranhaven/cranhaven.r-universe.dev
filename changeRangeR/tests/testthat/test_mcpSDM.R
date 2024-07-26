library(sf)

# create continuous raster
p <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(p)<- runif(n = (108*108))
raster::crs(p) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# create occurrences
xy <- dismo::randomPoints(p, 4)
# create original convex hull
ch.orig <- mcp(xy, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# set threshold
thr <- 0.5
# mcpSDM
sf_use_s2(FALSE)
out <- mcpSDM(p, xy, ch.orig, thr)


## TEST
test_that("output type checks", {
  expect_type(out, "list")
  expect_is(out$jsi, "numeric")
  expect_is(out$thr, "numeric")
  expect_is(out$ov.pts, "numeric")
  expect_is(out$best.fit, "SpatialPolygons")
  expect_is(out$best.fit.ind, "integer")
})
