#create rasters
r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(r1)<- runif(n = (108*108))
r2 <- r1 >=0.5
r2[r2<1] <- NA
r3 <- r1 >=0.75
r3[r3<1] <- NA
# Create r
r <- list(r2, r3)
# create r.names
r.names <- c('scenario 1', 'scenario 2')
# create futures
coords <- dismo::randomPoints(r1, 4)
future <- sp::Polygon(coords)
future <- sp::SpatialPolygons(list(sp::Polygons(list(future), ID = "a")))
futures <- list(future, future)
futures.names <- list("fut1", "fut2")
# set field and category
field = "a"
category = "All"
# run function
out <- futureOverlap(r, futures, field, category, r.names, futures.names)


## TESTS
test_that("output type checks", {
  expect_is(out, "data.frame")
})
