# Convert raster stack to points
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
Allxy <- raster::rasterToPoints(rStack)
# Drop first 2 columns (lat/long)
sites <- Allxy[,2:ncol(Allxy)]
sites[is.na(sites)] <- 0
library(ape)
tree <- rtree(n = 3)
tree$tip.label <- names(rStack)
out <- calc_PE(phylo.tree = tree, sites_x_tips = sites, presence = "presence")

## TEST
test_that("output type checks", {
 # expect_type(out, "data.frame")
  expect_is(out$site, "numeric")
  expect_is(out$PE, "numeric")
})
