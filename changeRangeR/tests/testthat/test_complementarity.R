# create raster
ras1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(ras1)<- runif(n = (108*108))
ras1[ras1 < 0.5] <- NA
ras1[!is.na(ras1)] <- 1
# create ras1mask
ras1mask <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(ras1mask)<- runif(n = (108*108))
ras1mask[ras1mask < 0.15] <- NA
ras1mask[!is.na(ras1mask)] <- 1
# complementarity
out <- complementarity(ras1, ras1mask)

## TESTS
test_that("output type checks", {
  expect_type(out, "list")
  expect_is(out$Percent_of_Total, "numeric")
  expect_is(out$Percent_unique_values, "data.frame")
})
