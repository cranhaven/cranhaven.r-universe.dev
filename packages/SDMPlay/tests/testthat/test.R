a <- 9
expect_that(a, is_less_than(10))

# test example Brisaster
library(SDMPlay)
data('Brisaster.antarcticus')
x <- brisaster.antarcticus
expect_that(nrow(x), equals(43))

# test delim.area function
data('predictors2005_2012')
envi <- predictors2005_2012

r <- SDMPlay:::delim.area(predictors = envi,
                          longmin = 70,longmax = 75, latmin = -50,latmax = -46,
                          interval = c(0,-1000))

expect_that(raster::extent(r)[1], equals(70))
expect_that(raster::extent(r)[2], equals(75))
expect_that(raster::extent(r)[3], equals(-50))
expect_that(raster::extent(r)[4], equals(-46))

# test Southern Ocean environmental layers
data("depth_SO")
expect_that(raster::ncell(depth_SO), equals(1260000))




