polys_t <- st_sfc(list(st_polygon(list(rbind(c(2,2), c(2,6), c(6,6),
                                             c(6,2), c(2, 2)))),
                       st_polygon(list(rbind(c(8,8), c(4,8), c(4,4),
                                             c(8,4), c(8,8))))),
                  crs = st_crs('OGC:CRS84'))

raster_t <- raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10,
                   vals = 1:100, crs = CRS(st_crs(polys_t)$proj4string))

test_that("overlap.weight works with sfc_POLYGON", {
  expect_equal(overlap.weight(raster_t, polys_t, warn = FALSE)[4,4], 28)
})

test_that("overlap.weight works with sf", {
  expect_equal(overlap.weight(raster_t, st_sf(data.frame(a = 1:2,
                                                         geometry = polys_t)),
                              warn = FALSE)[4,4], 28)
})

test_that("overlap.weight works with count", {
  expect_equal(overlap.weight(raster_t, polys_t, count = TRUE,
                              warn = FALSE)[4,4], 2)
})

test_that("overlap.weight warns about different extents", {
  expect_warning(overlap.weight(raster_t, polys_t, warn = TRUE))
})
