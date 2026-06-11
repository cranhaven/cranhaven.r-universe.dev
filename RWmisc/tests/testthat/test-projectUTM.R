library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
poly_t <- st_sfc(st_polygon(list(rbind(c(10,-10), c(10,-20), c(20,-20),
                                       c(20,-10), c(10,-10)))),
                 crs = st_crs('OGC:CRS84'))

test_that("projectUTM works with sfc_POLYGON", {
  expect_equal(st_crs(projectUTM(st_cast(st_geometry(nc[1,]),
                                         "POLYGON")))[["input"]],
               "+proj=utm +zone=17")
})

test_that("projectUTM works with sfc_MULTIPOLYGON", {
  expect_equal(st_crs(projectUTM(nc))[["input"]], "+proj=utm +zone=17")
})


test_that("projectUTM works with sfc_POINT", {
  expect_equal(st_crs(projectUTM(st_centroid(st_geometry(nc))))[["input"]],
               "+proj=utm +zone=17")
})

test_that("projectUTM works with sfc_POINT", {
  expect_equal(st_crs(projectUTM(st_cast(st_centroid(st_geometry(nc)),
                                         'MULTIPOINT')))[["input"]],
               "+proj=utm +zone=17")
})

test_that("projectUTM works with southern latitude", {
  expect_equal(st_crs(projectUTM(poly_t))[["input"]], "+proj=utm +south +zone=33")
})

test_that("projectUTM works with sf and sfc", {
  expect_equal(st_crs(projectUTM(st_geometry(nc))), st_crs(projectUTM(nc)))
})
