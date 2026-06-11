poly_t <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
                 crs = st_crs('OGC:CRS84'))
points_t <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5))),
                   crs = st_crs('OGC:CRS84'))

test_that("point.poly.dist works", {
  expect_equal(point.poly.dist(points_t, poly_t),
               units::set_units(100135, 'm'),
               tolerance = 1e-3)
})

test_that("point.poly.dist works with arguments", {
  expect_equal(point.poly.dist(points_t, poly_t, max = FALSE),
               units::set_units(61895.43, 'm'),
               tolerance = 1e-3)
  expect_equal(point.poly.dist(points_t, poly_t, max = FALSE,
                               by_element = TRUE)[2],
               units::set_units(61895.43, 'm'),
               tolerance = 1e-3)
})

test_that("point.poly.dist works with projected CRS", {
  expect_equal(point.poly.dist(points_t, projectUTM(poly_t)),
               units::set_units(100201.5, 'm'),
               tolerance = 1e-3)
})
