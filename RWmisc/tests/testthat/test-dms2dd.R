test_that("dms2dd works with heterogeneous input", {
  ll <- data.frame(lon = c("-122° 19' 55\"",
                           "71° 3' 32\" W"),
                   lat = c("47° 36' 22\"",
                           "42° 21' 36\" N"),
                   stringsAsFactors = FALSE)
  expect_equal(dms2dd(ll[, 'lon'], ll[, 'lat'])[1,2], 47.60611, tolerance = 1e-3)
})

test_that("dms2dd works with 0 values", {
  ll <- data.frame(lon = c("71° 0' 32\" W"),
                   lat = c("0° 21' 36\" N"),
                   stringsAsFactors = FALSE)
  expect_equal(dms2dd(ll[, 'lon'], ll[, 'lat'])[1,1], -71.00889, tolerance = 1e-3)
})
