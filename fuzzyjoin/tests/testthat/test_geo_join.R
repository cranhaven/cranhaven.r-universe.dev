context("geo_join")

set.seed(2016)
latlong1 <- tibble(index1 = 1:500,
                   latitude = rnorm(500, 40),
                   longitude = rnorm(500, 40))

latlong2 <- tibble(index2 = 1:500,
                   latitude = rnorm(500, 40),
                   longitude = rnorm(500, 40))

ll1 <- as.matrix(latlong1[c("longitude", "latitude")])
ll2 <- as.matrix(latlong2[c("longitude", "latitude")])

test_that("geo_inner_join works", {
  j <- latlong1 %>%
    geo_inner_join(latlong2, max_dist = 1, distance_col = "distance")

  expect_true(nrow(j) > 0)

  d <- geosphere::distHaversine(ll1[j$index1, ], ll2[j$index2, ]) / 1609.344

  expect_true(all(d <= 1))
  expect_true(any(d >= .5))
  expect_equal(d, j$distance)

  # test it works even when there are no matches
  j2 <- latlong1 %>%
    geo_inner_join(latlong2, max_dist = .00001, distance_col = "distance")

  expect_equal(nrow(j2), 0)
  expect_true(all(c("latitude.x", "latitude.y",
                    "longitude.x", "longitude.y") %in% colnames(j2)))

  # try other methods, and try each in miles and kilometers
  for (m in c("geo", "cosine", "meeus", "vincentysphere")) {
    for (unit in c("miles", "km")) {
      j3 <- latlong1 %>%
        geo_inner_join(latlong2, method = m, unit = unit,
                       max_dist = 1 + (.609344 * (unit == "km")),
                       distance_col = "distance")

      if (unit == "km") {
        j3$distance <- j3$distance / 1.609344
      }

      # it should be pretty close to Haversine method
      expect_true(all(j3$distance <= 1))
      expect_lt(abs(nrow(j3) - nrow(j)), 2)
    }
  }

  # vincentyellipsoid is very slow
  j4 <- latlong1 %>%
    head(10) %>%
    geo_inner_join(head(latlong2, 10), method = "vincentyellipsoid",
                   max_dist = 1,
                   distance_col = "distance")

  expect_equal(nrow(j4), 0)
})

test_that("geo_inner_join works when lat/lon columns have different names", {
  j <- latlong1 %>%
    geo_inner_join(latlong2, max_dist = 1, distance_col = "distance")

  j2 <- latlong1 %>%
    select(index1, longitude, latitude) %>%
    geo_inner_join(latlong2, max_dist = 1, distance_col = "distance")

  expect_equal(j, j2[colnames(j)])

  l1 <- latlong1 %>%
    select(index1, Lat = latitude, Lon = longitude)

  l2 <- latlong2 %>%
    select(index2, Lon = longitude, Lat = latitude)

  j3 <- geo_inner_join(l1, l2, max_dist = 1, distance_col = "distance")

  expect_equal(j2$index1, j3$index1)
  expect_equal(j2$index2, j3$index2)
})

test_that("geo_inner_join throws an error when more than two columns match", {
  latlongother1 <- mutate(latlong1, other = 1)
  latlongother2 <- mutate(latlong1, other = 2)

  expect_error(geo_inner_join(latlongother1, latlongother2),
               "needs exactly two")
})

test_that("geo joins where there are no overlapping rows still get a distance column", {
  a <- tibble(lat = 1:10, lon = 1:10)
  b <- tibble(lat = 21:30, lon = 21:30)

  result <- geo_left_join(a, b, by = c("lat", "lon"), max_dist = 1, distance_col = "distance")

  expect_equal(colnames(result), c("lat.x", "lon.x", "lat.y", "lon.y", "distance"))
  expect_equal(nrow(result), 10)
  expect_true(all(is.na(result$lat.y)))
  expect_true(all(is.na(result$distance)))

  result <- geo_inner_join(a, b, by = c("lat", "lon"), max_dist = 1, distance_col = "distance")

  expect_equal(colnames(result), c("lat.x", "lon.x", "lat.y", "lon.y", "distance"))
  expect_equal(nrow(result), 0)

  # Don't add it for semi or anti join
  result <- geo_semi_join(a, b, by = c("lat", "lon"), max_dist = 1, distance_col = "distance")
  expect_equal(colnames(result), colnames(a))
  expect_equal(nrow(result), 0)

  result <- geo_anti_join(a, b, by = c("lat", "lon"), max_dist = 1, distance_col = "distance")
  expect_equal(a, result)
})
