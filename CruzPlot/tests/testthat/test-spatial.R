library(mapdata)

test_that("map databases are present and map calls work", {
  skip_if_not_installed("maps")

  x.try1 <- try(maps::map("world", xlim = c(-135, -110), ylim = c(30, 50), plot = FALSE),
                silent = TRUE)
  x.try2 <- try(maps::map("world2", xlim = c(170, 220), ylim = c(30, 50), plot = FALSE),
                silent = TRUE)
  x.try3 <- try(maps::map("worldHires", xlim = c(-135, -110), ylim = c(30, 50), plot = FALSE),
                silent = TRUE)
  x.try4 <- try(maps::map("world2Hires", xlim = c(170, 220), ylim = c(30, 50), plot = FALSE),
                silent = TRUE)

  expect_identical(shiny::isTruthy(x.try1), TRUE)
  expect_identical(shiny::isTruthy(x.try2), TRUE)
  expect_identical(shiny::isTruthy(x.try3), TRUE)
  expect_identical(shiny::isTruthy(x.try4), TRUE)
})


test_that("geosphere functions work as expected", {
  skip_if_not_installed("geosphere")

  dist.ex <- geosphere::distVincentyEllipsoid(c(0, 0), c(90, 90))

  # Test 0 distance
  dist.0 <- geosphere::distVincentyEllipsoid(
    cbind(c(-130, 0, 130), c(-45, 0, 45)),
    cbind(c(-130, 0, 130), c(-45, 0, 45))
  )

  expect_equal(round(dist.ex, 0), 10001966)
  expect_equal(dist.0, c(0, 0, 0))

  # Test that distance at equator is as expected
  mat.eq <- matrix(c(c(-130, 130, 0, 0)), ncol = 2)
  dpt.eq <- round(unname(geosphere::destPoint(mat.eq, 90, 111.32 * 1000)), 4)

  # 0 distance is as expected
  mat.0 <- matrix(c(c(-130, 130, 25, 25)), ncol = 2)
  dpt.0 <- unname(geosphere::destPoint(mat.0, 90, 0))

  expect_equal(mat.0, dpt.0)
  expect_equal(mat.eq + matrix(c(1, 1, 0, 0), ncol = 2), dpt.eq)
})
