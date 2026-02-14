test_that("plot_predicted returns a ggplot with an sf layer and labeled scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  roads <- sf::st_sf(
    segment_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 1, 2, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  mu <- matrix(
    c(10, 11, 12,
      20, 21, 22,
      30, 31, 32,
      40, 41, 42),
    nrow = 4, byrow = TRUE
  )

  fit <- structure(
    list(draws = list(mu = mu), outcome_label = "Predicted speed"),
    class = "traffic_fit"
  )

  roads_copy <- roads
  p <- plot_predicted(fit, roads)

  expect_s3_class(p, "ggplot")
  expect_false("predicted" %in% names(roads_copy))

  expect_true(length(p$layers) >= 1)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSf"), logical(1))))

  # scale name may not be the first scale; search all of them
  scale_names <- vapply(p$scales$scales, function(s) s$name %||% "", character(1))
  expect_true(any(grepl("Predicted speed", scale_names, fixed = TRUE)))
})

test_that("plot_relative_congestion returns ggplot with sf layer and labeled scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  roads <- sf::st_sf(
    segment_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 1, 2, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  x <- matrix(
    c(-1, 0, 1,
      -2, 0, 2,
      -1, 0, 1,
      -3, 0, 3),
    nrow = 4, byrow = TRUE
  )

  fit <- structure(
    list(draws = list(x = x), outcome_label = "ignored"),
    class = "traffic_fit"
  )

  roads_copy <- roads
  p <- plot_relative_congestion(fit, roads)

  expect_s3_class(p, "ggplot")
  expect_false("relative_congestion" %in% names(roads_copy))

  expect_true(length(p$layers) >= 1)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSf"), logical(1))))

  scale_names <- vapply(p$scales$scales, function(s) s$name %||% "", character(1))
  expect_true(any(grepl("Relative congestion", scale_names, fixed = TRUE)))
})

test_that("plot_predicted rejects non-sf roads and missing geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  fit <- structure(
    list(draws = list(mu = matrix(rnorm(6), nrow = 2)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_predicted(fit, data.frame(segment_id = 1:3)),
    "sf|geometry",
    ignore.case = TRUE
  )

  roads <- sf::st_sf(
    segment_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2, 0, 3, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )
  roads_nogeo <- sf::st_drop_geometry(roads)

  expect_error(
    plot_predicted(fit, roads_nogeo),
    "geometry|sf",
    ignore.case = TRUE
  )
})

test_that("plot_relative_congestion rejects non-sf roads and missing geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  fit <- structure(
    list(draws = list(x = matrix(rnorm(6), nrow = 2)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_relative_congestion(fit, data.frame(segment_id = 1:3)),
    "sf|geometry",
    ignore.case = TRUE
  )

  roads <- sf::st_sf(
    segment_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2, 0, 3, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )
  roads_nogeo <- sf::st_drop_geometry(roads)

  expect_error(
    plot_relative_congestion(fit, roads_nogeo),
    "geometry|sf",
    ignore.case = TRUE
  )
})



