

test_that("plot_roads_static validates sf_aug is sf", {
  expect_error(
    trafficCAR::plot_roads_static(data.frame(x = 1)),
    "sf_aug.*sf",
    ignore.case = TRUE
  )
})



test_that("plot_roads_static validates value choices via match.arg", {
  sf_aug <- sf::st_sf(
    predicted_speed = 10,
    predicted_volume = 100,
    relative_congestion = 0.2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  expect_error(
    trafficCAR::plot_roads_static(sf_aug, value = "nope"),
    "should be one of|one of",
    ignore.case = TRUE
  )
})


test_that("plot_roads_static errors if required mapped column missing", {
  reg <- get(".value_registry", envir = asNamespace("trafficCAR"))
  spec <- reg[["predicted_speed"]]
  col  <- spec$column

  sf_aug <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  expect_false(col %in% names(sf_aug))

  expect_error(
    trafficCAR::plot_roads_static(sf_aug, value = "predicted_speed"),
    "Required column",
    ignore.case = FALSE
  )
})



test_that("plot_roads_static errors if mapped column is not numeric", {
  reg <- get(".value_registry", envir = asNamespace("trafficCAR"))
  spec <- reg[["predicted_speed"]]
  col  <- spec$column

  sf_aug <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )
  sf_aug[[col]] <- "not numeric"

  expect_error(
    trafficCAR::plot_roads_static(sf_aug, value = "predicted_speed"),
    "numeric",
    ignore.case = TRUE
  )
})



test_that("plot_roads_static returns a ggplot with expected layers and labels", {
  skip_if_not_installed("ggplot2")

  reg <- get(".value_registry", envir = asNamespace("trafficCAR"))

  for (val in c("predicted_speed", "predicted_volume", "relative_congestion")) {
    spec <- reg[[val]]
    col  <- spec$column
    lab  <- spec$label

    sf_aug <- sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE))
      ),
      crs = 4326
    )
    sf_aug[[col]] <- c(1, NA_real_)

    p <- trafficCAR::plot_roads_static(sf_aug, value = val)

    expect_s3_class(p, "ggplot")
    expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSf"), logical(1))))
    expect_equal(p$labels$title, lab)

    # legend title comes from the scale name
    sc_names <- vapply(p$scales$scales, function(s) s$name %||% NA_character_, character(1))
    expect_true(any(!is.na(sc_names) & sc_names == lab))

    # build should succeed even with NA
    b <- ggplot2::ggplot_build(p)
    expect_true("colour" %in% names(b$data[[1]]))
  }
})


test_that("plot_roads_static accepts NA values in mapped column", {
  skip_if_not_installed("ggplot2")

  reg <- get(".value_registry", envir = asNamespace("trafficCAR"))
  spec <- reg[["predicted_speed"]]
  col  <- spec$column

  sf_aug <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )
  sf_aug[[col]] <- c(10, NA_real_)

  expect_silent({
    p <- trafficCAR::plot_roads_static(sf_aug, value = "predicted_speed")
    expect_s3_class(p, "ggplot")
  })
})


