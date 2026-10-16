
.make_fake_sf <- function(n = 5) {
  sf::st_sf(
    seg_id = seq_len(n),
    predicted_mean = runif(n, 20, 60),
    relative_congestion = rnorm(n),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) {
        sf::st_linestring(matrix(c(i, 0, i + 1, 0), ncol = 2, byrow = TRUE))
      }),
      crs = 4326
    )
  )
}



test_that(".value_registry is well-formed", {
  expect_true(is.list(.value_registry))
  expect_true(all(c("column", "label") %in% names(.value_registry[[1]])))

  for (v in names(.value_registry)) {
    expect_true(is.character(.value_registry[[v]]$column))
    expect_true(is.character(.value_registry[[v]]$label))
    expect_length(.value_registry[[v]]$label, 1)
  }
})


test_that("map_roads_interactive validates inputs", {
  sf_ok <- .make_fake_sf()

  expect_error(map_roads_interactive(list()), "sf object")

  expect_error(map_roads_interactive(sf_ok, value = "bad_value"), "arg")

  expect_error(map_roads_interactive(sf_ok, engine = "plotly"), "leaflet")
})


test_that("map_roads_interactive_layers validates inputs", {
  sf_ok <- .make_fake_sf()

  expect_error(map_roads_interactive_layers(list()), "sf")

  expect_error(map_roads_interactive_layers(sf_ok, values = "bad_value"),
    "No valid traffic measures")
})


test_that("interactive maps fail gracefully without leaflet", {
  sf_ok <- .make_fake_sf()

  if (requireNamespace("leaflet", quietly = TRUE)) {
    skip("leaflet is installed; skipping missing-dependency test")
  }

  expect_error(map_roads_interactive(sf_ok), "leaflet")

  expect_error(map_roads_interactive_layers(sf_ok), "leaflet")
})


test_that("semantic values resolve to expected columns", {
  sf_ok <- .make_fake_sf()

  expect_silent(
    map_roads_interactive(sf_ok, value = "predicted_speed")
  )

  expect_silent(
    map_roads_interactive(sf_ok, value = "relative_congestion")
  )

  expect_silent(
    map_roads_interactive_layers(
      sf_ok, values = c("predicted_speed", "relative_congestion")
    )
  )
})


test_that("missing required columns are handled correctly", {
  sf_bad <- .make_fake_sf()
  sf_bad$predicted_mean <- NULL

  expect_error(
    map_roads_interactive(sf_bad, value = "predicted_speed"),
    "Required column"
  )

  # multi-layer silently skips missing layers
  expect_silent(
    map_roads_interactive_layers(sf_bad,
      values = c("predicted_speed", "relative_congestion")
    )
  )
})


test_that("non-numeric mapped columns are rejected", {
  sf_bad <- .make_fake_sf()
  sf_bad$predicted_mean <- as.character(sf_bad$predicted_mean)

  expect_error(
    map_roads_interactive(sf_bad, value = "predicted_speed"),
    "numeric"
  )
})


test_that("edge cases do not error", {
  sf_one <- .make_fake_sf(n = 1)
  sf_one$predicted_mean <- 30
  sf_one$relative_congestion <- 0

  expect_silent(map_roads_interactive(sf_one))

  expect_silent(map_roads_interactive_layers(sf_one,
      values = c("predicted_speed", "relative_congestion")))
})


test_that("no legacy value_col API remains", {
  expect_false(
    any(grepl("value_col", capture.output(print(map_roads_interactive))))
  )
})


test_that("legend titles are plain character scalars", {
  labs <- vapply(
    names(.value_registry),
    function(v) .value_registry[[v]]$label,
    character(1)
  )

  expect_true(all(vapply(labs, is.character, logical(1))))
  expect_true(all(nchar(labs) > 0))
})


test_that("interactive maps return leaflet widgets", {
  sf_ok <- .make_fake_sf()

  skip_if_not_installed("leaflet")

  m1 <- map_roads_interactive(sf_ok)
  m2 <- map_roads_interactive_layers(sf_ok)

  expect_true(inherits(m1, "leaflet"))
  expect_true(inherits(m2, "leaflet"))
})


test_that("interactive maps set explicit widget dimensions", {
  sf_ok <- .make_fake_sf()

  skip_if_not_installed("leaflet")

  m <- map_roads_interactive(sf_ok)

  expect_true(!is.null(m$width))
  expect_true(!is.null(m$height))
})
