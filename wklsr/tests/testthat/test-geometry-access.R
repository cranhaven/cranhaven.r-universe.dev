test_that("WKT returns correct format for San Francisco", {
  skip_if_offline()

  sf <- wkls$us$ca$sanfrancisco
  geom <- sf$wkt()

  expect_type(geom, "character")
  expect_true(startsWith(geom, "MULTIPOLYGON"))
})

test_that("WKB returns raw bytes for San Francisco", {
  skip_if_offline()

  sf <- wkls$us$ca$sanfrancisco
  geom <- sf$wkb()

  # In R, WKB should return raw bytes
  expect_type(geom, "raw")
})

test_that("HexWKB returns character string for San Francisco", {
  skip_if_offline()

  sf <- wkls$us$ca$sanfrancisco
  geom <- sf$hexwkb()

  expect_type(geom, "character")
  # HexWKB should be a hexadecimal string
  expect_true(grepl("^[0-9A-Fa-f]+$", geom))
})

test_that("GeoJSON returns valid JSON for San Francisco", {
  skip_if_offline()

  sf <- wkls$us$ca$sanfrancisco
  geom <- sf$geojson()

  expect_type(geom, "character")

  # Parse JSON to verify it's valid
  parsed <- jsonlite::fromJSON(geom)
  expect_type(parsed, "list")
  expect_true("type" %in% names(parsed) || "geometry" %in% names(parsed))
})

test_that("SVG returns character string for San Francisco", {
  skip_if_offline()

  sf <- wkls$us$ca$sanfrancisco
  geom <- sf$svg()

  expect_type(geom, "character")
})

test_that("countries without regions work correctly - Stoney Ridge", {
  skip_if_offline()

  stoneyridge <- wkls$fk$stoneyridge
  geom <- stoneyridge$wkt()

  expect_type(geom, "character")
  expect_true(startsWith(geom, "POLYGON"))
})

test_that("all geometry formats work for San Francisco", {
  skip_if_offline()

  sf <- wkls$us$ca$sanfrancisco

  # Test all formats in one test
  wkt <- sf$wkt()
  expect_type(wkt, "character")
  expect_true(startsWith(wkt, "MULTIPOLYGON"))

  wkb <- sf$wkb()
  expect_type(wkb, "raw")

  hexwkb <- sf$hexwkb()
  expect_type(hexwkb, "character")

  geojson <- sf$geojson()
  expect_type(geojson, "character")
  parsed_json <- jsonlite::fromJSON(geojson)
  expect_type(parsed_json, "list")

  svg <- sf$svg()
  expect_type(svg, "character")
})
