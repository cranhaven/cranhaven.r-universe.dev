# Tests for slope_aspect(), hillshade(), contour_lines(), zonal_stats()

dem_path <- system.file("extdata/dem_before.tif", package = "aboveR")
zones_path <- system.file("extdata/zones.gpkg", package = "aboveR")

test_that("slope_aspect() returns two-layer SpatRaster", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- slope_aspect(dem)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 2)
  expect_true("slope" %in% names(result))
  expect_true("aspect" %in% names(result))
})

test_that("slope_aspect() supports percent units", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- slope_aspect(dem, units = "percent")
  vals <- terra::values(result[["slope"]])[, 1]
  vals <- vals[!is.na(vals)]

  # Percent slope can be > 100 for steep terrain

  expect_true(all(vals >= 0))
})

test_that("slope_aspect() validates input", {
  expect_error(slope_aspect("not_a_raster"), "SpatRaster")
})

test_that("hillshade() returns single-layer SpatRaster", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- hillshade(dem)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 1)
  expect_equal(names(result), "hillshade")
})

test_that("hillshade() accepts custom angles", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- hillshade(dem, azimuth = 180, altitude = 30)
  expect_s4_class(result, "SpatRaster")
})

test_that("contour_lines() returns sf with elevation column", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- contour_lines(dem, interval = 5)

  expect_s3_class(result, "sf")
  expect_true("elevation" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("contour_lines() accepts specific levels", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- contour_lines(dem, levels = c(300, 305, 310))
  expect_s3_class(result, "sf")
})

test_that("zonal_stats() returns sf with statistics", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  zones <- sf::st_read(zones_path, quiet = TRUE)

  result <- zonal_stats(dem, zones, id_field = "zone_id")

  expect_s3_class(result, "sf")
  expect_true(all(c("min", "max", "mean", "median", "sd", "cell_count", "area_m2")
                   %in% names(result)))
  expect_equal(nrow(result), 2)
})

test_that("zonal_stats() errors on missing id_field", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  zones <- sf::st_read(zones_path, quiet = TRUE)

  expect_error(zonal_stats(dem, zones, id_field = "nonexistent"), "not found")
})
