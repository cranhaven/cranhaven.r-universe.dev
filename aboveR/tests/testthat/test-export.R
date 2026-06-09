# Tests for export_landxml() and export_stl()

dem_path <- system.file("extdata/dem_before.tif", package = "aboveR")

test_that("export_landxml() creates valid XML file", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  out <- file.path(tempdir(), "test_surface.xml")
  on.exit(unlink(out), add = TRUE)

  result <- export_landxml(dem, out, surface_name = "Test")

  expect_equal(result, out)
  expect_true(file.exists(out))

  # Check XML structure
  content <- readLines(out, n = 5)
  expect_true(any(grepl("LandXML", content)))
  expect_true(any(grepl("Test", content)))
})

test_that("export_landxml() supports decimation", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  out1 <- file.path(tempdir(), "full.xml")
  out2 <- file.path(tempdir(), "decimated.xml")
  on.exit(unlink(c(out1, out2)), add = TRUE)

  export_landxml(dem, out1, decimate = 1)
  export_landxml(dem, out2, decimate = 5)

  # Decimated file should be smaller
  expect_true(file.size(out2) < file.size(out1))
})

test_that("export_landxml() validates input", {
  expect_error(export_landxml("not_raster", "out.xml"), "SpatRaster")
})

test_that("export_stl() creates valid STL file", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  out <- file.path(tempdir(), "test_terrain.stl")
  on.exit(unlink(out), add = TRUE)

  result <- export_stl(dem, out, exaggeration = 2)

  expect_equal(result, out)
  expect_true(file.exists(out))

  # Check STL structure
  content <- readLines(out, n = 2)
  expect_true(any(grepl("solid terrain", content)))
})

test_that("export_stl() validates exaggeration", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  expect_error(export_stl(dem, "out.stl", exaggeration = 0), "must be > 0")
})
