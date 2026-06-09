library(testthat)
library(lidR)
library(vostokR)

library(testthat)
library(lidR)
library(vostokR)

# Load sample data for all tests
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
test_las <- readLAS(LASfile)

test_that("add_normals adds normal vectors to point cloud", {
  result <- add_normals(test_las)
  
  # Check that normal vectors were added
  expect_true(all(c("nx", "ny", "nz") %in% names(result@data)))
  expect_equal(nrow(result@data), nrow(test_las@data))
  
  # Check that normals are unit vectors
  normal_lengths <- sqrt(result@data$nx^2 + result@data$ny^2 + result@data$nz^2)
  expect_true(all(abs(normal_lengths - 1) < 0.001))
})

test_that("calculate_solar_potential validates inputs", {
  # Should error without normal vectors
  expect_error(
    calculate_solar_potential(test_las,
                            year = 2025,
                            day_start = 1,
                            day_end = 365,
                            day_step = 30,
                            minute_step = 30,
                            min_sun_angle = 5,
                            voxel_size = 1,
                            lat = 47.6062,
                            lon = -122.3321,
                            timezone = -8),
    "Point cloud must contain normal vectors"
  )
})

test_that("calculate_solar_potential produces reasonable results", {
  # Add normals first
  las_with_normals <- add_normals(test_las)
  
  result <- calculate_solar_potential(las_with_normals,
                                   year = 2025,
                                   day_start = 172,  # Summer solstice
                                   day_end = 172,
                                   day_step = 1,
                                   minute_step = 60,
                                   min_sun_angle = 5,
                                   voxel_size = 1,
                                   lat = 47.6062,  # Seattle
                                   lon = -122.3321,
                                   timezone = -8)
  
  # Check that solar potential was added
  expect_true("solar_potential" %in% names(result@data))
  
  # Check for reasonable value ranges
  expect_true(all(result@data$solar_potential >= 0))  # Should be non-negative
  expect_true(all(result@data$solar_potential < 10000))  # Reasonable upper bound for daily solar potential
  
  # Points higher up should generally receive more solar potential
  high_points <- result@data$Z > quantile(result@data$Z, 0.9)  # Top 10% by height
  low_points <- result@data$Z < quantile(result@data$Z, 0.1)   # Bottom 10% by height
  
  mean_high <- mean(result@data$solar_potential[high_points])
  mean_low <- mean(result@data$solar_potential[low_points])
  
  expect_gt(mean_high, mean_low)  # Higher points should get more sun
})
