# Test that all exported functions exist and are callable

test_that("all core analysis functions are exported", {
  expect_true(is.function(terrain_change))
  expect_true(is.function(change_by_zone))
  expect_true(is.function(estimate_volume))
  expect_true(is.function(impoundment_curve))
  expect_true(is.function(terrain_profile))
  expect_true(is.function(boundary_terrain_profile))
  expect_true(is.function(classify_highwall))
  expect_true(is.function(bench_detection))
  expect_true(is.function(reclamation_progress))
  expect_true(is.function(surface_roughness))
  expect_true(is.function(detect_channels))
  expect_true(is.function(pond_sedimentation))
})

test_that("all terrain derivative functions are exported", {
  expect_true(is.function(slope_aspect))
  expect_true(is.function(hillshade))
  expect_true(is.function(contour_lines))
  expect_true(is.function(zonal_stats))
})

test_that("all flood analysis functions are exported", {
  expect_true(is.function(flood_inundation))
  expect_true(is.function(flood_depth))
  expect_true(is.function(height_above_drainage))
})

test_that("all export functions are exported", {
  expect_true(is.function(export_landxml))
  expect_true(is.function(export_stl))
})

test_that("all color ramp functions are exported", {
  expect_true(is.function(change_colors))
  expect_true(is.function(terrain_colors))
  expect_true(is.function(flood_colors))
})

test_that("all KyFromAbove functions are exported", {
  expect_true(is.function(kfa_find_tiles))
  expect_true(is.function(kfa_tile_index))
  expect_true(is.function(kfa_read_dem))
  expect_true(is.function(kfa_read_pointcloud))
  expect_true(is.function(kfa_read_ortho))
  expect_true(is.function(kfa_stac_search))
  expect_true(is.function(kfa_county_bbox))
  expect_true(is.function(kfa_list_counties))
})
