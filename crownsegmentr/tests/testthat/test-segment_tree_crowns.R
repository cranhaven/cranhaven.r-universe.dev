# This file is part of crownsegmentr, an R package for identifying tree crowns
# within 3D point clouds.
#
# Copyright (C) 2025 Leon Steinmeier, Nikolai Knapp, UFZ Leipzig
# Contact: timon.miesner@thuenen.de
#
# crownsegmentr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# crownsegmentr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with crownsegmentr in a file called "COPYING". If not,
# see <http://www.gnu.org/licenses/>.

# data.frame/data.table method --------------------------------

test_that("the data.frame/data.table method works", {
  # Pseudo point cloud with just one point
  pseudo_point_cloud <- data.table::data.table(X = 1, Y = 1, Z = 1)
  # Also load real point cloud data
  test_point_cloud_file_path <- system.file(
    "extdata", "MixedConifer.laz",
    package = "lidR"
  )
  test_point_cloud_header <- lidR::readLASheader(test_point_cloud_file_path)
  test_point_cloud <- lidR::clip_rectangle(
    lidR::readLAS(test_point_cloud_file_path),
    xleft = test_point_cloud_header@PHB$`Min X`,
    ybottom = test_point_cloud_header@PHB$`Min Y`,
    xright = test_point_cloud_header@PHB$`Min X` + 50,
    ytop = test_point_cloud_header@PHB$`Min Y` + 50
  )

  # test the most simple form
  segmented_points <- segment_tree_crowns(
    point_cloud = as.data.frame(pseudo_point_cloud),
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5
  )
  expect_s3_class(segmented_points, class = "data.frame", exact = TRUE)
  expect_named(segmented_points,
    expected = c(names(pseudo_point_cloud), "crown_id")
  )
  # test with data.table and custom column name
  segmented_points <- segment_tree_crowns(
    point_cloud = test_point_cloud@data,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    crown_id_column_name = "test id col name"
  )
  expect_s3_class(segmented_points,
    class = c("data.table", "data.frame"),
    exact = TRUE
  )
  expect_named(segmented_points,
    expected = c(names(test_point_cloud@data), "test id col name")
  )

  # TODO maybe make this a test for the validate_crown_id_column_name function
  expect_error(
    segment_tree_crowns(
      point_cloud = test_point_cloud@data[1],
      crown_diameter_to_tree_height = 0.25,
      crown_length_to_tree_height = 0.5,
      crown_id_column_name = "treeID"
    ),
    regexp = paste0(
      "^The point cloud data already has a column/attribute with the name ",
      "\"treeID\"\\. Please either choose a different argument for the ",
      "crown_id_column_name parameter or modify the point cloud data\\.$"
    )
  )

  # Test varying different arguments
  segment_tree_crowns(
    point_cloud = test_point_cloud@data,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    crown_diameter_constant = 1,
    crown_length_constant = 2,
    verbose = FALSE,
    centroid_convergence_distance = 0.1,
    max_iterations_per_point = 50,
    dbscan_neighborhood_radius = 1,
    min_num_points_per_crown = 10
  )

  # Also return all centroids and vary some arguments
  segmentation_res <- segment_tree_crowns(
    point_cloud = test_point_cloud@data,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    max_iterations_per_point = 1000,
    min_num_points_per_crown = 10,
    also_return_terminal_centroids = TRUE,
    also_return_all_centroids = TRUE
  )

  # Plot segmentation results (for manual testing only)
  # crown_colors <- c(
  #   "#DDDDDD",
  #   lidR::random.colors(
  #     n = data.table::uniqueN(segmentation_res$segmented_point_cloud$crown_id)
  #     - 1
  #   )
  # )
  #
  # # Plot the segmented point cloud
  # lidR::plot(
  #   lidR::LAS(segmentation_res$segmented_point_cloud,
  #     header = test_point_cloud@header
  #   ),
  #   color = "crown_id",
  #   colorPalette = crown_colors,
  #   size = 5
  # )
  #
  # # Plot the segmented modes
  # lidR::plot(
  #   lidR::LAS(segmentation_res$modes[, .(X = x, Y = y, Z = z, crown_id)]),
  #   color = "crown_id",
  #   colorPalette = crown_colors,
  #   size = 5
  # )
  #
  # # Plot the segmented centroids
  # lidR::plot(
  #   lidR::LAS(segmentation_res$centroids[, .(X = x, Y = y, Z = z, crown_id)]),
  #   color = "crown_id",
  #   colorPalette = crown_colors,
  #   size = 1
  # )
})


# LAS method --------------------------------

test_that("the LAS method works", {
  # Load real point cloud data
  test_point_cloud_file_path <- system.file(
    "extdata", "MixedConifer.laz",
    package = "lidR"
  )
  test_point_cloud_header <- lidR::readLASheader(test_point_cloud_file_path)
  test_point_cloud <- lidR::clip_rectangle(lidR::readLAS(test_point_cloud_file_path),
    xleft = test_point_cloud_header@PHB$`Min X`,
    ybottom = test_point_cloud_header@PHB$`Min Y`,
    xright = test_point_cloud_header@PHB$`Min X` + 50,
    ytop = test_point_cloud_header@PHB$`Min Y` + 50
  )
  # Sample a few points for a very small "pseudo" point cloud
  test_points <- lidR::filter_poi(test_point_cloud, gpstime < 150747)

  # test the most simple form
  segmented_points <- segment_tree_crowns(
    point_cloud = test_points,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5
  )
  expect_s4_class(segmented_points, class = "LAS")
  expect_named(segmented_points@data,
    expected = c(names(test_points@data), "crown_id")
  )
  # Expect that the LAS header only knows about the already existing treeID
  # column
  expect_named(
    segmented_points@header@VLR$Extra_Bytes$`Extra Bytes Description`,
    expected = "treeID"
  )

  # test with verbose = FALSE, custom column name, and making the IDs
  # "file-writable"
  segmented_points <- segment_tree_crowns(
    point_cloud = test_points,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    verbose = FALSE,
    crown_id_column_name = "test id col name",
    write_crown_id_also_to_file = TRUE
  )
  expect_s4_class(segmented_points, class = "LAS")
  expect_named(segmented_points@data,
    expected = c(names(test_points@data), "test id col name")
  )
  # Expect that the LAS header knows about the new ID column
  expect_named(
    segmented_points@header@VLR$Extra_Bytes$`Extra Bytes Description`,
    expected = c("treeID", "test id col name")
  )

  # TODO maybe make this a test for the validate_crown_id_column_name function
  expect_error(
    segment_tree_crowns(
      point_cloud = test_points,
      crown_diameter_to_tree_height = 0.25,
      crown_length_to_tree_height = 0.5,
      crown_id_column_name = "treeID"
    ),
    regexp = paste0(
      "^The point cloud data already has a column/attribute with the name ",
      "\"treeID\"\\. Please either choose a different argument for the ",
      "crown_id_column_name parameter or modify the point cloud data\\.$"
    )
  )

  # Test varying different arguments
  segmented_points <- segment_tree_crowns(
    point_cloud = test_point_cloud,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    verbose = FALSE,
    centroid_convergence_distance = 0.1,
    max_iterations_per_point = 50,
    dbscan_neighborhood_radius = 1,
    min_num_points_per_crown = 10
  )
  expect_s4_class(segmented_points, class = "LAS")

  # Also return all centroids and vary some arguments
  segmentation_res <- segment_tree_crowns(
    point_cloud = test_point_cloud,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    max_iterations_per_point = 1000,
    min_num_points_per_crown = 10,
    also_return_terminal_centroids = TRUE,
    also_return_all_centroids = TRUE
  )
  expect_s4_class(segmentation_res$segmented_point_cloud, class = "LAS")
  expect_named(segmentation_res$segmented_point_cloud@data,
    expected = c(names(test_point_cloud@data), "crown_id")
  )
  expect_s4_class(segmentation_res$terminal_centroids, class = "LAS")
  expect_named(segmentation_res$terminal_centroids@data,
    expected = c("X", "Y", "Z", "crown_id", "point_index")
  )
  expect_s4_class(segmentation_res$centroids, class = "LAS")
  expect_named(segmentation_res$centroids@data,
    expected = c("X", "Y", "Z", "crown_id", "point_index")
  )

  # # Plot segmentation results (for manual testing only)
  # crown_colors <- c(
  #   "#DDDDDD",
  #   lidR::random.colors(
  #     n = data.table::uniqueN(segmentation_res$segmented_point_cloud@data$crown_id)
  #     - 1
  #   )
  # )
  #
  # # Plot the segmented point cloud
  # lidR::plot(
  #   segmentation_res$segmented_point_cloud,
  #   color = "crown_id",
  #   colorPalette = crown_colors,
  #   size = 5
  # )
  #
  # # Plot the segmented modes
  # lidR::plot(
  #   segmentation_res$modes,
  #   color = "crown_id",
  #   colorPalette = crown_colors,
  #   size = 5
  # )
  #
  # # Plot the segmented centroids
  # lidR::plot(
  #   segmentation_res$centroids,
  #   color = "crown_id",
  #   colorPalette = crown_colors,
  #   size = 1
  # )
})

# LAScatalog method -------------------------------------------

test_that("the LAScatalog method works", {
  skip_on_cran() # because it just takes quite long (ca. 20 s)

  # Parallelization to maximum 2 cores
  future::plan(strategy = future::multisession(workers = min(2, future::availableCores())))

  # Load real point cloud data
  test_catalog <- lidR::readLAScatalog(
    system.file("extdata", "MixedConifer.laz", package = "lidR"),
    chunk_size = 57,
    chunk_buffer = 20
  )

  segmented_points <- segment_tree_crowns(
    test_catalog,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    crown_id_column_name = "tree_id"
  )
  expect_s4_class(segmented_points, class = "LAS")
  expect_true("tree_id" %in% names(segmented_points@data))
  # Expect that the LAS header knows about the new ID column
  expect_named(
    segmented_points@header@VLR$Extra_Bytes$`Extra Bytes Description`,
    expected = c("treeID", "tree_id")
  )

  # # Plot segmentation results (for manual testing only)
  # crown_colors <- c(
  #   "#DDDDDD",
  #   lidR::random.colors(
  #     n = data.table::uniqueN(segmented_points@data$tree_id)
  #     - 1
  #   )
  # )
  #
  # # Plot the segmented point cloud
  # lidR::plot(
  #   segmented_points,
  #   color = "tree_id",
  #   colorPalette = crown_colors,
  #   size = 5
  # )
})

# Uniqueness of LAScatalog crown IDs ------------------------------

test_that("the calculation of unique crown IDs works for LAScatalog", {
  skip_on_cran() # because it takes long (ca. 14 s)

  # TODO Stuff seems to work but I haven't found a way to systematically test it
  # yet.

  las_object <- lidR::readLAS(
    system.file("extdata", "MixedConifer.laz", package = "lidR"),
    filter = "-drop_z_below 1"
  )
  segmented_las <- segment_tree_crowns(las_object,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5
  )

  las_catalog <- lidR::readLAScatalog(
    system.file("extdata", "MixedConifer.laz", package = "lidR"),
    chunk_size = 80,
    filter = "-drop_z_below 1"
  )

  segmented_las_catalog <- segment_tree_crowns(las_catalog,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5
  )

  expect_equal(
    data.table::uniqueN(segmented_las@data$crown_id),
    data.table::uniqueN(segmented_las_catalog@data$crown_id)
  )

  # Plot crowns which intersect chunk borders
  # chunks <- lidR::catalog_makechunks(las_catalog)
  #
  # chunk_id_sets <- lapply(chunks, function(chunk) {
  #   chunk_extent <- lidR::extent(chunk)
  #   chunk_points <- lidR::filter_poi(segmented_las,
  #     chunk_extent@xmin <= X & X < chunk_extent@xmax &
  #       chunk_extent@ymin <= Y & Y < chunk_extent@ymax
  #   )
  #   unique(chunk_points@data$crown_id)
  # })
  #
  # lidR::plot(
  #   lidR::filter_poi(segmented_las,
  #     crown_id %in% intersect(chunk_id_sets[[2]], chunk_id_sets[[4]])
  #   ),
  #   axis = TRUE,
  #   legend = TRUE,
  #   color = "crown_id"
  # )
})

# Different C++ back-ends -------------------------------------------------

test_that("The terraneous and flexible C++ back-ends work", {
  skip_on_cran() # because it takes long (ca. 10 s)

  test_points <- lidR::readLAS(
    system.file("extdata/Topography.laz", package = "lidR")
  )

  # The terraneous back-end
  ground_height_grid <- lidR::rasterize_terrain(
    test_points,
    algorithm = lidR::tin()
  )

  segmented_points_grid <- crownsegmentr::segment_tree_crowns(
    point_cloud = test_points,
    crown_diameter_to_tree_height = 0.5,
    crown_length_to_tree_height = 1,
    segment_crowns_only_above = 2,
    ground_height = ground_height_grid
  )

  segmented_points_alg <- crownsegmentr::segment_tree_crowns(
    point_cloud = test_points,
    crown_diameter_to_tree_height = 0.5,
    crown_length_to_tree_height = 1,
    segment_crowns_only_above = 2,
    ground_height = list(algorithm = lidR::tin())
  )

  testthat::expect_equal(segmented_points_alg, segmented_points_grid)

  # The flexible back-end
  crown_length_to_tree_height_grid <- terra::rast(
    matrix(c(0.5, 1), ncol = 2),
    crs = lidR::st_crs(test_points)$wkt,
    extent = terra::ext(
      lidR::st_bbox(test_points)$xmin,
      lidR::st_bbox(test_points)$xmax,
      lidR::st_bbox(test_points)$ymin,
      lidR::st_bbox(test_points)$ymax
    )
  )

  segmented_points <- crownsegmentr::segment_tree_crowns(
    point_cloud = test_points,
    crown_diameter_to_tree_height = 0.5,
    crown_length_to_tree_height = crown_length_to_tree_height_grid,
    ground_height = list(algorithm = lidR::tin())
  )

  # The flexible back-end with a normalized point cloud
  test_points <- lidR::readLAS(
    system.file("extdata/MixedConifer.laz", package = "lidR")
  )

  crown_diameter_to_tree_height_grid <- terra::rast(
    matrix(c(0.3, 0.8), ncol = 2),
    crs = lidR::st_crs(test_points)$wkt,
    extent = terra::ext(
      lidR::st_bbox(test_points)$xmin,
      lidR::st_bbox(test_points)$xmax,
      lidR::st_bbox(test_points)$ymin,
      lidR::st_bbox(test_points)$ymax
    )
  )

  segmented_points <- crownsegmentr::segment_tree_crowns(
    point_cloud = test_points,
    crown_diameter_to_tree_height = crown_diameter_to_tree_height_grid,
    crown_length_to_tree_height = 0.8
  )


  # Plot segmentation results (for manual testing only)

  # # Generate Crown Colors
  # crown_colors <- lidR::random.colors(
  #   n = data.table::uniqueN(segmented_points@data$crown_id) - 1
  # )
  #
  # # Plot the segmented point cloud
  # lidR::plot(
  #   lidR::filter_poi(segmented_points, !(Classification %in% c(2, 9))),
  #   color = "crown_id",
  #   pal = crown_colors,
  #   size = 5
  # )
})
