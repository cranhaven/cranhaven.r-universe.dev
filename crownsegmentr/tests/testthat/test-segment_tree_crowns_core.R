# This file is part of crownsegmentr, an R package for identifying tree crowns
# within 3D point clouds.
#
# Copyright (C) 2025 Leon Steinmeier, Timon Miesner, Nikolai Knapp
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


# Core function -----------------------------------------------

test_that("the core function works with 'normal' arguments", {
  # Load point cloud data
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

  # Request the most simple output (i.e. just crown IDs)
  test_output <- segment_tree_crowns_core(
    test_point_cloud@data,
    ground_height = NULL,
    segment_crowns_only_above = 0,
    crown_diameter_to_tree_height = 0.3,
    crown_length_to_tree_height = 0.5,
    crown_diameter_constant = 1,
    crown_length_constant = 1,
    verbose = TRUE,
    also_return_terminal_centroids = FALSE,
    also_return_all_centroids = FALSE,
    centroid_convergence_distance = 0.02,
    max_iterations_per_point = 200,
    dbscan_neighborhood_radius = 0.3,
    min_num_points_per_crown = 5
  )
  expect_length(test_output, 1)
  expect_named(test_output, expected = "crown_ids")
  # test crown IDs
  expect_true(is.numeric(test_output$crown_ids))
  expect_length(test_output$crown_ids, lidR::npoints(test_point_cloud))

  # Request terminal centroids
  test_output <- segment_tree_crowns_core(
    test_point_cloud@data,
    ground_height = NULL,
    segment_crowns_only_above = 0,
    crown_diameter_to_tree_height = 0.3,
    crown_length_to_tree_height = 0.5,
    crown_diameter_constant = 1,
    crown_length_constant = 1,
    verbose = TRUE,
    also_return_terminal_centroids = TRUE,
    also_return_all_centroids = FALSE,
    centroid_convergence_distance = 0.02,
    max_iterations_per_point = 200,
    dbscan_neighborhood_radius = 0.3,
    min_num_points_per_crown = 5
  )
  expect_length(test_output, 2)
  expect_named(test_output,
    expected = c("crown_ids", "terminal_coordinates")
  )
  # test crown IDs
  expect_true(is.numeric(test_output$crown_ids))
  # test terminal_centroids
  expect_s3_class(test_output$terminal_coordinates,
    class = c("data.table", "data.frame"),
    exact = TRUE
  )
  expect_length(test_output$terminal_coordinates, 5)
  expect_named(test_output$terminal_coordinates,
    expected = c("x", "y", "z", "crown_id", "point_index")
  )
  expect_setequal(test_output$terminal_coordinates$crown_id,
    expected = test_output$crown_ids
  )

  # Request centroids
  test_output <- segment_tree_crowns_core(
    test_point_cloud@data,
    ground_height = NULL,
    segment_crowns_only_above = 0,
    crown_diameter_to_tree_height = 0.3,
    crown_length_to_tree_height = 0.5,
    crown_diameter_constant = 1,
    crown_length_constant = 1,
    verbose = TRUE,
    also_return_terminal_centroids = FALSE,
    also_return_all_centroids = TRUE,
    centroid_convergence_distance = 0.02,
    max_iterations_per_point = 200,
    dbscan_neighborhood_radius = 0.3,
    min_num_points_per_crown = 5
  )
  expect_length(test_output, 2)
  expect_named(test_output,
    expected = c("crown_ids", "centroid_coordinates")
  )
  # test crown IDs
  expect_true(is.numeric(test_output$crown_ids))
  # test centroids
  expect_s3_class(test_output$centroid_coordinates,
    class = c("data.table", "data.frame"),
    exact = TRUE
  )
  expect_length(test_output$centroid_coordinates, 5)
  expect_named(test_output$centroid_coordinates,
    expected = c("x", "y", "z", "crown_id", "point_index")
  )
  expect_setequal(test_output$centroid_coordinates$crown_id,
    expected = test_output$crown_ids
  )
  # There should be at least as many centroids as points
  expect_gte(nrow(test_output$centroid_coordinates),
    expected = nrow(test_point_cloud@data)
  )

  # Request all centroids
  test_output <- segment_tree_crowns_core(
    test_point_cloud@data,
    ground_height = NULL,
    segment_crowns_only_above = 0,
    crown_diameter_to_tree_height = 0.3,
    crown_length_to_tree_height = 0.5,
    crown_diameter_constant = 1,
    crown_length_constant = 1,
    verbose = TRUE,
    also_return_terminal_centroids = TRUE,
    also_return_all_centroids = TRUE,
    centroid_convergence_distance = 0.02,
    max_iterations_per_point = 200,
    dbscan_neighborhood_radius = 0.3,
    min_num_points_per_crown = 5
  )
  expect_length(test_output, 3)
  expect_named(test_output,
    expected = c("crown_ids", "terminal_coordinates", "centroid_coordinates")
  )
  # test crown IDs
  expect_true(is.numeric(test_output$crown_ids))
  # test terminal centroids
  expect_s3_class(test_output$terminal_coordinates,
    class = c("data.table", "data.frame"),
    exact = TRUE
  )
  expect_length(test_output$terminal_coordinates, 5)
  expect_named(test_output$terminal_coordinates,
    expected = c("x", "y", "z", "crown_id", "point_index")
  )
  expect_setequal(test_output$terminal_coordinates$crown_id,
    expected = test_output$crown_ids
  )
  # test centroids
  expect_s3_class(test_output$centroid_coordinates,
    class = c("data.table", "data.frame"),
    exact = TRUE
  )
  expect_length(test_output$centroid_coordinates, 5)
  expect_named(test_output$centroid_coordinates,
    expected = c("x", "y", "z", "crown_id", "point_index")
  )
  expect_setequal(test_output$centroid_coordinates$crown_id,
    expected = test_output$crown_ids
  )


  # # Visually confirm that the segmentation worked
  # random_crown_id_colors <- c(
  #   "#DDDDDD", # use a very light gray for crown IDs == 0
  #   lidR::random.colors(data.table::uniqueN(test_output$crown_ids) - 1)
  # )
  #
  # # Plot the segmented point cloud
  # lidR::plot(
  #   lidR::add_attribute(test_point_cloud,
  #     test_output$crown_ids,
  #     name = "crown_id"
  #   ),
  #   color = "crown_id",
  #   colorPalette = random_crown_id_colors,
  #   size = 5
  # )
  # # Plot the terminal centroids
  # lidR::plot(
  #   lidR::LAS(test_output$terminal_coordinates[, .(X = x, Y = y, Z = z, crown_id)]),
  #   color = "crown_id",
  #   colorPalette = random_crown_id_colors,
  #   size = 5
  # )
  # # Plot the segmented centroid coordinates
  # lidR::plot(
  #   lidR::LAS(test_output$centroid_coordinates[
  #     , .(X = x, Y = y, Z = z, crown_id)]),
  #   color = "crown_id",
  #   colorPalette = random_crown_id_colors,
  #   size = 1
  # )
})
