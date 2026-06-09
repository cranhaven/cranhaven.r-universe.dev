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


#' Calls the C++ back-end and the DBSCAN algorithm to perform the segmentation
#'
#' This functions is meant to be used internally by methods of the
#' `segment_tree_crowns` generic.
#'
#' @param coordinate_table A [data.frame][base::data.frame()] or
#'   [data.table][data.table::data.table()]
#'   which is a valid coordinate table according to `validate_coordinate_table`.
#' @param ground_height One of
#'   \itemize{
#'     \item `NULL`, indicating that the point cloud stored in
#'     `coordinate_table` is normalized with ground height at zero.
#'     \item A [SpatRaster][terra::SpatRaster] providing ground heights for
#'     the area of the (not normalized) point cloud stored in
#'     `coordinate_table`.
#'   }
#' @inheritParams segment_tree_crowns
#'
#' @inheritSection segment_tree_crowns How the algorithm works
#'
#' @returns A list with at most three elements:
#'   \describe{
#'     \item{crown_ids}{A vector of IDs of segmented bodies.}
#'     \item{terminal_coordinates}{
#'       If `also_return_terminal_centroids` was set to `TRUE`, a
#'       [data.table][data.table::data.table()] with mode coordinates as the
#'       second list element.
#'       The table has two additional columns: \describe{
#'         \item{crown_id}{
#'           Holds the IDs also returned with the first list element.
#'         }
#'         \item{point_index}{
#'           Holds row indices of the original points in the input
#'           `coordinate_table`.
#'         }
#'       }
#'     }
#'     \item{centroid_coordinates}{
#'       If `also_return_all_centroids` was set to `TRUE`, a
#'       [data.table][data.table::data.table()] with centroid coordinates as the
#'       last list element.
#'       The table has two additional columns:
#'       \describe{
#'         \item{crown_id}{
#'           Holds the IDs also returned with the first list element.
#'         }
#'         \item{point_index}{
#'           Holds row indices of the original points in the input
#'           `coordinate_table`.
#'         }
#'       }
#'     }
#'   }
segment_tree_crowns_core <- function(
    coordinate_table,
    segment_crowns_only_above,
    ground_height,
    crown_diameter_to_tree_height,
    crown_length_to_tree_height,
    crown_diameter_constant,
    crown_length_constant,
    verbose,
    centroid_convergence_distance,
    max_iterations_per_point,
    dbscan_neighborhood_radius,
    min_num_points_per_crown,
    also_return_terminal_centroids,
    also_return_all_centroids) {
  coordinate_values <- extract_coordinate_values(coordinate_table)

  # Warn about likely not normalized point clouds
  if (is.null(ground_height)) {
    warn_about_normalization(coordinate_table[[3]])
  } else {
    # Get the raster values only in the bounding box of the point cloud
    ground_height <- crop_raster_with_coordinates_extent(
      ground_height, coordinate_values
    )

    # Create a list with raster data that can be used by the C++ back-end
    ground_height <- create_cpp_list_from_raster(ground_height)
  }

  # If either of the crown diameter or crown length to tree height ratios is a
  # raster, convert both of them and the ground height to lists for the C++
  # back-end.
  if (methods::is(crown_diameter_to_tree_height, "SpatRaster") ||
    methods::is(crown_length_to_tree_height, "SpatRaster")) {
    if (methods::is(crown_diameter_to_tree_height, "SpatRaster")) {
      crown_diameter_to_tree_height <- crop_raster_with_coordinates_extent(
        crown_diameter_to_tree_height, coordinate_values
      )

      crown_diameter_to_tree_height <- create_cpp_list_from_raster(
        crown_diameter_to_tree_height
      )
    } else {
      crown_diameter_to_tree_height <- list(
        value = crown_diameter_to_tree_height
      )
    }

    if (methods::is(crown_length_to_tree_height, "SpatRaster")) {
      crown_length_to_tree_height <- crop_raster_with_coordinates_extent(
        crown_length_to_tree_height, coordinate_values
      )

      crown_length_to_tree_height <- create_cpp_list_from_raster(
        crown_length_to_tree_height
      )
    } else {
      crown_length_to_tree_height <- list(
        value = crown_length_to_tree_height
      )
    }

    if (is.null(ground_height)) {
      ground_height <- list(value = 0)
    }
  }


  # Call the C++ back-end

  # If crown_diameter_to_tree_height is a list, call the "flexible" C++ back-end
  if (is.list(crown_diameter_to_tree_height)) {
    all_centroids <- calculate_centroids_flexible(
      coordinate_values,
      min_point_height_above_ground = segment_crowns_only_above,
      ground_height,
      crown_diameter_to_tree_height,
      crown_length_to_tree_height,
      crown_diameter_constant,
      crown_length_constant,
      centroid_convergence_distance,
      max_iterations_per_point,
      also_return_all_centroids,
      show_progress_bar = verbose
    )

    # Call the C++ back-end for normalized point clouds
  } else if (is.null(ground_height)) {
    all_centroids <- calculate_centroids_normalized(
      coordinate_values,
      min_point_height_above_ground = segment_crowns_only_above,
      crown_diameter_to_tree_height,
      crown_length_to_tree_height,
      crown_diameter_constant,
      crown_length_constant,
      centroid_convergence_distance,
      max_iterations_per_point,
      also_return_all_centroids,
      show_progress_bar = verbose
    )

    # Call the C++ back-end for not normalized point clouds
  } else {
    all_centroids <- calculate_centroids_terraneous(
      coordinate_values,
      min_point_height_above_ground = segment_crowns_only_above,
      ground_height,
      crown_diameter_to_tree_height,
      crown_length_to_tree_height,
      crown_diameter_constant,
      crown_length_constant,
      centroid_convergence_distance,
      max_iterations_per_point,
      also_return_all_centroids,
      show_progress_bar = verbose
    )
  }

  terminal_centroids <- all_centroids$terminal_coordinates

  # Find terminal_centroids with NA coordinate values to exclude them from the DBSCAN
  # clustering and directly set their IDs to NA
  is_na_mode_row <- is.na(terminal_centroids$x) |
    is.na(terminal_centroids$y) |
    is.na(terminal_centroids$z)

  if (verbose) message("  Finding mode clusters...", appendLF = FALSE)

  # Set up a vector for the crown IDs
  crown_ids <- vector(mode = "integer", length = nrow(terminal_centroids))
  # Set the crown IDs to NA wherever a mode coordinate value is NA
  crown_ids[is_na_mode_row] <- NA_integer_

  # Assign the cluster IDs found with DBSCAN as crown IDs to those indices
  # where there are no NA mode coordinate values
  crown_ids[!is_na_mode_row] <- dbscan::dbscan(
    terminal_centroids[!is_na_mode_row, ],
    eps = dbscan_neighborhood_radius,
    minPts = min_num_points_per_crown
  )$cluster

  # Set all IDs == 0 to NA to indicate unsegmented points
  crown_ids[crown_ids == 0] <- NA_integer_

  # I have observed that the DBSCAN algorithm sometimes identifies clusters with
  # less than minPts points. My guess is that clusters loose points to
  # neighboring clusters after having been initially identified with minPts
  # points.
  # In any case, treat all segmented bodies with less than
  # min_num_points_per_crown points as noise by setting their points' IDs
  # to NA.

  # count the number of points per ID
  num_points_per_id <- table(crown_ids)
  # select IDs with an insufficient number of points
  ids_with_too_few_points <- as.integer(names(
    num_points_per_id[which(num_points_per_id < min_num_points_per_crown)]
  ))
  # set these IDs to NA
  crown_ids[which(crown_ids %in% ids_with_too_few_points)] <- NA_integer_

  if (verbose) message("done.") # ...with the mode clustering


  # Create the to-be-returned list
  res <- list("crown_ids" = crown_ids)

  if (also_return_terminal_centroids) {
    # set mode coordinates with crown IDs and point indices as the second list
    # element
    res[["terminal_coordinates"]] <- data.table::data.table(
      terminal_centroids,
      crown_id = crown_ids,
      point_index = seq_len(nrow(coordinate_table))
    )
    # remove rows with NA mode coordinates
    res$terminal_coordinates <- res$terminal_coordinates[!is_na_mode_row]
  }

  if (also_return_all_centroids) {
    crown_ids_w_point_indices <- data.table::data.table(
      crown_id = crown_ids,
      point_index = seq_len(nrow(coordinate_table))
    )

    # For R CMD check which sees variables in data.table syntax as global
    . <- x <- y <- z <- crown_id <- point_index <- NULL

    # Join the crown IDs to the centroid coordinates via the point index using
    # some data.table syntax.
    res[["centroid_coordinates"]] <- data.table::as.data.table(
      all_centroids$centroid_coordinates
    )[
      crown_ids_w_point_indices,
      on = "point_index"
    ][ # join syntax
      # make point_index the last column for consistency with the terminal
      # centroids data
      , .(x, y, z, crown_id, point_index)
    ]

    # Find centroids with NA coordinates
    na_centroid_coordinates <-
      is.na(res$centroid_coordinates$x) |
        is.na(res$centroid_coordinates$y) |
        is.na(res$centroid_coordinates$z)

    # Exclude centroids with NA coordinates
    res$centroid_coordinates <- res$centroid_coordinates[
      !na_centroid_coordinates,
    ]
  }

  return(res)
}


warn_about_normalization <- function(Z_coordinate_values) {
  # Get min, max, and 1% Z values
  z_percentiles <- stats::quantile(Z_coordinate_values, probs = c(0, 0.01, 1))

  lowest_point_too_high <- z_percentiles[[1]] > 1
  highest_point_too_high <- z_percentiles[[3]] > 100
  lowest_points_too_far_apart <- z_percentiles[[2]] - z_percentiles[[1]] > 3

  if (any(
    lowest_point_too_high,
    highest_point_too_high,
    lowest_points_too_far_apart
  )) {
    warning_start <- paste0(
      "Your point cloud might not be normalized! If you want to segment trees ",
      "in not normalized point clouds, you have to provide ground heights via ",
      "the `ground_height` parameter.\n",
      "(Your point cloud looks like it's not normalized because "
    )

    if (lowest_point_too_high) {
      reason <- "the lowest point lies above 1 m"
    } else if (highest_point_too_high) {
      reason <- "the highest point lies above 100 m"
    } else if (lowest_points_too_far_apart) {
      reason <-
        "the lowest 1% of points covers a vertical distance of more than 3 m"
    }

    warning(warning_start, reason, " (units are assumed).)",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}

crop_raster_with_coordinates_extent <- function(raster, coordinates) {
  return(terra::crop(raster, terra::ext(
    min(coordinates[[1]], na.rm = TRUE),
    max(coordinates[[1]], na.rm = TRUE),
    min(coordinates[[2]], na.rm = TRUE),
    max(coordinates[[2]], na.rm = TRUE)
  )))
}

create_cpp_list_from_raster <- function(raster) {
  return(list(
    values = terra::values(raster[[1]])[, 1],
    num_rows = as.integer(terra::nrow(raster)),
    num_cols = as.integer(terra::ncol(raster)),
    x_min = terra::xmin(raster),
    x_max = terra::xmax(raster),
    y_min = terra::ymin(raster),
    y_max = terra::ymax(raster)
  ))
}
