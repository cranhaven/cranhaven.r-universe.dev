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


#' Assert that the extent of a raster covers that of a data.frame point cloud
#'
#' @param raster A [SpatRaster][terra::SpatRaster].
#' @param data_frame_point_cloud Point cloud data in [data.frame()] format.
#' @param message Length-one character vector. Message to be used on assertion
#'  failure.
assert_that_raster_covers_data_frame_point_cloud <- function(
    raster,
    data_frame_point_cloud,
    message) {
  point_cloud_coordinates <- extract_coordinate_values(data_frame_point_cloud)

  point_cloud_xmin <- min(point_cloud_coordinates[[1]], na.rm = FALSE)
  point_cloud_ymin <- min(point_cloud_coordinates[[2]], na.rm = FALSE)
  point_cloud_xmax <- max(point_cloud_coordinates[[1]], na.rm = FALSE)
  point_cloud_ymax <- max(point_cloud_coordinates[[2]], na.rm = FALSE)

  assert_that(
    terra::xmin(raster) <= point_cloud_xmin,
    terra::ymin(raster) <= point_cloud_ymin,
    terra::xmax(raster) >= point_cloud_xmax,
    terra::ymax(raster) >= point_cloud_ymax,
    msg = message
  )
}

#' Assert that the extent of a raster covers that of a LAS point cloud
#'
#' @param raster A [SpatRaster][terra::SpatRaster].
#' @param las_point_cloud Point cloud data in [lidR::LAS] format.
#' @param message Length-one character vector. Message to be used on assertion
#'  failure.
assert_that_raster_covers_las_point_cloud <- function(
    raster,
    las_point_cloud,
    message) {
  pc_extent <- lidR::st_bbox(las_point_cloud)

  assert_that(
    terra::xmin(raster) <= pc_extent$xmin,
    terra::ymin(raster) <= pc_extent$ymin,
    terra::xmax(raster) >= pc_extent$xmax,
    terra::ymax(raster) >= pc_extent$ymax,
    msg = paste(
      "The crown diameter to tree height raster does not cover the",
      "extent of the point cloud."
    )
  )
}


assert_that_raster_fits_point_cloud <- function(raster, point_cloud, raster_name) {
  if (inherits(point_cloud, "data.frame")) {
    # if point_cloud is a data.frame(-like) object
    assert_that_raster_covers_data_frame_point_cloud(
      raster = raster,
      data_frame_point_cloud = point_cloud,
      message = paste(
        "The", raster_name, "raster does not cover the extent of the point cloud."
      )
    )
  } else if (methods::is(point_cloud, "LAS") ||
    methods::is(point_cloud, "LAScatalog")) {
    # if point_cloud is a LAS or LAScatalog object

    assert_that_raster_covers_las_point_cloud(
      raster = raster,
      las_point_cloud = point_cloud,
      message = paste(
        "The", raster_name, "raster does not cover the extent of the point cloud."
      )
    )
  } else {
    # if point_cloud is none of the above
    stop(paste(
      "This shouldn't happen. Your point cloud does neither inherit from",
      "data.frame nor is it a LAS or LAScatalog object."
    ))
  }
}

assert_that_raster_has_numeric_values <- function(raster, raster_name) {
  if (terra::inMemory(raster)) {
    assert_that(
      terra::hasValues(raster),
      msg = paste(raster_name, "contains no values.")
    )

    raster_values <- terra::values(raster)
    assert_that(
      is.numeric(raster_values),
      msg = paste(raster_name, "does not contain numeric values.")
    )
  } else {
    assert_that(
      terra::datatype(raster)[1] %in% c("FLT4S", "FLT8S"),
      msg = paste(
        raster_name, "does not contain numeric values."
      )
    )
  }
}

# Note: The definition of a valid coordinate table used by this function is
# relied on by the segment_tree_crowns_core function. Be aware of that when
# changing this function.
validate_coordinate_table <- function(coordinate_table) {
  assert_that(is.data.frame(coordinate_table), msg = paste(
    "The coordinate data needs to be stored in a data.frame or another data",
    "type which can be treated as one."
  ))

  # Get the number of numeric columns
  num_numeric_cols <- length(which(sapply(coordinate_table, is.numeric)))

  assert_that(num_numeric_cols >= 3, msg = paste(
    "The coordinate table needs to have at least three numeric columns for",
    "x-, y-, and z-coordinates but there are only", num_numeric_cols,
    "numeric columns."
  ))
}

validate_kernel_params <- function(
    kernel_to_tree_height,
    kernel_constant,
    point_cloud,
    which = "diameter") {
  # intercept is numeric and not NA
  assert_that(
    assertthat::is.number(kernel_constant),
    assertthat::noNA(kernel_constant)
  )

  assert_that(
    kernel_constant >= 0,
    msg = paste(
      "Used a crown",
      which,
      "constant below 0 which doesn't work."
    )
  )


  # if kernel_to_tree_height is a raster
  if (methods::is(kernel_to_tree_height, "SpatRaster")) {
    if (terra::nlyr(kernel_to_tree_height) > 1) {
      warning(
        paste0(
          "crown_",
          which,
          "_to_tree_height has more than one raster layer. Only the first",
          "layer is considered."
        )
      )
    }

    assert_that_raster_has_numeric_values(
      raster = kernel_to_tree_height,
      raster_name = paste0("kernel_", which, "_slope")
    )

    raster_minmax <- terra::minmax(kernel_to_tree_height, compute = TRUE)[, 1]

    if (kernel_constant == 0) {
      assert_that(
        raster_minmax["min"] > 0,
        msg = paste(
          "Used a crown", which, " to tree height value equal to or less than",
          "zero. This does not work when the constant is zero."
        )
      )
    } else { # if kernel intercept > 0
      assert_that(
        raster_minmax["min"] >= 0,
        msg = paste(
          "The crown", which, "to tree height raster contains values below",
          "zero."
        )
      )
    }


    # warning for high values
    if (raster_minmax["max"] > 2) {
      warning(paste0(
        "A crown ", which, " to tree height value greater than 2 is likely",
        " too high (the largest of the ratios that you provide is ",
        raster_minmax["max"], ")."
      ))
    }

    assert_that_raster_fits_point_cloud(
      raster = kernel_to_tree_height,
      point_cloud = point_cloud,
      raster_name = paste0("crown_", which, "_to_tree_height")
    )
  } else if (methods::is(kernel_to_tree_height, "character")) {
    assert_that(!inherits(point_cloud, "data.frame"), msg = paste(
      "Passing an algorithm name for crown_diameter_to_tree_height is not",
      "supported with data.frame(-like) point clouds."
    ))
    assert_that(!inherits(point_cloud, "LAScatalog"), msg = paste(
      "Passing an algorithm name for crown_diameter_to_tree_height is not",
      "supported with point clouds of type LAS Catalog."
    ))

    assert_that(
      which == "diameter",
      msg = "crown_length_to_tree_height must be numeric or SpatRaster."
    )

    legitimate_arguments <- c("li", "li2012", "ws", "watershed")
    assert_that(
      (kernel_to_tree_height %in% legitimate_arguments),
      msg = paste(
        "crown_diameter_to_tree_height must be numeric, SpatRaster,",
        "or one of the following strings: 'li2012' or 'watershed'"
      )
    )
  } else { # if kernel_to_tree_height is neither raster nor character
    assert_that(
      assertthat::is.number(kernel_to_tree_height),
      assertthat::noNA(kernel_to_tree_height)
    )

    if (kernel_constant == 0) {
      assert_that(
        kernel_to_tree_height > 0,
        msg = paste(
          "Used a crown", which, "to tree height value equal to or less than",
          "zero. This does not work when the constant is zero."
        )
      )
    } else { # if kernel intercept > 0
      assert_that(
        kernel_to_tree_height >= 0,
        msg = paste("Used a crown", which, "to tree height value below zero.")
      )
    }
    if (kernel_to_tree_height > 2) {
      warning(paste(
        "A crown", which,
        "to tree height greater than 2 is likely too high."
      ))
    }
  }
}


validate_segment_crowns_only_above <- function(segment_crowns_only_above) {
  assert_that(
    assertthat::is.number(segment_crowns_only_above),
    assertthat::noNA(segment_crowns_only_above),
    segment_crowns_only_above >= 0
  )
}

validate_ground_height <- function(ground_height, point_cloud) {
  if (is.null(ground_height)) {
    # if ground_height is NULL do nothing
  } else if (methods::is(ground_height, "SpatRaster")) {
    # if ground_height is a raster
    if (terra::nlyr(ground_height) > 1) {
      warning(
        "ground_height has more than one raster layer. Only the first layer is",
        " considered."
      )
    }

    assert_that_raster_has_numeric_values(
      raster = ground_height,
      raster_name = "ground_height"
    )

    assert_that_raster_fits_point_cloud(
      raster = ground_height,
      point_cloud = point_cloud,
      raster_name = "ground height"
    )
  } else if (is.list(ground_height)) {
    # if ground_height is a list
    assert_that(!inherits(point_cloud, "data.frame"), msg = paste(
      "Passing a list for parameter ground_height is not supported with",
      "data.frame(-like) point clouds."
    ))
    assert_that(assertthat::not_empty(ground_height))
    assert_that(!(assertthat::has_name(ground_height, "las")), msg = paste(
      "Parameter ground_height must not contain an element called \"las\" when",
      "it is a list (see documentation)."
    ))
  } else {
    # if ground_height is none of the above
    stop(paste0(
      "ground_height is neither NULL, nor a raster object, nor a list."
    ))
  }
}

validate_crown_id_column_name <- function(crown_id_column_name,
                                          coordinate_table) {
  # Check the data type and validity of the crown ID column name
  assert_that(
    assertthat::is.string(crown_id_column_name),
    assertthat::noNA(crown_id_column_name),
    crown_id_column_name != ""
  )

  # Assert that crown_id_column_name is not already a column name of the
  # coordinate_table
  assert_that(
    !(make.names(crown_id_column_name) %in% colnames(coordinate_table)),
    msg = paste0(
      "The point cloud data already has a column/attribute with the name \"",
      crown_id_column_name, "\". Please either choose a different argument for",
      " the crown_id_column_name parameter or modify the point cloud data."
    )
  )
}

validate_verbose <- function(verbose) {
  assert_that(
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
}

validate_also_return_terminal_centroids <- function(also_return_terminal_centroids) {
  assert_that(
    assertthat::is.flag(also_return_terminal_centroids),
    assertthat::noNA(also_return_terminal_centroids)
  )
}

validate_also_return_all_centroids <- function(also_return_all_centroids) {
  assert_that(
    assertthat::is.flag(also_return_all_centroids),
    assertthat::noNA(also_return_all_centroids)
  )
}

validate_centroid_convergence_distance <-
  function(centroid_convergence_distance) {
    assert_that(
      assertthat::is.number(centroid_convergence_distance),
      assertthat::noNA(centroid_convergence_distance),
      centroid_convergence_distance > 0
    )
  }

validate_max_iterations_per_point <- function(max_iterations_per_point) {
  assert_that(
    assertthat::is.count(max_iterations_per_point),
    max_iterations_per_point >= 1
  )
}

validate_dbscan_neighborhood_radius <- function(dbscan_neighborhood_radius) {
  assert_that(
    assertthat::is.number(dbscan_neighborhood_radius),
    assertthat::noNA(dbscan_neighborhood_radius),
    dbscan_neighborhood_radius > 0
  )
}

validate_min_num_points_per_crown <-
  function(min_num_points_per_crown) {
    assert_that(
      assertthat::is.count(min_num_points_per_crown),
      min_num_points_per_crown >= 1
    )
  }

validate_write_crown_id_also_to_file <- function(write_crown_id_also_to_file) {
  assert_that(
    assertthat::is.flag(write_crown_id_also_to_file),
    assertthat::noNA(write_crown_id_also_to_file)
  )
}

#' Ensures that crown IDs are written to output files of the LAScatalog
#'
#' Issues a warning if the user wanted to write the output to files but not
#' store IDs of segmented bodies.
#'
#' @param write_crown_id_also_to_file The to-be-validated parameter.
#' @param LAScatalog The [LAScatalog][lidR::LAScatalog-class] whose
#'   settings are compared to the value of `write_crown_id_also_to_file`.
#'
#' @return A possibly corrected value for `write_crown_id_also_to_file`.
validate_write_crown_id_also_to_file_for_LAScatalogs <-
  function(write_crown_id_also_to_file, LAScatalog) {
    validate_write_crown_id_also_to_file(write_crown_id_also_to_file)

    if (!write_crown_id_also_to_file &&
      lidR::opt_output_files(LAScatalog) != "") {
      warning(
        "IDs of segmented bodies will be written to the output files. ",
        "(This warning was generated because you set the ",
        "write_crown_id_also_to_file parameter to FALSE but requested ",
        "output files instead of an in-memory object via the LAScatalog ",
        "options.)"
      )
      return(TRUE)
    } else {
      return(write_crown_id_also_to_file)
    }
  }

validate_crown_id_file_description <- function(crown_id_file_description) {
  assert_that(
    assertthat::is.string(crown_id_file_description),
    assertthat::noNA(crown_id_file_description),
    crown_id_file_description != ""
  )
}

#' Asserts that all files referenced by a LAScatalog have the same scale and
#' offset values.
#'
#' @param LAScatalog The [LAScatalog][lidR::LAScatalog-class] to be tested.
validate_scale_n_offset_are_consistent <- function(LAScatalog) {
  scales_n_offsets <- collect_scale_n_offset_of_LAScatalog_files(
    LAScatalog
  )[, 1:6] # select only the values and not the last column with the file paths

  # Iterate over the scale and offset values for the x, y, and z coordinates one
  # after the other.
  for (column_name in colnames(scales_n_offsets)) {
    # Get the column
    values <- scales_n_offsets[[column_name]]

    # the following logic was inspired by this discussion:
    # https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-numeric-vector#

    # Sanity check for NA values
    assert_that(!anyNA(values),
      msg = paste0(
        "This shouldn't happen? Your LAScatalog appears to be referencing at ",
        "least one LAS file with their ", column_name, " set to NA."
      )
    )

    # Only do the following if there is more than one value
    if (length(values) > 1) {
      # Check that all the values are near-equal.
      assert_that(abs(max(values) - min(values)) < .Machine$double.eps^0.5,
        msg = paste0(
          "The scale and offset values of all used LAS files have to be equal.",
          " Your LAScatalog appears to be referencing LAS files with differing",
          " ", column_name, "s. Call ",
          "crownsegmentr::collect_scale_n_offset_of_LAScatalog_files(<your LAScatalog object>)",
          " to get an overview of these values for all files referenced by ",
          "your LAScatalog."
        )
      )
    }
  }
}

# Validation functions for diameter_raster

validate_crown_diameter_constant <- function(intercept) {
  assert_that(
    assertthat::is.number(intercept),
    assertthat::noNA(intercept),
    intercept >= 0
  )
}

validate_diameter_limits <- function(limits) {
  assert_that(
    is.vector(limits),
    is.numeric(limits),
    length(limits) > 1,
    assertthat::noNA(limits)
  )
}

validate_smoothing_radius <- function(smoothing_radius) {
  assert_that(
    assertthat::is.number(smoothing_radius),
    assertthat::noNA(smoothing_radius),
    smoothing_radius >= 0
  )
}
