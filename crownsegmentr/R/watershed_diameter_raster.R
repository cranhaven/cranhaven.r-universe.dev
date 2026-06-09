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
#
# Generic S4 Function ------------------------------------------------

#' Calculate a raster of crown diameter to tree height using watershed
#' segmentation
#'
#' The function calculates a raster with values for
#' crown_diameter_to_tree_height as input for the AMS3D algorithm. It segments
#' the tree crowns with the watershed algorithm from lidR, calculates a ratio of
#' crown diameter to tree height for each tree, and converts this into a raster.
#'
#' @param point_cloud the input point cloud, either as LAS or as data.frame.
#' @param crown_diameter_constant a fixed value for crown_diameter_constant, which
#' reduces the crown diameters by the given value before calculating the ratio
#' of crown diameter to tree height
#' @param limits a numeric vector with minimum and maximum values for
#' the ratio, at which every tree's ratio will be capped
#' @param ground_height (optional) either
#' * NULL, indicating that the point cloud is normalized, or
#' * a [SpatRaster][terra::SpatRaster] digital terrain model, or
#' * a list of arguments to the
#' [lidR rasterize_terrain()][lidR::rasterize_terrain()] function to normalize
#' the point cloud.
#' @param smoothing_radius The radius of the filter used for smoothing the
#' diameter-to-height ratio from individual trees.
#' @param ... further parameters will be passed to the function
#' [lidR::watershed()]
#' @return a terra SpatRaster
#'
#' @section Details:
#'
#' The output raster can serve as input for the parameter
#' "crown_diameter_to_tree_height" for the function
#' segment_tree_crowns.
#' It averages the ratio of crown diameter to tree height for a given radius,
#' for trees that were detected with watershed segmentation.
#' The Bioconductor package "EBImage" is required to use this function.
#'
#' @example \dontrun{R/examples/watershed_diameter_raster_examples.R}
#'
#' @export
methods::setGeneric("watershed_diameter_raster",
  function(point_cloud,
           crown_diameter_constant = 0,
           limits = c(0, 1),
           ground_height = NULL,
           smoothing_radius = 5,
           ...) {
    standardGeneric("watershed_diameter_raster")
  },
  signature = "point_cloud"
)

# Declare variables to avoid check notes for watershed_diameter_raster
utils::globalVariables(c("Z", "ID", "."))

# watershed_diameter_raster for LAS ----------------------------------
#' @describeIn watershed_diameter_raster Calculate a raster of crown diameter
#' for tree height using watershed segmentation
#'
#' @importClassesFrom lidR LAS
methods::setMethod(
  "watershed_diameter_raster",
  signature(point_cloud = "LAS"),
  function(point_cloud,
           crown_diameter_constant,
           limits,
           ground_height,
           smoothing_radius,
           ...) {
    validate_crown_diameter_constant(crown_diameter_constant)
    validate_diameter_limits(limits)
    validate_ground_height(ground_height, point_cloud)

    # If ground_height is a list of arguments, pass them to
    # lidR::rasterize_terrain
    if (is.list(ground_height)) {
      ground_height <- do.call(lidR::rasterize_terrain,
        args = c(las = point_cloud, ground_height)
      )
    }

    # if the limits vector is longer, only the min and max values will be taken
    # into account
    my_limits <- range(limits)

    # normalize point cloud if applicable
    if (!is.null(ground_height)) {
      err.msg <- "Ground height raster does not cover the area of the point cloud."
      assert_that_raster_covers_las_point_cloud(ground_height, point_cloud, err.msg)

      norm.las <- lidR::normalize_height(
        las = point_cloud,
        algorithm = lidR::kriging(),
        dtm = ground_height
      )
    } else {
      norm.las <- point_cloud
    }

    # define the resolution for the chm: if the point density is higher than 16
    # in more than half of the relevant area, use 0.25m resolution. If point
    # density is higher than 5, use 0.5 m resolution, otherwise 1 m.
    dens <- lidR::rasterize_density(norm.las, res = 1)
    if (terra::global(dens, function(x) sum(x >= 16)) >=
      0.5 * terra::global(dens, function(x) sum(x > 0))) {
      chm.res <- 0.25
    } else if (terra::global(dens, function(x) sum(x >= 5)) >=
      0.5 * terra::global(dens, function(x) sum(x > 0))) {
      chm.res <- 0.5
    } else {
      chm.res <- 1
    }

    # create canopy height model
    chm <- lidR::rasterize_canopy(norm.las,
      res = chm.res,
      algorithm = lidR::p2r(subcircle = 0.25)
    )

    # fill in NA values with 0
    chm[is.na(chm)] <- 0

    # watershed delineation
    wsh <- lidR::watershed(chm, ...)()

    # convert to polygon
    poly.wsh <- terra::as.polygons(terra::rast(wsh))

    # tree height as highest chm value
    height.points <- data.table::data.table(terra::extract(chm, poly.wsh))
    trees <- height.points[, .(height = max(Z)), keyby = ID]

    # calculate crown area
    poly.wsh$area <- terra::expanse(poly.wsh, transform = F)
    # calculate crown diameter as square root of area
    poly.wsh$diam <- sqrt(poly.wsh$area)

    # calculate diameter to height ratio
    poly.wsh$height <- trees$height
    poly.wsh$diam.height.ratio <- pmin(
      pmax(
        (poly.wsh$diam - crown_diameter_constant) /
          poly.wsh$height,
        my_limits[1]
      ),
      my_limits[2]
    )

    # build raster of average ratio
    dhr.rast <- terra::rasterize(poly.wsh,
      chm,
      field = "diam.height.ratio"
    )
    # smooth raster if applicable
    if (smoothing_radius >= chm.res) {
      window_size <- floor(smoothing_radius / chm.res) * 2 + 1
      double_window_size <- floor(2 * smoothing_radius / chm.res) * 2 + 1
      ratio.avg <- terra::focal(
        x = dhr.rast,
        w = window_size,
        fun = "mean",
        na.rm = T,
        pad = T
      )
      # where there are NA values, fill with double smoothing radius average
      ratio.avg[is.na(ratio.avg)] <- terra::focal(
        x = dhr.rast,
        w = double_window_size,
        fun = "mean",
        na.rm = T,
        pad = T
      )
    } else { # if smoothing radius is too small to be meaningful
      ratio.avg <- dhr.rast
    }

    # where there are NA values, fill with 10m average
    ratio.avg[is.na(ratio.avg)] <- terra::focal(
      x = dhr.rast,
      fun = "mean",
      na.rm = T,
      pad = T
    )
    # if there are still NA values, arbitrarily fill with 0.5
    ratio.avg[is.na(ratio.avg)] <- 0.5

    return(ratio.avg)
  }
)


# watershed_diameter_raster (dummy) for data frame  ----------------------------
#' @describeIn watershed_diameter_raster Calculate a raster of crown diameter
#' for tree height using watershed segmentation
#'
methods::setMethod(
  "watershed_diameter_raster",
  signature(point_cloud = "data.frame"),
  function(point_cloud,
           crown_diameter_constant,
           limits,
           ground_height,
           smoothing_radius,
           ...) {
    stop(paste(
      "watershed_diameter_raster is not (yet) implemented for point cloud",
      "of type data.frame."
    ), call. = FALSE)
    return(1)
  }
)

# watershed_diameter_raster (dummy) for LAScatalog -----------------------------
#' @describeIn watershed_diameter_raster Calculate a raster of crown diameter
#' for tree height using watershed segmentation
#'
#' @importClassesFrom lidR LAScatalog
methods::setMethod(
  "watershed_diameter_raster",
  signature(point_cloud = "LAScatalog"),
  function(point_cloud,
           crown_diameter_constant,
           limits,
           ground_height,
           smoothing_radius,
           ...) {
    stop(paste(
      "watershed_diameter_raster is not (yet) implemented for point cloud",
      "of type LasCatalog."
    ), call. = FALSE)
    return(1)
  }
)
