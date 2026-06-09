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


# Generic S4 Function -------------------------------------------------

#' Segment Tree Crowns in a 3D Point Cloud
#'
#' Employs a variant of the mean shift algorithm (Ferraz et. al, 2016) and after
#' that the DBSCAN algorithm in order to identify tree crowns in airborne lidar
#' data.
#'
#' @param point_cloud A data set containing xyz-coordinates. Can be passed as
#'   either a [data.frame][data.frame()], a
#'   [data.table][data.table::data.table()], a [LAS object][lidR::LAS-class] or
#'   a [LAScatalog][lidR::LAScatalog-class].
#'
#'   If it's a [data.frame][data.frame()] or a
#'   [data.table][data.table::data.table()] the function searches for coordinate
#'   columns by looking for the first numeric columns named "x"/"X", "y"/"Y", or
#'   "z"/"Z". For each instance where it can't find one of those it selects the
#'   next available numeric column in the table and issues a warning.
#' @param crown_diameter_to_tree_height Single number or
#'   [SpatRasters][terra::SpatRaster] covering the area of the `point_cloud`.
#'   The diameter of the search kernel will be calculated by multiplying this
#'   value and the height above ground of the kernel center, and adding the
#'   crown_diameter_constant. For details see "How the algorithm works". Points
#'   will not be segmented wherever a raster contains `NA` values.
#' @param crown_length_to_tree_height Single number or
#'   [SpatRasters][terra::SpatRaster] covering the area of the `point_cloud`.
#'   The height of the search kernel will be calculated by multiplying this
#'   value and the height above ground of the kernel center, and adding the
#'   crown_length_constant. For details see "How the algorithm works". Points
#'   will not be segmented wherever a raster contains `NA` values.
#' @param crown_diameter_constant,crown_length_constant Single number >=0.
#'    Used to determine the dimensions of the search kernel, together with the
#'    respective ratios to tree height. For details see "How the algorithm
#'    works".
#' @param segment_crowns_only_above A single positive number denoting the
#'   minimum height above ground at which crown IDs will be calculated.
#'
#'   Note that points directly below this threshold will still be considered
#'   during the segmentation if they are within reach of search kernels
#'   constructed at the `segment_crowns_only_above` height. See "How the
#'   algorithm works" to learn about the search kernels.
#' @param ground_height One of
#'   * `NULL`, indicating that `point_cloud` is normalized with ground height at
#'     zero.
#'   * A [SpatRaster][terra::SpatRaster] providing ground heights for the area
#'     of the (not normalized) `point_cloud`.
#'   * A list of (ideally named) arguments to the
#'     [lidR rasterize_terrain()][lidR::rasterize_terrain()] function, which
#'     will be used to generate a ground height grid from `point_cloud`.
#'     Currently not supported with point clouds stored in
#'     [data.frames][data.frame()]. The list should not contain an argument to
#'     the "las" parameter of [rasterize_terrain()][lidR::rasterize_terrain()].
#'
#'   Points will not be segmented wherever ground heights are NA.
#' @param crown_id_column_name A character string. The column or attribute name
#'   under which IDs for segmented bodies should be stored.
#' @param centroid_convergence_distance A single number. Distance at which it is
#'   assumed that subsequently calculated centroids have converged to the
#'   nearest mode. See "How the algorithm works" to learn about centroids and
#'   modes in the context of the AMS3D algorithm.
#' @param max_iterations_per_point A single integer. Maximum number of
#'   centroids calculated before the search for the nearest mode stops. See
#'   "How the algorithm works" to learn about centroids and modes in the context
#'   of the AMS3D algorithm.
#' @param dbscan_neighborhood_radius A single number. Radius for the spherical
#'   DBSCAN neighborhood around a mode. See "How the algorithm works" to learn
#'   about neighborhoods in the context of the DBSCAN algorithm.
#' @param min_num_points_per_crown A single integer. The minimum number of
#'   converged centroids within a DBSCAN neighborhood at which the centroid in
#'   the neighborhood's center will be treated as a core point. See "How the
#'   algorithm works" to learn about neighborhoods and core points in the
#'   context of the DBSCANb algorithm.
#' @param ... Unused.
#'
#' @return The point cloud which was passed to the function but extended with a
#'   column/attribute holding for each point the ID of a segmented body. IDs
#'   with the value `NA` indicate that a point was not assigned to any
#'   body.
#'
#'   If `also_return_terminal_centroids` and/or `also_return_all_centroids` were set to `TRUE`, a
#'   list with at most three named elements in the following order:
#'   \describe{
#'     \item{segmented_point_cloud}{
#'       The segmented point cloud which would have been returned directly if
#'       `also_return_terminal_centroids` and `also_return_all_centroids` had been set
#'       to `FALSE`.
#'     }
#'     \item{terminal_centroids}{
#'       If `also_return_terminal_centroids` was set to `TRUE`, a point cloud of
#'       the same type as the input point cloud holding the terminal centroids calculated
#'       with the AMS3D algorithm and two additional columns/attributes. One of
#'       these columns/attributes holds IDs of the segmented bodies that the
#'       modes belong to and the other (named "point_index") holds indices to
#'       the points in the input point cloud.
#'     }
#'     \item{centroids}{
#'       If `also_return_all_centroids` was set to `TRUE`, a point cloud
#'       of the same type as the input point cloud holding the centroids
#'       calculated with the AMS3D algorithm and two additional
#'       columns/attributes. One of these columns/attributes holds IDs of the
#'       segmented bodies that the centroids belong to and the other (named
#'       "point_index") holds indices to the points in the input point cloud.
#'     }
#'   }
#'
#'   The method for [LASCatalogs][lidR::LAScatalog-class] works just like
#'   any other lidR function that accepts them, i.e. it returns either an
#'   in-memory [LAS object][lidR::LAS-class] or writes the processed chunks to
#'   individual files and returns those file's names. Please refer to the
#'   [LASCatalog][lidR::LAScatalog-class] documentation for more details.
#'
#' @section How the algorithm works:
#'
#'   The basic assumption is that tree crowns form local maxima of point density
#'   and height within lidar point clouds. These local maxima are called
#'   *modes*. The algorithm tries to find the nearest mode for each point. This
#'   is done by looking at the surrounding points and moving into the direction
#'   of the highest point density until the nearest mode is (almost) reached.
#'
#'   The surrounding points are found with a search kernel (a three-dimensional
#'   search window) which has the shape of a vertical cylinder. According to
#'   literature, the algorithm works best if the search kernel has roughly the
#'   size of the surrounding crowns. Therefore, the parameters controlling the
#'   kernels dimension are simplistically called
#'   `crown_diameter_to_tree_height`, `crown_diameter_constant`, and
#'   `crown_lenght...` respectively. The diameter of the kernel is calculated
#'   from the  height above ground of the kernels center times the value for
#'   crown_diameter_to_tree_height, plus the crown_diameter constant. The
#'   height of the kernel is calculated respectively.
#'
#'   The direction of the highest point density is found by calculating the
#'   average position of all points within the cylinder, the cylinder's so
#'   called *centroid*. In order to move further into the direction of the
#'   highest point density, a new cylinder is placed on the centroid and a new
#'   centroid is calculated for that cylinder. This continues on until the
#'   cylinders "stop moving", i.e. until two subsequently calculated centroids
#'   are closer to each other than `centroid_convergence_distance`. At this
#'   point, the most recently calculated centroid, hence called 'terminal
#'   centroid', is assumed to be close enough to the mode, so that the
#'   original point can be linked to the respective tree top.
#'
#'   It sometimes happens that centroids converge only after a lot of
#'   iterations. In order to prevent situations where an excessive number of
#'   centroids is calculated for just one point, the parameter
#'   `max_iterations_per_point` is used to stop the centroid
#'   calculations after a certain number of them has been performed.
#'   Nonetheless, the last centroid found before stopping is still taken as
#'   a good enough guess of the nearest mode's position.
#'
#'   After the terminal centroids of the individual points have been
#'   calculated, it can be seen that terminal centroids of points belonging to
#'   the same tree crown are positioned very close to each other, shortly below
#'   the crown's apex. These dense clusters of terminal centroids are
#'   identified with the DBSCAN algorithm which assigns a cluster ID to every
#'   one of them. The cluster IDs are then finally connected back to the points
#'   of the point cloud and used as crown IDs.
#'
#'   The DBSCAN clustering is explained nicely in
#'   [Wikipedia](https://en.wikipedia.org/wiki/DBSCAN#Preliminary) but here
#'   is a quick sketch of what it does: The DBSCAN algorithm classifies points
#'   as either core points, border points, or noise and assigns core and border
#'   points to the same cluster if they are close enough to at least one other
#'   core point of the cluster.
#'
#'   In order to be core points, points need to have enough neighbors. The
#'   parameter `dbscan_neighborhood_radius` determines the radius of the
#'   neighborhood and the parameter `min_num_points_per_crown`
#'   determines the minimum number of points in the neighborhood (including the
#'   to-be-classified one), which are needed for a core point.
#'
#'   Border points are within the neighborhood of core points but don't have
#'   enough neighbors to be core points themselves. Noise points are not within
#'   the neighborhood of any core point and also don't have enough neighbors to
#'   be core points.
#'
#'   Clusters are identified by iterating over the points and classifying them
#'   one by one. For each point the neighborhood is scanned and the point is
#'   classified accordingly. If the point is a core or border point, the
#'   neighboring points are classified next. As long as it is possible to
#'   directly connect to new core or border points in this way, the same cluster
#'   ID is assigned to each encountered point.
#'
#' @references Ferraz, A., S. Saatchi, C. Mallet, and V. Meyer (2016)
#'   \emph{Lidar detection of individual tree size in tropical forests}. Remote
#'   Sensing of Environment 183:318–333. <doi:10.1016/j.rse.2016.05.028>
#' @references Ferraz, A., F. Bretar, S. Jaquemond, G. Gonçalves, L. Pereira,
#'   M. Tomé, and P. Soares (2012)
#'   \emph{3-D mapping of a multi-layered Mediteranean forest using ALS data}.
#'   Remote Sensing of Environment, 121:210-223.
#'   \doi{10.1016/j.rse.2012.01.020}
#'
#'
#' @example \dontrun{R/examples/segment_tree_crowns_examples.R}
#'
#' @export
methods::setGeneric("segment_tree_crowns",
  function(point_cloud,
           crown_diameter_to_tree_height,
           crown_length_to_tree_height,
           crown_diameter_constant = 0,
           crown_length_constant = 0,
           segment_crowns_only_above = 0,
           ground_height = NULL,
           crown_id_column_name = "crown_id",
           centroid_convergence_distance = 0.01,
           max_iterations_per_point = 500,
           dbscan_neighborhood_radius = 0.3,
           min_num_points_per_crown = 5,
           ...) {
    standardGeneric("segment_tree_crowns")
  },
  signature = "point_cloud"
)


# Method for data.frames and data.tables ----------------------------------

#' @describeIn segment_tree_crowns Segments coordinates stored as three columns
#'   in a [data.frame][data.frame()] or [data.table][data.table::data.table()].
#'
#' @param verbose `TRUE` or `FALSE`. Should the function show a progress bar and
#'   other runtime information in the console?
#' @param also_return_terminal_centroids `TRUE` or `FALSE`. Should mode coordinates be
#'   returned as well?
#' @param also_return_all_centroids `TRUE` or `FALSE`. Should all centroid coordinates
#'   be returned as well? This slows down processing by a little bit and will
#'   return a data set which requires at least ~10 times more memory than the
#'   input point cloud.
methods::setMethod(
  "segment_tree_crowns",
  signature(point_cloud = c("data.frame")),
  function(point_cloud,
           crown_diameter_to_tree_height,
           crown_length_to_tree_height,
           crown_diameter_constant,
           crown_length_constant,
           segment_crowns_only_above,
           ground_height,
           crown_id_column_name,
           centroid_convergence_distance,
           max_iterations_per_point,
           dbscan_neighborhood_radius,
           min_num_points_per_crown,
           verbose = TRUE,
           also_return_terminal_centroids = FALSE,
           also_return_all_centroids = FALSE) {
    validate_coordinate_table(point_cloud)
    validate_kernel_params(
      crown_diameter_to_tree_height,
      crown_diameter_constant,
      point_cloud,
      which = "diameter"
    )
    validate_kernel_params(
      crown_length_to_tree_height,
      crown_length_constant,
      point_cloud,
      which = "length"
    )
    validate_segment_crowns_only_above(segment_crowns_only_above)
    validate_ground_height(ground_height, point_cloud)
    validate_crown_id_column_name(crown_id_column_name, point_cloud)
    validate_centroid_convergence_distance(centroid_convergence_distance)
    validate_max_iterations_per_point(max_iterations_per_point)
    validate_dbscan_neighborhood_radius(dbscan_neighborhood_radius)
    validate_min_num_points_per_crown(min_num_points_per_crown)
    validate_verbose(verbose)
    validate_also_return_terminal_centroids(also_return_terminal_centroids)
    validate_also_return_all_centroids(also_return_all_centroids)


    # Segment the point cloud
    ids_terminal_centroids <- segment_tree_crowns_core(
      point_cloud,
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
      also_return_all_centroids
    )

    # Bind the IDs to the point cloud data
    if (data.table::is.data.table(point_cloud)) {
      res <- data.table::data.table(
        point_cloud,
        ids_terminal_centroids$crown_ids
      )
    } else {
      res <- cbind(
        point_cloud,
        ids_terminal_centroids$crown_ids
      )
    }
    # Rename the ID column with the requested name
    names(res)[ncol(res)] <- crown_id_column_name

    # Return the point cloud with crown IDs if no additional data was requested
    if (!also_return_terminal_centroids && !also_return_all_centroids) {
      return(res)
    } else { # set up a list with the different result data sets

      res <- list(segmented_point_cloud = res)

      if (also_return_terminal_centroids) {
        # Add the terminal centroids to the list
        if (data.table::is.data.table(point_cloud)) {
          res[["terminal_centroids"]] <- ids_terminal_centroids$terminal_coordinates
        } else {
          res[["terminal_centroids"]] <- as.data.frame(
            ids_terminal_centroids$terminal_coordinates
          )
        }
        # Rename the ID column with the requested name
        names(res$terminal_centroids)[4] <- crown_id_column_name
      }

      if (also_return_all_centroids) {
        # Add the centroids to the list
        if (data.table::is.data.table(point_cloud)) {
          res[["centroids"]] <- ids_terminal_centroids$centroid_coordinates
        } else {
          res[["centroids"]] <- as.data.frame(
            ids_terminal_centroids$centroid_coordinates
          )
        }
        # Rename the ID column with the requested name
        names(res$centroids)[4] <- crown_id_column_name
      }

      return(res)
    }
  }
)

# Method for lidR::LAS Objects ----------------------------------------

#' @describeIn segment_tree_crowns Segments the point cloud data of a
#'   [LAS object][lidR::LAS-class].
#'
#' @param write_crown_id_also_to_file `TRUE` or `FALSE`. When writing
#'   the returned LAS object to disk, should the IDs of segmented bodies be
#'   written into that file as well? See the
#'   [lidR function add_lasattribute()][lidR::add_lasattribute()] for additional
#'   details. Will also be used for all attributes of the
#'   [LAS object(s)][lidR::LAS-class] which are returned if `also_return_terminal_centroids`
#'   and/or `also_return_all_centroids` were set to `TRUE`.
#'
#'   For [LAScatalogs][lidR::LAScatalog-class], this is only used if the
#'   result is returned as a [LAS object][lidR::LAS-class] in memory. If the
#'   LAScatalog is set up to write the segmented point clouds into files, the
#'   IDs of segmented bodies will always be written to these files as well.
#'
#' @param crown_id_file_description A character string. If
#'   `write_crown_id_also_to_file` is set to `TRUE` this will be used
#'   as an additional description of the IDs of segmented bodies when the LAS
#'   object is written to disk. See the "desc" parameter of the
#'   [lidR function add_lasattribute()][lidR::add_lasattribute()] for additional
#'   details.
#'
#' @importClassesFrom lidR LAS
methods::setMethod(
  "segment_tree_crowns",
  signature(point_cloud = "LAS"),
  function(point_cloud,
           crown_diameter_to_tree_height,
           crown_length_to_tree_height,
           crown_diameter_constant,
           crown_length_constant,
           segment_crowns_only_above,
           ground_height,
           crown_id_column_name,
           centroid_convergence_distance,
           max_iterations_per_point,
           dbscan_neighborhood_radius,
           min_num_points_per_crown,
           verbose = TRUE,
           also_return_terminal_centroids = FALSE,
           also_return_all_centroids = FALSE,
           write_crown_id_also_to_file = FALSE,
           crown_id_file_description = crown_id_column_name) {
    validate_kernel_params(
      crown_diameter_to_tree_height,
      crown_diameter_constant,
      point_cloud,
      which = "diameter"
    )
    validate_kernel_params(
      crown_length_to_tree_height,
      crown_length_constant,
      point_cloud,
      which = "length"
    )
    validate_segment_crowns_only_above(segment_crowns_only_above)
    validate_ground_height(ground_height, point_cloud)
    validate_crown_id_column_name(crown_id_column_name, point_cloud@data)
    validate_centroid_convergence_distance(centroid_convergence_distance)
    validate_max_iterations_per_point(max_iterations_per_point)
    validate_dbscan_neighborhood_radius(dbscan_neighborhood_radius)
    validate_min_num_points_per_crown(min_num_points_per_crown)
    validate_verbose(verbose)
    validate_also_return_terminal_centroids(also_return_terminal_centroids)
    validate_also_return_all_centroids(also_return_all_centroids)
    validate_write_crown_id_also_to_file(write_crown_id_also_to_file)
    validate_crown_id_file_description(crown_id_file_description)

    # If ground_height is a list of arguments, pass them to
    # lidR::rasterize_terrain
    if (is.list(ground_height)) {
      ground_height <- do.call(lidR::rasterize_terrain,
        args = c(las = point_cloud, ground_height)
      )
    }

    # If crown_diameter_to_tree_height is an algorithm name, calculate a raster
    if (is.character(crown_diameter_to_tree_height)) {
      if (crown_diameter_to_tree_height %in% c("li", "li2012")) {
        crown_diameter_to_tree_height <-
          crownsegmentr::li_diameter_raster(point_cloud,
            crown_diameter_constant,
            ground_height = ground_height
          )
      } else if (crown_diameter_to_tree_height %in% c("ws", "watershed")) {
        crown_diameter_to_tree_height <-
          crownsegmentr::watershed_diameter_raster(point_cloud,
            crown_diameter_constant,
            ground_height = ground_height
          )
      }
    }


    # Segment the point cloud
    ids_terminal_centroids <- segment_tree_crowns_core(
      point_cloud@data,
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
      also_return_all_centroids
    )

    # Bind the IDs to the point cloud data using the requested name
    if (write_crown_id_also_to_file) { # IDs will be saved with LAS files
      res <- lidR::add_lasattribute(
        las = point_cloud,
        x = ids_terminal_centroids$crown_ids,
        name = crown_id_column_name,
        desc = crown_id_file_description
      )
    } else { # IDs will only be available in memory
      res <- lidR::add_attribute(
        las = point_cloud,
        x = ids_terminal_centroids$crown_ids,
        name = crown_id_column_name
      )
    }

    # Return the point cloud with crown IDs if no additional data was requested
    if (!also_return_terminal_centroids && !also_return_all_centroids) {
      return(res)
    } else { # set up a list with the different result data sets

      res <- list(segmented_point_cloud = res)

      # For R CMD check which sees variables in data.table syntax as global
      . <- x <- y <- z <- NULL

      if (also_return_terminal_centroids) {
        # Create a LAS object storing the mode coordinates
        res[["terminal_centroids"]] <- suppressWarnings(
          lidR::las_update(
            lidR::las_quantize(
              lidR::LAS(
                ids_terminal_centroids$terminal_coordinates[
                  , .(X = x, Y = y, Z = z)
                ],
                header = point_cloud@header
              )
            )
          )
        )

        # Add the crown IDs and point indices to the terminal centroids
        res$terminal_centroids <- lidR::add_attribute(
          las = lidR::add_attribute(
            las = res$terminal_centroids,
            x = ids_terminal_centroids$terminal_coordinates$crown_id,
            name = crown_id_column_name
          ),
          x = ids_terminal_centroids$terminal_coordinates$point_index,
          name = "point_index"
        )

        # If requested for the crown IDs, make the crown ID and point index
        # attributes of the terminal_centroids LAS "file-writable" as well
        if (write_crown_id_also_to_file) {
          res$terminal_centroids <- lidR::add_lasattribute(
            las = lidR::add_lasattribute(
              las = res$terminal_centroids,
              name = crown_id_column_name,
              desc = crown_id_file_description
            ),
            name = "point_index",
            desc = "Index of original points"
          )
        }
      } # end if also_return_terminal_centroids

      if (also_return_all_centroids) {
        # Create a LAS object storing the centroid coordinates
        res[["centroids"]] <- suppressWarnings(
          lidR::las_update(
            lidR::las_quantize(
              lidR::LAS(
                ids_terminal_centroids$centroid_coordinates[
                  , .(X = x, Y = y, Z = z)
                ],
                header = point_cloud@header
              )
            )
          )
        )

        # Add the crown IDs and point indices to the centroids
        res$centroids <- lidR::add_attribute(
          las = lidR::add_attribute(
            las = res$centroids,
            x = ids_terminal_centroids$centroid_coordinates$crown_id,
            name = crown_id_column_name
          ),
          x = ids_terminal_centroids$centroid_coordinates$point_index,
          name = "point_index"
        )

        # If requested for the crown IDs, make the crown ID and point index
        # attributes of the centroids LAS "file-writable" as well
        if (write_crown_id_also_to_file) {
          res$centroids <- lidR::add_lasattribute(
            las = lidR::add_lasattribute(
              las = res$centroids,
              name = crown_id_column_name,
              desc = crown_id_file_description
            ),
            name = "point_index",
            desc = "Index of original points"
          )
        }
      } # end if also_return_all_centroids

      return(res)
    } # end returning mode and/or centroid coordinates
  }
)


# Method for lidR::LAScatalog Objects ---------------------------------
utils::globalVariables(c("crown_id_column_name", "..crown_id_column_name"))

#' @describeIn segment_tree_crowns Segments the point cloud data of a
#'   [LAScatalog][lidR::LAScatalog-class]. This method does not support
#'   additionally returning centroids. Instead of the verbose
#'   parameter use the LAScatalog's progress option (see the
#'   [LAScatalog documentation][lidR::LAScatalog-class] -> "Processing options"
#'   -> "progress").
#'
#' @importClassesFrom lidR LAScatalog
methods::setMethod(
  "segment_tree_crowns",
  signature(point_cloud = "LAScatalog"),
  function(point_cloud,
           crown_diameter_to_tree_height,
           crown_length_to_tree_height,
           crown_diameter_constant,
           crown_length_constant,
           segment_crowns_only_above,
           ground_height,
           crown_id_column_name,
           centroid_convergence_distance,
           max_iterations_per_point,
           dbscan_neighborhood_radius,
           min_num_points_per_crown,
           write_crown_id_also_to_file = TRUE,
           crown_id_file_description = crown_id_column_name) {
    validate_scale_n_offset_are_consistent(point_cloud)
    validate_kernel_params(
      crown_diameter_to_tree_height,
      crown_diameter_constant,
      point_cloud,
      which = "diameter"
    )
    validate_kernel_params(
      crown_length_to_tree_height,
      crown_length_constant,
      point_cloud,
      which = "length"
    )
    # validate_crown_id_column_name(crown_id_column_name, point_cloud@data)
    # TODO find out whether there is a way to validate this here instead of in
    # the individual calls to the LAS methods.
    validate_segment_crowns_only_above(segment_crowns_only_above)
    validate_ground_height(ground_height, point_cloud)
    validate_centroid_convergence_distance(centroid_convergence_distance)
    validate_max_iterations_per_point(max_iterations_per_point)
    validate_dbscan_neighborhood_radius(dbscan_neighborhood_radius)
    validate_min_num_points_per_crown(min_num_points_per_crown)
    write_crown_id_also_to_file <-
      validate_write_crown_id_also_to_file_for_LAScatalogs(
        write_crown_id_also_to_file,
        point_cloud
      )
    validate_crown_id_file_description(crown_id_file_description)


    # Used for selecting the x and y coordinates of the highest point of a tree
    # crown point cloud. Called in the catalog_function below.
    select_apex_helper <- function(x, y, z) {
      max_z_index <- which.max(z)
      return(list(
        X = x[max_z_index],
        Y = y[max_z_index]
      ))
    }

    # This function is forwarded to the lidR::LAScatalog framework by which it
    # is used to process the individual chunks of the input LAScatalog.
    catalog_function <- function(las, bbox, ...) {
      # segment the chunk with the LAS method
      segmented_las <- segment_tree_crowns(
        point_cloud = las,
        crown_diameter_to_tree_height,
        crown_length_to_tree_height,
        crown_diameter_constant,
        crown_length_constant,
        segment_crowns_only_above,
        ground_height,
        crown_id_column_name,
        centroid_convergence_distance,
        max_iterations_per_point,
        dbscan_neighborhood_radius,
        min_num_points_per_crown,
        verbose = FALSE,
        also_return_terminal_centroids = FALSE,
        also_return_all_centroids = FALSE,
        write_crown_id_also_to_file,
        crown_id_file_description
      )

      # For R CMD check which sees variables in data.table syntax as global
      X <- Y <- Z <- NULL

      # Get the apex xy-coordinates and ID of each tree crown
      # Then select only those crowns whose apex lies in the core area
      core_apices <- segmented_las@data[
        !is.na(segmented_las@data[[crown_id_column_name]]),
        select_apex_helper(X, Y, Z),
        by = crown_id_column_name
      ][
        # filter the apices that are inside the core area
        bbox$xmin <= X & X < bbox$xmax &
          bbox$ymin <= Y & Y < bbox$ymax,
      ]
      # -> I could use the buffer atrribute here instead but I have observed
      # that lidR sometimes produces duplicated trees on chunk borders and
      # that's what I am trying to prevent here by using the <= and <.
      # TODO Test whether this actually makes a difference.

      # For R CMD check which sees variables in data.table syntax as global
      buffer <- NULL

      # Get the trees and any points with ID == NA in the core area
      # I.e. in the end, trees at the chunk edge will extend into the buffer
      # while unsegmented points are only selected inside the core area.
      core_trees_n_points <- lidR::filter_poi(
        segmented_las,
        # i.e. either the point's ID belongs to one of the trees in the core
        # area...
        segmented_las@data[[crown_id_column_name]] %in%
          core_apices[[crown_id_column_name]] |
          # ...or the ID is NA and the point is inside the core area.
          (
            is.na(segmented_las@data[[crown_id_column_name]]) &
              as.integer(buffer) == 0
          )
        # TODO Instead of the buffer I could use the same filter here as with
        # the apices above but I guess a few duplicated points that don't
        # belong to any tree won't be a big problem anytime soon. One might
        # try to account for such artifacts after processing the catalog.
      )

      # Calculate cross-chunk unique crown IDs
      # The following lines were adapted from the source code of the
      # lidR::find_trees function
      # (https://github.com/Jean-Romain/lidR/blob/14fadf2735b6dca425a9c65da65943eda19a4a25/R/find_trees.R#L105)
      x_offset <- las@header@PHB[["X offset"]]
      y_offset <- las@header@PHB[["Y offset"]]

      x_scale <- las@header@PHB[["X scale factor"]]
      y_scale <- las@header@PHB[["Y scale factor"]]

      # For R CMD check which sees variables in data.table syntax as global
      . <- NULL

      # Connect the IDs of the core apices with the their respective bitshift
      # IDs
      bit_shift_ids <- core_apices[, .(
        crown_id = core_apices[[crown_id_column_name]],
        bit_shift_id =
          round((X - x_offset) / x_scale) * 2^32 +
            round((Y - y_offset) / y_scale)
      )]
      # Adaption end

      # The ID of unsegmented points is missing from the core apices so let's
      # add it here
      bit_shift_ids <- rbind(
        bit_shift_ids,
        data.table::data.table(crown_id = NA, bit_shift_id = NA)
      )
      # Keep IDs with the value NA at that value
      # -> I think this is unnecessary
      # bit_shift_ids[is.na(crown_id), bit_shift_id := NA]

      # Give the crown_id column in the bit_shift_ids data.table the same name
      # as the crown IDs in the point cloud data
      names(bit_shift_ids)[1] <- crown_id_column_name

      # Join the bitshift IDs to the individual points based on their crown IDs
      joined_ids <- bit_shift_ids[
        core_trees_n_points@data[, ..crown_id_column_name],
        on = (crown_id_column_name)
      ]$bit_shift_id

      # Overwrite the "normal" crown IDs with the bitshift ones
      core_trees_n_points@data[, (crown_id_column_name) := joined_ids]

      # Remove the buffer column
      core_trees_n_points@data[, buffer := NULL]

      return(core_trees_n_points)
    }

    return(lidR::catalog_apply(
      ctg = point_cloud,
      FUN = catalog_function,
      .options = list(need_buffer = TRUE, automerge = TRUE, autoread = TRUE)
    ))
  }
)
