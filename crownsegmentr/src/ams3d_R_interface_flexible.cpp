// This file is part of crownsegmentr, an R package for identifying tree crowns
// within 3D point clouds.
//
// Copyright (C) 2025 Leon Steinmeier, Nikolai Knapp, UFZ Leipzig
// Contact: timon.miesner@thuenen.de
//
// crownsegmentr is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// crownsegmentr is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with crownsegmentr in a file called "COPYING". If not,
// see <http://www.gnu.org/licenses/>.


#include "ams3d_R_interface.h"

#include "spatial.h"
#include "ams3d.h"

//' @describeIn calculate_centroids_normalized Can take either a single value or
//'     raster data for both the ground height and the
//'     \code{crown_diameter_to_tree_height} and
//'     \code{crown_length_to_tree_height} parameters.
//'
//' @param ground_height_data A list containing either a single ground height
//'     value (named "value") or a set of elements that make up a ground height
//'     raster covering the whole area of the point cloud. Such a set has to
//'     consist of the named elements described in the section "Raster argument
//'     structure" below.
//' @param crown_diameter_to_tree_height_data A list containing either a single
//'     numeric value (named "value") or the data for a raster of values (see
//'     section "Raster argument structure" below for how the raster data has to
//'     be stored in the list). The values indicate the estimated ratio of crown
//'     diameter to tree height for the whole plot or individual raster pixels
//'     respectively.
//' @param crown_length_to_tree_height_data A list containing either a single
//'     numeric value (named "value") or the data for a raster of values (see
//'     section "Raster argument structure" below for how the raster data has to
//'     be stored in the list). The values indicate the estimated ratio of crown
//'     height to tree height for the whole plot or individual raster pixels
//'     respectively.
//' @param crown_diameter_constant Single number >=0. Intercept for the linear
//'     function determining the kernel diameter (bandwidth) in relationship to
//'     the height above ground.
//' @param crown_length_constant Single number >=0.  Intercept for the linear
//'     function determining the kernel height (bandwidth) in relationship to
//'     the height above ground.
//'
//' @section Raster argument structure:
//'     Raster data has to be passed as a list comprising the following named
//'     elements:
//'     \itemize{
//'         \item values: Numeric vector holding the values.
//'         \item num_rows: Integer number indicating the number of rows.
//'         \item num_cols: Integer number indicating the number of columns.
//'         \item x_min: Number indicating the lowest x coordinate covered.
//'         \item x_max: Number indicating the largest x coordinate covered.
//'         \item y_min: Number indicating the lowest y coordinate covered.
//'         \item y_max: Number indicating the largest y coordinate covered.
//'     }
//'
// [[Rcpp::export]]
Rcpp::List calculate_centroids_flexible (
    const Rcpp::DataFrame &coordinate_table,
    const spatial::coordinate_t &min_point_height_above_ground,
    const Rcpp::List &ground_height_data,
    const Rcpp::List &crown_diameter_to_tree_height_data,
    const Rcpp::List &crown_length_to_tree_height_data,
    const double crown_diameter_constant,
    const double crown_length_constant,
    const spatial::distance_t &centroid_convergence_distance,
    const int max_iterations_per_point,
    const bool also_return_all_centroids,
    const bool show_progress_bar
) {
    // Convert the coordinate table to an array of point objects.
    std::vector< spatial::point_3d_t > points {
        ams3d_R_interface_util::create_point_objects_from( coordinate_table )
    };

    // Convert the ground height data into a shared pointer to a raster object.
    std::shared_ptr< spatial::I_Raster< spatial::coordinate_t > >
    ground_height_grid_ptr {
        ams3d_R_interface_util::convert_list_argument_to_double_raster_ptr (
            ground_height_data
        )
    };

    // Convert the crown diameter to tree height data into a unique pointer to a
    // raster object.
    std::unique_ptr< spatial::I_Raster< double > >
    crown_diameter_to_tree_height_grid_ptr {
        ams3d_R_interface_util::convert_list_argument_to_double_raster_ptr (
            crown_diameter_to_tree_height_data
        )
    };

    // Convert the crown height to tree height data into a unique pointer to a
    // raster object.
    std::unique_ptr< spatial::I_Raster< double > >
    crown_length_to_tree_height_grid_ptr {
        ams3d_R_interface_util::convert_list_argument_to_double_raster_ptr (
            crown_length_to_tree_height_data
        )
    };

    // Create a shared pointer to a raster of kernel bottom heights above ground.
    std::shared_ptr< spatial::I_Raster< spatial::coordinate_t > >
    kernel_bottom_height_above_ground_grid_ptr {
        ams3d::_Kernel::bottom_height_above_ground_grid_with (
            min_point_height_above_ground,
            *crown_length_to_tree_height_grid_ptr,
            crown_length_constant
        )
    };

    // Set up a spatial index for the point objects.
    spatial::index_for_3d_points_t point_cloud_index {
        spatial::create_index_of_above_ground (
            points,
            kernel_bottom_height_above_ground_grid_ptr,
            ground_height_grid_ptr
        )
    };

    // Set up an array for the terminal centroids to be calculated.
    std::vector< spatial::point_3d_t > terminal_centroids{};
    terminal_centroids.reserve( points.size() );

    // Optionally set up a progress bar.
    RProgress::RProgress progress_bar;
    if (show_progress_bar) {
        progress_bar = ams3d_R_interface_util::create_progress_bar (
            points.size()
        );
        progress_bar.tick( 0 );
    };

    // Set up arrays for the optionally returned centroids and their point indices.
    std::vector< spatial::point_3d_t > centroids{};
    std::vector< int > point_indices{};

    int point_index{ 1 }; // 1-based point index for use with the centroids in R

    if (also_return_all_centroids)
    {
        // For all points in the input point cloud...
        for (const auto &point : points)
        {
            // ...calculate their mode and get the centroids as well.
            std::pair< spatial::point_3d_t, std::vector< spatial::point_3d_t > >
            all_centroids {
                ams3d::calculate_all_centroids (
                    point,
                    point_cloud_index,
                    min_point_height_above_ground,
                    *ground_height_grid_ptr,
                    *crown_diameter_to_tree_height_grid_ptr,
                    *crown_length_to_tree_height_grid_ptr,
                    crown_diameter_constant,
                    crown_length_constant,
                    centroid_convergence_distance,
                    max_iterations_per_point
                )
            };

            // Store the calculated mode.
            terminal_centroids.push_back( all_centroids.first );

            // Store the calculated centroids.
            centroids.insert (
                centroids.end(), // append at the end of centroids
                all_centroids.second.begin(),
                all_centroids.second.end()
            );

            // Store the current point index as many times as there are centroids.
            point_indices.insert (
                point_indices.end(), // Append at the end of point_indices...
                all_centroids.second.size(), // ...n_centroid times...
                point_index // ...this value.
            );

            point_index++;

            if (terminal_centroids.size() % ams3d_R_interface_constants::num_points_per_tick == 0)
            {
                // Check whether the R user wants to abort the computation
                Rcpp::checkUserInterrupt();

                // Advance the progress bar.
                if (show_progress_bar)
                {
                    progress_bar.tick (
                        ams3d_R_interface_constants::num_points_per_tick
                    );
                }
            }
        }
    }
    else // Do not return centroids.
    {
        // For all points in the input point cloud...
        for (const auto &point : points)
        {
            // ...calculate their mode.
            terminal_centroids.push_back (
                ams3d::calculate_terminal_centroid (
                    point,
                    point_cloud_index,
                    min_point_height_above_ground,
                    *ground_height_grid_ptr,
                    *crown_diameter_to_tree_height_grid_ptr,
                    *crown_length_to_tree_height_grid_ptr,
                    crown_diameter_constant,
                    crown_length_constant,
                    centroid_convergence_distance,
                    max_iterations_per_point
                )
            );

            if (terminal_centroids.size() % ams3d_R_interface_constants::num_points_per_tick == 0)
            {
                // Check whether the R user wants to abort the computation
                Rcpp::checkUserInterrupt();

                // Advance the progress bar.
                if (show_progress_bar)
                {
                    progress_bar.tick (
                        ams3d_R_interface_constants::num_points_per_tick
                    );
                }
            }
        }
    }

    // Finish the progress bar.
    if (show_progress_bar) { progress_bar.tick( points.size() ); }

    // Return the centroids to R
    return ams3d_R_interface_util::create_return_data (
        also_return_all_centroids,
        terminal_centroids,
        centroids,
        point_indices
    );
}
