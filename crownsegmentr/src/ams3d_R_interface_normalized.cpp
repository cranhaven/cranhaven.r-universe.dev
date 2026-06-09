// This file is part of crownsegmentr, an R package for identifying tree crowns
// within 3D point clouds.
//
// Copyright (C) 2025 Leon Steinmeier, Timon Miesner, Nikolai Knapp
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

//' Searches modes with the AMS3D algorithm for a lidar point cloud of a forest
//'
//' Employs the 3D adaptive mean shift algorithm (Ferraz et al., 2016) to
//' estimate the mode of each point in a point cloud which is assumed to contain
//' trees. In this context the mode is a theoretical "center of mass" of a tree
//' crown point cloud, that is usually located shortly below the crown apex.
//'
//' @param coordinate_table A \code{data.frame}. The first three columns are
//'     treated as the x-, y-, and z-coordinates of an airborne lidar point
//'     cloud.
//' @param min_point_height_above_ground A single positive number. The minimum
//'     point height above ground at which the function will calculate
//'     centroids.
//' @param crown_diameter_to_tree_height,crown_length_to_tree_height Single
//'     numbers. Determine the size of the search kernel (bandwidth) of the
//'     algorithm, as a function of height above ground. The kernel should have
//'     roughly the size of the expected tree crowns. If the intercepts are
//'     zero, the slopes translate to ratios of crown diameter to tree height
//'     or crown length to tree height, respectively.
//' @param crown_diameter_constant Single number >=0. Intercept for the linear
//'     function determining the kernel diameter (bandwidth) in relationship to
//'     the height above ground.
//' @param crown_length_constant Single number >=0.  Intercept for the linear
//'     function determining the kernel height (bandwidth) in relationship to
//'     the height above ground.
//' @param centroid_convergence_distance Numeric Scalar. Distance at which it is
//'     assumed that subsequently calculated centroids have converged to the
//'     nearest mode.
//' @param max_iterations_per_point Integer Scalar. Maximum number of
//'     centroids calculated before the search for the nearest mode stops.
//' @param also_return_all_centroids Boolean Scalar. Should all centroid coordinates be
//'     returned as well?
//' @param show_progress_bar Boolean Scalar. Should a progress bar be shown
//'     during the computation?
//'
//' @returns A list with either one or two elements:
//'     \itemize{
//'         \item The first element (named
//'             "terminal_coordinates") contains the terminal centroids for all points in the
//'             \code{coordinate_table}. These are stored in a \code{data.frame}
//'             with three columns that hold the x-, y-, and z-coordinates and they are
//'             stored in the same order as their respective points in the
//'             \code{coordinate_table}.
//'         \item The second element (named "centroid_coordinates") is only present if
//'             \code{also_return_all_centroids} was set to \code{TRUE} and contains the
//'             centroids calculated during the mode finding process. The prior centroids are
//'             stored in a \code{data.frame} with xyz-coordinate columns like the
//'             terminal centroids. To enable grouping of these centroids by the point they belong
//'             to, there is one additional column (named "point_index") which holds row
//'             indices of the corresponding points in the \code{coordinate_table}.
//'     }
//'
//' @references Ferraz, A., S. Saatchi, C. Mallet, and V. Meyer (2016)
//'     \emph{Lidar detection of individual tree size in tropical forests}.
//'     Remote Sensing of Environment 183:318–333.
//'     \doi{10.1016/j.rse.2016.05.028}.
//'
// [[Rcpp::export]]
Rcpp::List calculate_centroids_normalized (
    const Rcpp::DataFrame &coordinate_table,
    const spatial::coordinate_t &min_point_height_above_ground,
    const double crown_diameter_to_tree_height,
    const double crown_length_to_tree_height,
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

    // Set up a spatial index for the point objects.
    spatial::index_for_3d_points_t point_cloud_index {
        spatial::create_index_of_finite (
            points,
            ams3d::_Kernel::bottom_height_above_ground_with (
                min_point_height_above_ground,
                crown_length_to_tree_height,
                crown_length_constant
            )
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
                    crown_diameter_to_tree_height,
                    crown_length_to_tree_height,
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
                    crown_diameter_to_tree_height,
                    crown_length_to_tree_height,
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
