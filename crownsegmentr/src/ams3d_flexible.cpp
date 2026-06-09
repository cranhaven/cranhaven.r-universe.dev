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


#include "ams3d.h"

#include "spatial.h"

namespace ams3d
{
    spatial::point_3d_t calculate_terminal_centroid (
        const spatial::point_3d_t &point,
        const spatial::index_for_3d_points_t &indexed_point_cloud,
        const spatial::coordinate_t &min_point_height_above_ground,
        const spatial::I_Raster< spatial::coordinate_t > &ground_height_grid,
        const spatial::I_Raster< double > &crown_diameter_to_tree_height_grid,
        const spatial::I_Raster< double > &crown_length_to_tree_height_grid,
        const double crown_diameter_constant,
        const double crown_length_constant,
        const spatial::distance_t &centroid_convergence_distance,
        const int max_iterations_per_point
    ) {
        // If any coordinate value of point is non-finite, return NaN.
        if (spatial::has_non_finite_coordinate_value( point ))
            { return spatial::nan_point(); }

        // Get the ground height at point.
        spatial::coordinate_t ground_height {
            ground_height_grid.no_throw_value_at_xy_of( point )
        };

        // If the ground height is non-finite or the point lies below the
        // minimum above-ground height, return NaN.
        if (!std::isfinite( ground_height ) ||
            spatial::get_z( point ) - ground_height
                < min_point_height_above_ground)
        {
            return spatial::nan_point();
        }

        // Set the current centroid to the starting point because it will be
        // queried in the loop below.
        spatial::point_3d_t current_centroid{ point };
        spatial::point_3d_t former_centroid{};

        int num_calculated_centroids{ 0 };
        do
        {
            // Get the ground height and the kernel dimension parameters at the
            // current centroid.
            ground_height = ground_height_grid.no_throw_value_at_xy_of (
                current_centroid
            );
            double crown_diameter_to_tree_height {
                crown_diameter_to_tree_height_grid.no_throw_value_at_xy_of (
                    current_centroid
                )
            };
            double crown_length_to_tree_height {
                crown_length_to_tree_height_grid.no_throw_value_at_xy_of (
                    current_centroid
                )
            };

            // If the ground height or the kernel dimension parameters are
            // non-finite, return NaN.
            if (!std::isfinite( ground_height ) ||
                !std::isfinite( crown_diameter_to_tree_height ) ||
                !std::isfinite( crown_length_to_tree_height ))
            {
                return spatial::nan_point();
            }

            // Create a kernel at the previously calculated centroid (which is
            // the starting point during the first iteration).
            _Kernel kernel {
                current_centroid,
                ground_height,
                crown_diameter_to_tree_height,
                crown_length_to_tree_height,
                crown_diameter_constant,
                crown_length_constant
            };

            // Store the current centroid and calculate a new centroid with the
            // new kernel.
            former_centroid = current_centroid;
            current_centroid = kernel.calculate_centroid_in(
                indexed_point_cloud
            );

            num_calculated_centroids++;
        }
        while (
            spatial::distance( former_centroid, current_centroid ) >
            centroid_convergence_distance
            && num_calculated_centroids < max_iterations_per_point
        );

        return current_centroid;
    }


    std::pair< spatial::point_3d_t, std::vector< spatial::point_3d_t > >
    calculate_all_centroids (
        const spatial::point_3d_t &point,
        const spatial::index_for_3d_points_t &indexed_point_cloud,
        const spatial::coordinate_t &min_point_height_above_ground,
        const spatial::I_Raster< spatial::coordinate_t > &ground_height_grid,
        const spatial::I_Raster< double > &crown_diameter_to_tree_height_grid,
        const spatial::I_Raster< double > &crown_length_to_tree_height_grid,
        const double crown_diameter_constant,
        const double crown_length_constant,
        const spatial::distance_t &centroid_convergence_distance,
        const int max_iterations_per_point
    ) {
        // If any coordinate value of point is non-finite, return NaN as 
        // terminal centroid, and no prior centroids.
        if (spatial::has_non_finite_coordinate_value( point ))
        {
            return std::pair {
                spatial::nan_point(), std::vector< spatial::point_3d_t >{}
            };
        }

        // Get the ground height at point.
        spatial::coordinate_t ground_height {
            ground_height_grid.no_throw_value_at_xy_of( point )
        };

        // If the ground height is non-finite or the point lies below the
        // minimum above-ground height, return NaN as terminal centroid, and no
        // prior centroids.
        if (!std::isfinite( ground_height ) ||
            spatial::get_z( point ) - ground_height
                < min_point_height_above_ground)
        {
            return std::pair {
                spatial::nan_point(), std::vector< spatial::point_3d_t >{}
            };
        }

        // Set the current centroid to the starting point because it will be
        // queried in the loop below.
        spatial::point_3d_t current_centroid{ point };
        spatial::point_3d_t former_centroid{};

        // Create an array for calculated centroids.
        std::vector< spatial::point_3d_t > centroids;
        centroids.reserve( 60 );

        int num_calculated_centroids{ 0 };
        do
        {
            // Get the ground height and the kernel dimension parameters at the
            // current centroid.
            ground_height = ground_height_grid.no_throw_value_at_xy_of (
                current_centroid
            );
            double crown_diameter_to_tree_height {
                crown_diameter_to_tree_height_grid.no_throw_value_at_xy_of (
                    current_centroid
                )
            };
            double crown_length_to_tree_height {
                crown_length_to_tree_height_grid.no_throw_value_at_xy_of (
                    current_centroid
                )
            };

            // If the ground height or the kernel dimension parameters are
            // non-finite, return NaN as terminal centroid, and no prior 
            // centroids.
            if (!std::isfinite( ground_height ) ||
                !std::isfinite( crown_diameter_to_tree_height ) ||
                !std::isfinite( crown_length_to_tree_height ))
            {
                return std::pair {
                    spatial::nan_point(), std::vector< spatial::point_3d_t >{}
                };
            }

            // Create a kernel at the previously calculated centroid (which is
            // the starting point during the first iteration).
            _Kernel kernel {
                current_centroid,
                ground_height,
                crown_diameter_to_tree_height,
                crown_length_to_tree_height,
                crown_diameter_constant,
                crown_length_constant
            };

            // Store the current centroid and calculate a new centroid with the
            // new kernel.
            former_centroid = current_centroid;
            current_centroid = kernel.calculate_centroid_in (
                indexed_point_cloud
            );

            // Append the new centroid to the already calculated ones.
            centroids.push_back( current_centroid );

            num_calculated_centroids++;
        }
        while (
            spatial::distance( former_centroid, current_centroid ) >
                centroid_convergence_distance
            && num_calculated_centroids < max_iterations_per_point
        );

        // Return the terminal centroid plus all prior centroids in an array.
        return std::pair{ current_centroid, centroids };
    }
}
