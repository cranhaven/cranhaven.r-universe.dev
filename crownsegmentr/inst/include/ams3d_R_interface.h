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


#ifndef AMS3D_R_INTERFACE_H
#define AMS3D_R_INTERFACE_H

#include "spatial_types.h"
#include "spatial_raster.h"

#include <Rcpp.h>

#include <RProgress.h> // This line needs to go after "#include <Rcpp.h>"!
// (RProgress seems to rely on Rinternal.h which is probably loaded implicitly
// by "#include <Rcpp.h>".)

# include <memory> // for std::unique_ptr

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
);

Rcpp::List calculate_centroids_terraneous (
    const Rcpp::DataFrame &coordinate_table,
    const spatial::coordinate_t &min_point_height_above_ground,
    const Rcpp::List &ground_height_grid_data,
    const double crown_diameter_to_tree_height,
    const double crown_length_to_tree_height,
    const double crown_diameter_constant,
    const double crown_length_constant,
    const spatial::distance_t &centroid_convergence_distance,
    const int max_iterations_per_point,
    const bool also_return_all_centroids,
    const bool show_progress_bar
);

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
);

namespace ams3d_R_interface_constants
{
    /** The number of modes which have to be calculated before the progress bar
     *      is ticked and the program checks for a user interrupt from R.
     *      Setting this to a lower value might result in a significant
     *      performance drop in R (possibly because the R console cannot handle
     *      too frequent updates as easily).
     */
    inline constexpr std::size_t num_points_per_tick{ 2000 };
}

namespace ams3d_R_interface_util
{
    std::vector< spatial::point_3d_t > create_point_objects_from (
        const Rcpp::DataFrame &coordinate_table
    );

    RProgress::RProgress create_progress_bar( double total );

    Rcpp::List create_return_data (
        const bool also_return_all_centroids,
        const std::vector< spatial::point_3d_t > &terminal_centroids,
        const std::vector< spatial::point_3d_t > &prior_centroids,
        const std::vector< int > &point_indices
    );

    spatial::Raster< double > convert_list_argument_to_double_raster (
        const Rcpp::List &list
    );

    /** If \p list contains an element named "value", a
     *      Single_value_pseudo_raster< double > is created with the value
     *      stored in that element and returned. Otherwise, a Raster< double >
     *      object is created from raster data stored in \p list.
     */
    std::unique_ptr< spatial::I_Raster< double > >
    convert_list_argument_to_double_raster_ptr (
        const Rcpp::List &list
    );

//     template< typename T >
//     std::unique_ptr< spatial::I_Raster< T > > convert_list_argument_to_raster_ptr (
//         const Rcpp::List &list
//     );
}

#endif  // define AMS3D_R_INTERFACE_H
