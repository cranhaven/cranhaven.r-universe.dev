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
        const double crown_diameter_to_tree_height,
        const double crown_length_to_tree_height,
        const double crown_diameter_constant,
        const double crown_length_constant,
        const spatial::distance_t &centroid_convergence_distance,
        const int max_iterations_per_point
    ) {
        // If any coordinate value of point is non-finite or point lies below
        // the minimum point height, return NaN.
        if (spatial::has_non_finite_coordinate_value( point ) ||
            spatial::get_z( point ) < min_point_height_above_ground)
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
            // Create a kernel at the current centroid.
            _Kernel kernel {
                current_centroid,
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
        const double crown_diameter_to_tree_height,
        const double crown_length_to_tree_height,
        const double crown_diameter_constant,
        const double crown_length_constant,
        const spatial::distance_t &centroid_convergence_distance,
        const int max_iterations_per_point
    ) {
        // If any coordinate value of point is non-finite or point lies below
        // the minimum point height, return NaN.
        if (spatial::has_non_finite_coordinate_value( point ) ||
            spatial::get_z( point ) < min_point_height_above_ground)
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
            // Create a kernel at the previously calculated centroid (which is
            // the starting point during the first iteration).
            _Kernel kernel {
                current_centroid,
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



// The comments below contain code of the algorithm's old version that was
// commented, formatted and played around with in order to better understand the
// algorithm.


// /** \brief Checks whether a point [PointX, PointY, PointZ] is within a cylinder
//  *  of a given radius and height centered around the point at
//  *  [CtrX, CtrY, CtrZ].
//  */
// bool InCylinder(
//     double PointX, double PointY, double PointZ,
//     double CtrX, double CtrY, double CtrZ,
//     double Radius,
//     double Height
// ){
//     return pow((PointX - CtrX), 2.0) + pow((PointY - CtrY), 2.0) <= pow(Radius, 2.0) &&
//            // i.e. horizontal distance between Point and Center is smaller than radius
//            (PointZ >= (CtrZ - (0.5 * Height))) &&
//            (PointZ <= (CtrZ + (0.5 * Height)));
//
//     if ((pow((PointX - CtrX), 2.0) + pow((PointY - CtrY), 2.0) <= pow(Radius, 2.0)) && (PointZ >= (CtrZ - (0.5*Height))) && (PointZ <= (CtrZ + (0.5*Height))) == true) {
//         return true;
//     } else {
//         return false;
//     }
// }
//
// // Help functions for vertical filter
// double VerticalDistance(double Height, double CtrZ, double PointZ){
//
//   double BottomDistance{ std::abs((CtrZ - Height/4 - PointZ) / (3 * Height/8)) };
//   double TopDistance{ std::abs((CtrZ + Height/2 - PointZ) / (3 * Height/8)) };
//
//   double MinDistance{ std::min(BottomDistance, TopDistance) };
//   return MinDistance;
// }
//
//Equivalent R code
//distx <- function(h, CtrZ, PointZ){
//  bottomdist <- abs((CtrZ-h/4-PointZ)/(3*h/8))
//  topdist <- abs((CtrZ+h/2-PointZ)/(3*h/8))
//  mindist <- pmin(bottomdist, topdist)
//  return(mindist)
//}
//
// // Selects all points in the upper three quarters of the cylinder.
// double VerticalMask(double Height, double CtrZ, double PointZ){
//
//     if(CtrZ - Height/4 <= PointZ && PointZ <= CtrZ + Height/2)
//     {
//         return 1;
//     } else {
//         return 0;
//     }
//
//     if((PointZ >= CtrZ - Height/4) && (PointZ <= CtrZ + Height/2))
//     {
//         return 1;
//     } else {
//         return 0;
//     }
// }
//
//Equivalent R code
//maskx <- function(h, CtrZ, PointZ){
//  maskvec <- ifelse(PointZ >= CtrZ-h/4 & PointZ <= CtrZ+h/2, 1, 0)
//  return(maskvec)
//}
//
// // Epanechnikov function for vertical filter
// double EpanechnikovFunction(double Height, double CtrZ, double PointZ){
//
//     double Result {
//         VerticalMask(Height, CtrZ, PointZ) *
//         (1 - pow(1 - VerticalDistance(Height, CtrZ, PointZ), 2.0))
//     };
//     // i.e. for every point in the upper three quarters of the cylinder do:
//     // 1 - (1 - VerticalDistance(Point))²
//
//     return Result;
// }
//
//Equivalent R code
//Epanechnikov <- function(h, CtrZ, PointZ){
//  output <- maskx(h, CtrZ, PointZ)*(1-(1-distx(h, CtrZ, PointZ))^2)
//  return(output)
//}
//
// Gauss function for horizontal filter
// double GaussFunction(
//     double Width,
//     double CtrX, double CtrY,
//     double PointX, double PointY
// ){
//     double Distance = pow(
//         pow((PointX - CtrX), 2.0) +
//         pow((PointY - CtrY), 2.0),
//         0.5
//     );
//
//     double NormDistance = Distance / Width;
//
//     double Result = std::exp(-5.0 * pow(NormDistance, 2.0));
//     return Result;
// }
//
//Equivalent R code
//gauss <- function(w, CtrX, CtrY, PointX, PointY){
//  distance <- ((PointX-CtrX)^2+(PointY-CtrY)^2)^0.5
//  norm.distance <- distance/w
//  output <- exp(-5*norm.distance^2)
//  return(output)
//}
