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


#include "spatial_util.h"

#include <iterator> // for std::back_inserter
#include <numeric>  // for std::accumulate

namespace spatial
{
    /** \note When modifying this function keep in mind that there might be a
     *  significant impact on performance since the function is called very
     *  often while calculating centroids.
     */
    std::vector< point_3d_t > get_points_intersecting_vertical_cylinder (
        const index_for_3d_points_t &point_cloud,
        const point_2d_t &xy_center,
        distance_t radius,
        distance_t bottom_height,
        distance_t top_height
    ) {

        // Construct a box around the cylinder.
        point_3d_t min_corner_point {
            _geom::get<0>( xy_center ) - radius,
            _geom::get<1>( xy_center ) - radius,
            bottom_height
        };
        point_3d_t max_corner_point {
            _geom::get<0>( xy_center ) + radius,
            _geom::get<1>( xy_center ) + radius,
            top_height
        };

        box_t cylinder_box{ min_corner_point, max_corner_point };

        // Set up a vector for storing the points inside the cylinder.
        std::vector< point_3d_t > intersecting_points{};
        // This is a "magic number" guessed from experience
        // TODO make more meaningful estimates based on the point cloud density
        // Keep in mind that the default allocation strategy of std::vectors is
        // likely quadratic. I.e. if you reserve a capacity of e.g. 1000 at the
        // beginning, the vector will allocate twice as much (2000) when it is
        // asked to insert the 1001th element. When it is asked to insert the
        // 2001th element it will allocate capacity for 4000 elements and so on.
        intersecting_points.reserve( 1000 );

        // Set up a unary predicate object.
        _within_xy_distance_functor within_xy_distance {
            xy_center, radius
        };

        // Query the point cloud.
        point_cloud.query (
                   _geom::index::intersects( cylinder_box )
                && _geom::index::satisfies( within_xy_distance ),
            std::back_inserter( intersecting_points )
        );

        return intersecting_points;
    }

    point_3d_t weighted_mean_of (
        const std::vector< point_3d_t > &points,
        const std::vector< double > &weights
    ) {
        // Set up a mean point
        point_3d_t mean_point{ 0, 0 ,0 };

        // Weigh all points and add them together
        for(std::size_t i{ 0 }; i < points.size(); i++)
        {
            _geom::add_point (
                mean_point,
                point_3d_t {
                    _geom::get<0>( points[i] ) * weights[i],
                    _geom::get<1>( points[i] ) * weights[i],
                    _geom::get<2>( points[i] ) * weights[i]
                }
            );
        }

        // Divide the sum of all weighted points by the sum of the weights
        _geom::divide_value(
            mean_point,
            std::accumulate( weights.begin(), weights.end(), double{ 0 } )
        );

        return mean_point;
    }
}
