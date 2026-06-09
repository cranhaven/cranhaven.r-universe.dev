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


#ifndef SPATIAL_UTIL_H
#define SPATIAL_UTIL_H

#include "spatial_types.h"

namespace spatial
{
    // Internal components are indicated by a leading underscore!

    /** Indicates whether \p point has at least one non-finite coordinate value. */
    inline bool has_non_finite_coordinate_value( const point_3d_t &point )
    {
        // Z values are tested first because they are a tad more likely to be
        // non-finite.
        return
            !std::isfinite( _geom::get<2>( point ) ) ||
            !std::isfinite( _geom::get<0>( point ) ) ||
            !std::isfinite( _geom::get<1>( point ) );
    }

    /** Creates a 3D point with NaN coordinate values. */
    inline point_3d_t nan_point()
    {
        return point_3d_t {
            std::numeric_limits< coordinate_t >::quiet_NaN(),
            std::numeric_limits< coordinate_t >::quiet_NaN(),
            std::numeric_limits< coordinate_t >::quiet_NaN()
        };
    }

    // These functions simply forward some of the boost::geometry functionality.
    template< typename Geometry1, typename Geometry2 >
    inline distance_t distance( const Geometry1 &geom1, const Geometry2 &geom2 )
    {
        return _geom::distance( geom1, geom2 );
    }

    template< typename Geometry >
    inline coordinate_t get_x( const Geometry &geom )
    {
        return _geom::get<0>( geom );
    }

    template< typename Geometry >
    inline coordinate_t get_y( const Geometry &geom )
    {
        return _geom::get<1>( geom );
    }

    template< typename Geometry >
    inline coordinate_t get_z( const Geometry &geom )
    {
        return _geom::get<2>( geom );
    }

    std::vector< point_3d_t > get_points_intersecting_vertical_cylinder (
        const index_for_3d_points_t &point_cloud,
        const point_2d_t &xy_center,
        distance_t radius,
        distance_t bottom_height,
        distance_t top_height
    );

    /** Get the x and y value of a 3D point and return them as a 2D point. */
    inline point_2d_t get_xy_point ( const point_3d_t & point_3d )
    {
        return { _geom::get<0>( point_3d ), _geom::get<1>( point_3d ) };
    }

    /** A functor for querying whether the distance on the x-y-plane between any
     *      point and the point of this functor is smaller than a threshold.
     */
    class _within_xy_distance_functor
    {
    private:
        const point_2d_t _xy_point;
        const distance_t _comparable_distance;

    public:
        _within_xy_distance_functor (
            const point_2d_t &xy_point, const distance_t distance
        ):
            _xy_point{ xy_point },
            _comparable_distance{ _geom::comparable_distance (
                point_2d_t{ 0, 0 }, point_2d_t{ 0, distance }
            ) }
        {}

        bool operator()( const point_3d_t &point ) const
        {
            return _geom::comparable_distance( get_xy_point( point ), _xy_point )
                        <= _comparable_distance;
        }
    };

    /** Calculates the weighted arithmetic mean of a set of points. */
    point_3d_t weighted_mean_of (
        const std::vector< point_3d_t > &points,
        const std::vector< double > &weights
    );
}

#endif  // define SPATIAL_UTIL_H
