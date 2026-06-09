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
    _Kernel::_Kernel (
        const spatial::point_3d_t &center,
        const double crown_diameter_to_tree_height,
        const double crown_length_to_tree_height,
        const double crown_diameter_constant,
        const double crown_length_constant
    ):
        _xy_center{ spatial::get_x( center ), spatial::get_y( center ) },
        _center_height_initial{ spatial::get_z( center ) },
        _radius{ (crown_diameter_to_tree_height * _center_height_initial
            + crown_diameter_constant
            ) * 0.5
          },
        _height{ (crown_length_to_tree_height * _center_height_initial
            + crown_length_constant
            ) * 0.75
          },

        _half_height        { _height * 0.5 },
        _half_height_squared{ std::pow( _half_height, 2 ) },
        _radius_squared     { std::pow(_radius, 2) },

        // The kernel is positioned vertically asymmetric around center.
        _top_height   { _center_height_initial + _height * 2.0/3.0 },
        _center_height{ _top_height - _height * 0.5 },
        _bottom_height{ _top_height - _height }
    {}


    _Kernel::_Kernel (
        const spatial::point_3d_t &center,
        const spatial::coordinate_t &ground_height_at_center,
        const double crown_diameter_to_tree_height,
        const double crown_length_to_tree_height,
        const double crown_diameter_constant,
        const double crown_length_constant
    ):
        _xy_center{ spatial::get_x( center ), spatial::get_y( center ) },
        _center_height_initial{ spatial::get_z( center ) },
        _radius
        {
            (crown_diameter_to_tree_height
            * (_center_height_initial - ground_height_at_center)
            + crown_diameter_constant
            ) * 0.5
        },
        _height
        {
            (crown_length_to_tree_height
            * (_center_height_initial - ground_height_at_center)
            + crown_length_constant
            ) * 0.75

        },

        _half_height        { _height * 0.5 },
        _half_height_squared{ std::pow( _half_height, 2 ) },
        _radius_squared     { std::pow(_radius, 2) },

        // The kernel is positioned vertically asymmetric around center.
        _top_height   { _center_height_initial + _height * 2.0/3.0 },
        _center_height{ _top_height - _height * 0.5 },
        _bottom_height{ _top_height - _height }
    {}

    std::vector<spatial::point_3d_t> _Kernel::_find_intersecting_points_in (
        const spatial::index_for_3d_points_t &point_cloud
    ) const
    {
        return spatial::get_points_intersecting_vertical_cylinder (
            point_cloud,
            _xy_center, _radius,
            _bottom_height, _top_height
        );
    }

    spatial::distance_t
    _Kernel::_calculate_squared_relative_horizontal_distance_of_center_to (
        const spatial::point_3d_t &point
    ) const
    {
        return (
            std::pow( spatial::get_x( _xy_center ) - spatial::get_x( point ), 2 )
          + std::pow( spatial::get_y( _xy_center ) - spatial::get_y( point ), 2 )
        ) / _radius_squared;
    }

    spatial::distance_t
    _Kernel::_calculate_squared_relative_vertical_distance_of_center_to (
        const spatial::point_3d_t &point
    ) const
    {
        return std::pow( _center_height - spatial::get_z( point ), 2 )
            / _half_height_squared;
    }

    double _Kernel::_calculate_point_weight_of (
        const spatial::point_3d_t &point
    ) const
    {
        return
            _math_functions::gauss_unsquared (
                _calculate_squared_relative_horizontal_distance_of_center_to( point )
            )
          * _math_functions::epanechnikov_unsquared (
                _calculate_squared_relative_vertical_distance_of_center_to( point )
            );
    }

    spatial::point_3d_t _Kernel::calculate_centroid_in (
        const spatial::index_for_3d_points_t &point_cloud
    ) const
    {
        // Find the points intersecting with the kernel.
        std::vector<spatial::point_3d_t> points_in_kernel {
            _find_intersecting_points_in( point_cloud )
        };

        // If there is only one point in the kernel, directly return it as the
        // centroid.
        // (This usually concerns (near-)ground points.)
        if (points_in_kernel.size() == 1) { return points_in_kernel[0]; }

        // Set up an array of point weights.
        std::vector<double> point_weights{};
        point_weights.reserve( points_in_kernel.size() );

        // Calculate point weights.
        for (const auto &point_in_kernel : points_in_kernel)
        {
            point_weights.push_back (
                _calculate_point_weight_of( point_in_kernel )
            );
        }

        // Return the weighted mean point of all points in the kernel.
        return spatial::weighted_mean_of( points_in_kernel, point_weights );
    }
}
