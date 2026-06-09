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


#ifndef SPATIAL_INDEX_CREATION_H
#define SPATIAL_INDEX_CREATION_H

#include "spatial_types.h"
#include "spatial_raster.h"
#include "spatial_util.h"

#include <iterator>  // for std::input_iterator_tag
#include <cstddef>   // for std::ptrdiff_t

namespace spatial
{
    // Leading underscores indicate internal components!

    /** \brief Iterates over a std::vector of 3D points and skips points with
     *      non-finite coordinate values and points below a minimum height.
     *
     *  This iterator is intended for creating R*-trees using a packing
     *  algorithm. The Boost Geometry R*-tree constructor uses such an algorithm
     *  when it is given iterators but not when a for-loop is used to insert
     *  points one-by-one. Constructing R*-trees with a packing algorithm makes
     *  them faster.
     *
     *  Intructions for how to create this iterator class were taken from here:
     *  https://www.internalpointers.com/post/writing-custom-iterators-modern-cpp
     */
    class _Finite_points_above_height_iterator
    {
    public:
        using iterator_category = std::input_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = const point_3d_t;
        using pointer           = value_type*;
        using reference         = value_type&;

    private:
        pointer _ptr;
        pointer _end_ptr;
        coordinate_t _min_height;

        bool _skip_current_point()
        {
            // Don't skip the end pointer.
            if (_ptr == _end_ptr) { return false; }

            // Skip points with non-finite coordinate values.
            if (has_non_finite_coordinate_value( *_ptr )) { return true; }

            // Skip points below the minimum height.
            return _geom::get<2>( *_ptr ) < _min_height;
        }

        _Finite_points_above_height_iterator (
            pointer ptr,
            pointer end_ptr,
            const coordinate_t &min_height
        ):
            _ptr{ ptr }, _end_ptr{ end_ptr }, _min_height{ min_height }
        {
            while (_skip_current_point()) { _ptr++; }
        }

    public:
        // Static functions for creating _Finite_points_iterators
        static _Finite_points_above_height_iterator cbegin (
            const std::vector< point_3d_t > &points,
            const coordinate_t &min_height
        ) {
            return _Finite_points_above_height_iterator {
                &(*points.cbegin()), &(*points.cend()), min_height
            };
        }
        static _Finite_points_above_height_iterator cend (
            const std::vector< point_3d_t > &points,
            const coordinate_t &min_height
        ) {
            return _Finite_points_above_height_iterator {
                &(*points.cend()), &(*points.cend()), min_height
            };
        }

        reference operator*() const { return *_ptr; }
        pointer operator->() { return _ptr; }

        // Prefix increment
        _Finite_points_above_height_iterator& operator++()
        {
            _ptr++;

            while (_skip_current_point()) { _ptr++; }

            return *this;
        }

        // Postfix increment
        _Finite_points_above_height_iterator operator++(int) {
            _Finite_points_above_height_iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        friend bool operator== (
            const _Finite_points_above_height_iterator& a,
            const _Finite_points_above_height_iterator& b
        ) {
            return a._ptr == b._ptr;
        };
        friend bool operator!= (
            const _Finite_points_above_height_iterator& a,
            const _Finite_points_above_height_iterator& b
        ) {
            return a._ptr != b._ptr;
        };
    };

    /** \brief Iterates over a std::vector of 3D points and skips points with
     *      non-finite coordinate values, points at non-finite ground
     *      heights, and points below a certain height above ground.
     *
     *  See documentation of the _Finite_points_above_height_iterator for a
     *      rationale.
     */
    class _Finite_points_above_ground_iterator
    {
    public:
        using iterator_category = std::input_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = const point_3d_t;
        using pointer           = value_type*;
        using reference         = value_type&;

    private:
        pointer _ptr;
        pointer _end_ptr;
        coordinate_t _min_height_above_ground;
        std::shared_ptr< const Raster< coordinate_t > > _ground_height_grid_ptr;

        bool _skip_current_point()
        {
            // Don't skip the end pointer.
            if (_ptr == _end_ptr) { return false; }

            // Skip points with non-finite coordinate values.
            if (has_non_finite_coordinate_value( *_ptr )) { return true; }

            coordinate_t point_height_above_ground {
                // The calculation below reads:
                // "absolute point height minus ground height at point location"
                _geom::get<2>( *_ptr ) -
                _ground_height_grid_ptr->no_throw_value_at_xy_of( *_ptr )
            };

            // Skip points at non-finite ground heights and points below the
            // minimum height.
            return
                !std::isfinite( point_height_above_ground ) ||
                point_height_above_ground < _min_height_above_ground;
        }

        _Finite_points_above_ground_iterator (
            pointer ptr,
            pointer end_ptr,
            const coordinate_t min_height_above_ground,
            const std::shared_ptr< const Raster< coordinate_t > > &ground_height_grid_ptr
        ):
            _ptr{ ptr },
            _end_ptr{ end_ptr },
            _min_height_above_ground{ min_height_above_ground },
            _ground_height_grid_ptr{ ground_height_grid_ptr }
        {
            while (_skip_current_point()) { _ptr++; }
        }

    public:
        // Static functions for creating _Above_ground_points_iterators
        static _Finite_points_above_ground_iterator cbegin (
            const std::vector< point_3d_t > &points,
            const coordinate_t min_height_above_ground,
            const std::shared_ptr< const Raster< coordinate_t > > &ground_height_grid_ptr
        ) {
            return _Finite_points_above_ground_iterator {
                &(*points.cbegin()), &(*points.cend()),
                min_height_above_ground,
                ground_height_grid_ptr
            };
        }
        static _Finite_points_above_ground_iterator cend (
            const std::vector< point_3d_t > &points
        ) {
            return _Finite_points_above_ground_iterator {
                &(*points.cend()), &(*points.cend()),
                0,
                nullptr
            };
        }

        reference operator*() const { return *_ptr; }
        pointer operator->() { return _ptr; }

        // Prefix increment
        _Finite_points_above_ground_iterator& operator++()
        {
            _ptr++;

            while (_skip_current_point()) { _ptr++; }

            return *this;
        }

        // Postfix increment
        _Finite_points_above_ground_iterator operator++(int) {
            _Finite_points_above_ground_iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        friend bool operator== (
            const _Finite_points_above_ground_iterator& a,
            const _Finite_points_above_ground_iterator& b
        ) {
            return a._ptr == b._ptr;
        };
        friend bool operator!= (
            const _Finite_points_above_ground_iterator& a,
            const _Finite_points_above_ground_iterator& b
        ) {
            return a._ptr != b._ptr;
        };
    };

    /** \brief Iterates over a std::vector of 3D points and skips points with
     *      non-finite coordinate values, points at non-finite ground
     *      heights, and points below a certain height above ground.
     *
     *  Same as _Finite_points_above_ground_iterator but uses a grid also for
     *      the minimum height above ground, instead of just for the ground
     *      height.
     */
    class _Finite_points_above_height_grid_iterator
    {
    public:
        using iterator_category = std::input_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = const point_3d_t;
        using pointer           = value_type*;
        using reference         = value_type&;

    private:
        pointer _ptr;
        pointer _end_ptr;
        std::shared_ptr< const I_Raster< coordinate_t > > _min_height_above_ground_grid;
        std::shared_ptr< const I_Raster< coordinate_t > > _ground_height_grid_ptr;

        bool _skip_current_point()
        {
            // Don't skip the end pointer.
            if (_ptr == _end_ptr) { return false; }

            // Skip points with non-finite coordinate values.
            if (has_non_finite_coordinate_value( *_ptr )) { return true; }

            coordinate_t point_height_above_ground {
                // The calculation below reads:
                // "absolute point height minus ground height at point location"
                _geom::get<2>( *_ptr ) -
                _ground_height_grid_ptr->no_throw_value_at_xy_of( *_ptr )
            };

            coordinate_t min_height_above_ground {
                _min_height_above_ground_grid->no_throw_value_at_xy_of( *_ptr )
            };

            // Skip points wherever the ground height or the minimum
            // above-ground height are non-finite and where the points'
            // above-ground height lies below the minimum above-ground height.
            return
                !std::isfinite( point_height_above_ground ) ||
                !std::isfinite( min_height_above_ground ) ||
                point_height_above_ground < min_height_above_ground;
        }

        _Finite_points_above_height_grid_iterator (
            pointer ptr,
            pointer end_ptr,
            const std::shared_ptr< const I_Raster< coordinate_t > > &min_height_above_ground_grid_ptr,
            const std::shared_ptr< const I_Raster< coordinate_t > > &ground_height_grid_ptr
        ):
            _ptr{ ptr },
            _end_ptr{ end_ptr },
            _min_height_above_ground_grid{ min_height_above_ground_grid_ptr },
            _ground_height_grid_ptr{ ground_height_grid_ptr }
        {
            while (_skip_current_point()) { _ptr++; }
        }

    public:
        // Static functions for creating _Above_ground_points_iterators
        static _Finite_points_above_height_grid_iterator cbegin (
            const std::vector< point_3d_t > &points,
            const std::shared_ptr< const I_Raster< coordinate_t > > &min_height_above_ground_grid_ptr,
            const std::shared_ptr< const I_Raster< coordinate_t > > &ground_height_grid_ptr
        ) {
            return _Finite_points_above_height_grid_iterator {
                &(*points.cbegin()), &(*points.cend()),
                min_height_above_ground_grid_ptr,
                ground_height_grid_ptr
            };
        }
        static _Finite_points_above_height_grid_iterator cend (
            const std::vector< point_3d_t > &points
        ) {
            return _Finite_points_above_height_grid_iterator {
                &(*points.cend()), &(*points.cend()),
                0,
                nullptr
            };
        }

        reference operator*() const { return *_ptr; }
        pointer operator->() { return _ptr; }

        // Prefix increment
        _Finite_points_above_height_grid_iterator& operator++()
        {
            _ptr++;

            while (_skip_current_point()) { _ptr++; }

            return *this;
        }

        // Postfix increment
        _Finite_points_above_height_grid_iterator operator++(int) {
            _Finite_points_above_height_grid_iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        friend bool operator== (
            const _Finite_points_above_height_grid_iterator& a,
            const _Finite_points_above_height_grid_iterator& b
        ) {
            return a._ptr == b._ptr;
        };
        friend bool operator!= (
            const _Finite_points_above_height_grid_iterator& a,
            const _Finite_points_above_height_grid_iterator& b
        ) {
            return a._ptr != b._ptr;
        };
    };


    //=================================
    // Public index creation functions
    //=================================

    inline index_for_3d_points_t create_index_of_finite (
        const std::vector< point_3d_t > &points,
        const coordinate_t &min_height
    ) {
        return index_for_3d_points_t {
            _Finite_points_above_height_iterator::cbegin( points, min_height ),
            _Finite_points_above_height_iterator::cend( points, min_height )
        };
    }

    inline index_for_3d_points_t create_index_of_above_ground (
        const std::vector< point_3d_t > &points,
        const coordinate_t min_height_above_ground,
        const std::shared_ptr< Raster< coordinate_t > > &ground_height_grid_ptr
    ) {
        return index_for_3d_points_t {
            _Finite_points_above_ground_iterator::cbegin (
                points,
                min_height_above_ground,
                ground_height_grid_ptr
            ),
            _Finite_points_above_ground_iterator::cend( points )
        };
    }

    inline index_for_3d_points_t create_index_of_above_ground (
        const std::vector< point_3d_t > &points,
        const std::shared_ptr< const I_Raster< coordinate_t > > &min_height_above_ground_grid_ptr,
        const std::shared_ptr< const I_Raster< coordinate_t > > &ground_height_grid_ptr
    ) {
        return index_for_3d_points_t {
            _Finite_points_above_height_grid_iterator::cbegin (
                points,
                min_height_above_ground_grid_ptr,
                ground_height_grid_ptr
            ),
            _Finite_points_above_height_grid_iterator::cend( points )
        };
    }
}

#endif  // define SPATIAL_INDEX_CREATION_H
