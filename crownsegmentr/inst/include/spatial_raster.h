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


#ifndef SPATIAL_RASTER_H
#define SPATIAL_RASTER_H

#include "spatial_types.h"

namespace spatial
{
    /** An abstract raster interface class.
     *
     *  Instructions on how to create this class were taken from here:
     *  https://www.learncpp.com/cpp-tutorial/pure-virtual-functions-abstract-base-classes-and-interface-classes/
     */
    template< typename T_value >
    class I_Raster
    {
    public:
        virtual const std::vector< T_value >& values() const = 0;

        // Can't return an I_Raster by value because it's an abstract type
        virtual std::unique_ptr< I_Raster< T_value > > copy_w_new_values (
            const std::vector< T_value > &new_values
        ) const = 0;

        virtual bool has_value_at_xy_of( const point_3d_t &point ) const = 0;
        virtual const T_value& value_at_xy_of( const point_3d_t &point ) const = 0;
        virtual const T_value& no_throw_value_at_xy_of( const point_3d_t &point ) const = 0;

        virtual ~I_Raster() {}
    };

    /** Can be used like a raster but actually just returns the same value every time. */
    template< typename T_value >
    class Single_value_pseudo_raster : public I_Raster< T_value >
    {
    private:
        T_value _value;
        std::vector< T_value > _values;

    public:
        Single_value_pseudo_raster( const T_value &value )
        : _value{ value }, _values{ {value} }
        {}


        virtual const std::vector< T_value >& values() const override { return _values; }

        virtual std::unique_ptr< I_Raster< T_value > > copy_w_new_values (
            const std::vector< T_value > &new_values
        ) const override
        {
            if (new_values.size() != 1)
            {
                throw std::runtime_error (
                    "Tried to copy-create a raster with the wrong number of new values."
                );
            }

            return std::make_unique< Single_value_pseudo_raster< T_value > > (
                new_values[0]
            );
        }

        virtual bool has_value_at_xy_of( const point_3d_t &point ) const override
            { return true; }
        virtual const T_value& value_at_xy_of( const point_3d_t &point ) const override
            { return _value; }
        virtual const T_value& no_throw_value_at_xy_of( const point_3d_t &point ) const override
            { return _value; }
    };

    /** Models a rectangular, non-rotated raster. */
    template< typename T_value >
    class Raster : public I_Raster< T_value >
    {
    private:
        /** The raster values go from top left to bottom right, row-wise. */
        const std::vector< T_value > _values;

        const std::size_t _num_rows;
        const std::size_t _num_cols;

        const coordinate_t _x_min;
        const coordinate_t _x_max;
        const coordinate_t _y_min;
        const coordinate_t _y_max;

        const coordinate_t _row_height;
        const coordinate_t _col_width;

    public:
        Raster (
            const std::vector< T_value > &values,
            std::size_t num_rows,
            std::size_t num_cols,
            const coordinate_t &x_min,
            const coordinate_t &x_max,
            const coordinate_t &y_min,
            const coordinate_t &y_max
        ):
            _values{ values },
            _num_rows{ num_rows },
            _num_cols{ num_cols },
            _x_min{ x_min },
            _x_max{ x_max },
            _y_min{ y_min },
            _y_max{ y_max },
            _row_height{ (_y_max - _y_min) / _num_rows },
            _col_width{ (_x_max - _x_min) / _num_cols }
        {}


        virtual const std::vector< T_value >& values() const override
            { return _values; }

        virtual std::unique_ptr< I_Raster< T_value > > copy_w_new_values (
            const std::vector< T_value > &new_values
        ) const override
        {
            if (new_values.size() != _values.size())
            {
                throw std::runtime_error (
                    "Tried to copy-create a raster with the wrong number of new values."
                );
            }

            return std::make_unique< Raster< T_value > > (
                new_values,
                _num_rows, _num_cols,
                _x_min, _x_max,
                _y_min, _y_max
            );
        }

        /** Indicates whether the raster has a value at the xy-coordinates of
         *      \p point.
         */
        virtual bool has_value_at_xy_of( const point_3d_t &point ) const override
        {
            return _x_min <= _geom::get<0>( point ) &&
                             _geom::get<0>( point ) <= _x_max &&
                   _y_min <= _geom::get<1>( point ) &&
                             _geom::get<1>( point ) <= _y_max;
        }

        /** \brief Returns the raster value at the xy-coordinates of \p point.
         *
         *  Throws an exception if x or y are NaN or the location x|y lies
         *      outside of the raster's extent.
         */
        virtual const T_value& value_at_xy_of( const point_3d_t &point ) const override
        {
            if (std::isnan( _geom::get<0>( point ) ) ||
                std::isnan( _geom::get<1>( point ) ))
            {
                throw std::runtime_error (
                    "Tried to access raster value with NaN xy-coordinates."
                );
            }

            if (!this->has_value_at_xy_of( point ))
            {
                throw std::out_of_range (
                    "Tried to access raster value outside of raster extent."
                );
            }

            return no_throw_value_at_xy_of( point );
        }

        /** Same as value_at_xy_of but does not throw exceptions. For NaN
         *      coordinate values and locations outside the raster, the behavior
         *      of this method is undefined.
         */
        virtual const T_value& no_throw_value_at_xy_of( const point_3d_t &point ) const override
        {
            std::size_t row_index { static_cast< std::size_t > (
                (_y_max - _geom::get<1>( point )) / _row_height
            ) };
            // if y == _y_min, the row index is too big by one
            if (row_index == _num_rows) { --row_index; }

            std::size_t col_index { static_cast< std::size_t > (
                (_geom::get<0>( point ) - _x_min) / _col_width
            ) };
            // if x == _x_max, the column index is too big by one
            if (col_index == _num_cols) { --col_index; }

            return _values[_num_cols * row_index + col_index];
        }
    };
}

#endif  // define SPATIAL_RASTER_H
