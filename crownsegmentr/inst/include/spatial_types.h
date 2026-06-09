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


#ifndef SPATIAL_TYPES_H
#define SPATIAL_TYPES_H

#include <boost/geometry.hpp>

namespace spatial
{
    // Internal components are indicated by a leading underscore!

    /** Shortcut for addressing boost::geometry */
    namespace _geom = boost::geometry;

    using coordinate_t = double; /**< Data type for coordinate values. */
    using distance_t = double;   /**< Data type for distance values. */

    /** Data type for 2D points. */
    using point_2d_t = _geom::model::d2::point_xy< coordinate_t >;
    /** Data type for 3D points. */
    using point_3d_t = _geom::model::d3::point_xyz< coordinate_t >;

    namespace constants
    {
        /** \brief Parameter for the R*-tree spatial index.
         *
         *  This value was chosen based on some charts and examples shown at
         *      https://www.boost.org/doc/libs/1_76_0/libs/geometry/doc/html/geometry/spatial_indexes/introduction.html and
         *      https://www.boost.org/doc/libs/1_76_0/libs/geometry/doc/html/geometry/spatial_indexes/creation_and_modification.html.
         *      Feel free to test whether performance increases with different
         *      values.
         */
        inline constexpr int max_num_elements_per_r_tree_node{ 8 };
    }

    /** Data type for an R*-tree storing 3D points. */
    using index_for_3d_points_t = _geom::index::rtree <
        point_3d_t,
        _geom::index::rstar<constants::max_num_elements_per_r_tree_node>
    >;

    using box_t = _geom::model::box< point_3d_t >;
}

#endif  // define SPATIAL_TYPES_H
