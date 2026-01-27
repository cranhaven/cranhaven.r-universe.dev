#ifndef _HEADER_
#define _HEADER_
#endif

#include <Rcpp.h>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <CGAL/natural_neighbor_coordinates_2.h>
#include <CGAL/Interpolation_traits_2.h>
#include <CGAL/Interpolation_gradient_fitting_traits_2.h>
#include <CGAL/sibson_gradient_fitting.h>
#include <CGAL/interpolation_functions.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef CGAL::Delaunay_triangulation_2<K>                   Delaunay2;
typedef CGAL::Interpolation_traits_2<K>                     Traits;
typedef CGAL::Interpolation_gradient_fitting_traits_2<K>    gradTraits;
typedef K::FT                                               Coord;
typedef K::Point_2                                          Point2;
typedef K::Vector_2                                         Vector2;
typedef K::Vector_3                                         Vector3;
typedef std::map<Point2, Coord, K::Less_xy_2>               Coord_field;
typedef std::map<Point2, Vector2, K::Less_xy_2>             Vector2_field;
typedef std::map<Point2, Vector3, K::Less_xy_2>             Vector3_field;
