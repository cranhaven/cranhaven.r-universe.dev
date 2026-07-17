#ifndef AHULL3D_TYPES_H
#define AHULL3D_TYPES_H

// Use standard CGAL headers (provided by RcppCGAL)
#include <Rcpp.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_3.h>
#include <CGAL/Fixed_alpha_shape_vertex_base_3.h>
#include <CGAL/Fixed_alpha_shape_cell_base_3.h>
#include <CGAL/Fixed_alpha_shape_3.h>
#include <CGAL/Tetrahedron_3.h>

#include <unordered_map>
#include <list>

// Typedefs
typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_3                                          Point3;
typedef CGAL::Fixed_alpha_shape_vertex_base_3<K>            Vb;
typedef CGAL::Fixed_alpha_shape_cell_base_3<K>              Cb;
typedef CGAL::Triangulation_data_structure_3<Vb, Cb>        Tds;
typedef CGAL::Delaunay_triangulation_3<K, Tds>              Delaunay;
typedef CGAL::Fixed_alpha_shape_3<Delaunay>                 Fixed_alpha_shape_3;
typedef Fixed_alpha_shape_3::Facet                          Facet;
typedef Fixed_alpha_shape_3::Cell_handle                    Cell_handle;
typedef K::Tetrahedron_3                                    Tetrahedron;

// Function declaration
Rcpp::List FAS_cpp_with_labels(Rcpp::NumericMatrix pts, double alpha, 
                               Rcpp::NumericVector input_labels, bool volume);

#endif // AHULL3D_TYPES_H
