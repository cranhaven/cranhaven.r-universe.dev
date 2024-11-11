#ifndef HILBERT_CGAL_H
#define HILBERT_CGAL_H

#include <CGAL/basic.h>
#include <CGAL/Cartesian_d.h>
#include <CGAL/spatial_sort.h>
#include <CGAL/Spatial_sort_traits_adapter_d.h>
#include <CGAL/boost/iterator/counting_iterator.hpp>
#include <CGAL/hilbert_sort.h>
#include <CGAL/Spatial_sort_traits_adapter_d.h>

//' Interfaces from R data types to CGAL for Hilbert sorting
//'
//' @param A a pointer to the data
//' @param D an integer denoting the number of covariates
//' @param N an integer denoting the number of observations
//' @param idx an integer pointer giving the sort index
//' @return void
//' @details Returns the orders along the Hilbert space-filling
//' curve using a median policy. For more info see
//' <https://doc.cgal.org/latest/Spatial_sorting/group__PkgSpatialSortingFunctions.html>
//' @keywords internal
void hilbert_sort_cgal_fun(const double * A, int D, int N, int * idx)  ;

#endif //HILBERT_CGAL_H
