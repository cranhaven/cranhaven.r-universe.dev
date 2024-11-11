#ifndef HILBERT_CGAL_H
#define HILBERT_CGAL_H

#include <CGAL/Cartesian_d.h>
#include <CGAL/spatial_sort.h>
#include <CGAL/Spatial_sort_traits_adapter_d.h>
#include <CGAL/boost/iterator/counting_iterator.hpp>
#include <CGAL/hilbert_sort.h>
#include <CGAL/Spatial_sort_traits_adapter_d.h>

void hilbert_sort_cgal_fun(const double * A, int D, int N, int * idx)  ;

#endif //HILBERT_CGAL_H
