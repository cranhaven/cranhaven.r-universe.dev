#include "hilbert_cgal.h"


//following tutorials at https://doc.cgal.org/latest/Spatial_sorting/examples.html
//and adapting code from https://github.com/pierrejacob/winference/blob/master/src/HilbertCode.cpp
typedef CGAL::Cartesian_d<double>           Kernel;
typedef Kernel::Point_d                     Point_d;

typedef CGAL::Spatial_sort_traits_adapter_d<Kernel, Point_d*>   Search_traits_d;

void hilbert_sort_cgal_fun(const double * A, int D, int N,  int * idx)
{

  std::vector<Point_d> v;
  double * temp = new double[D];

  for (int n = 0; n < N; n++ ) {
    for (int d = 0; d < D; d ++) {
      temp[d] = A[D * n + d];
    }
    v.push_back(Point_d(D, temp, temp+D));
  }

  std::vector<std::ptrdiff_t> temp_index;
  temp_index.reserve(v.size());

  std::copy(
    boost::counting_iterator<std::ptrdiff_t>(0),
    boost::counting_iterator<std::ptrdiff_t>(v.size()),
    std::back_inserter(temp_index) );

  CGAL::hilbert_sort (temp_index.begin(), temp_index.end(), Search_traits_d( &(v[0]) ) ) ;

  for (int n = 0; n < N; n++) {
    idx[n] = temp_index[n];
  }

  delete [] temp;
  temp=NULL;
}
