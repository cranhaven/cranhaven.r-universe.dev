#include "approxOT_types.h"
#include "hilbert_cgal.h"

//' Returns orders along the Hilbert space-filling Curve
//'
//' @param A a matrix of data-values of class Eigen::MatrixXd
//' @return An integer vector of orders
//' @keywords internal
// [[Rcpp::export]]
Rcpp::IntegerVector hilbert_proj_(const matrix & A)
{
  int K = A.rows();
  int N = A.cols();
  std::vector<int> idx(N);
  
  hilbert_sort_cgal_fun(A.data(), K, N, &idx[0] );
  return(Rcpp::wrap(idx));
}
