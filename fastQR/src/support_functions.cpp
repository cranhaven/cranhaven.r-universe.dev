#include <RcppEigen.h>
#include <numeric>

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Eigen;

Eigen::ArrayXi seq (const int& n) {
  
  std::vector<int> s(n);
  std::iota (std::begin(s), std::end(s), 0); 
  int* ptr = &s[0];
  Eigen::Map<Eigen::ArrayXi> res(ptr, s.size());
  return res;
}

Eigen::ArrayXi setdiff (const Eigen::VectorXi& x1,
                        const Eigen::VectorXi& x2) {
  
  std::vector<int> diff;
  std::set_difference(x1.data(), x1.data() + x1.size(), x2.data(), x2.data() + x2.size(), std::inserter(diff, diff.begin()));
  
  int* ptr = &diff[0];
  Eigen::Map<Eigen::ArrayXi> res(ptr, diff.size());
  return res;
}

Eigen::VectorXd repelem (const double& x,
                         const int& n) {
  
  /* Vectors and matrices declaration       */
  Eigen::VectorXd out(n);
  
  /* replace elements       */
  for (int i = 0; i < n; i++)
    out(i) = x;
  
  // return output
  return out;
}

// end file
