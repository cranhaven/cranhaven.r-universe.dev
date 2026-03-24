#include <RcppParallel.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// This function compute uper triangle Jaccard distance matrix
// Worker for upper triangle Jaccard distance
struct JacIndexUpper : public RcppParallel::Worker {
  const RcppParallel::RMatrix<double> mat;
  RcppParallel::RMatrix<double> result;
  std::size_t ncol;

  JacIndexUpper(const NumericMatrix mat, NumericMatrix result)
    : mat(mat), result(result), ncol(mat.ncol()) {}

  void operator()(std::size_t index_begin, std::size_t index_end) {
    for (std::size_t idx = index_begin; idx < index_end; ++idx) {
      std::size_t i = idx / ncol;
      std::size_t j = idx % ncol;
      if (i < j) {
        double u = 0;
        double intersect = 0;
        for (std::size_t r = 0; r < mat.nrow(); ++r) {
          if (!std::isnan(mat(r, i)) && !std::isnan(mat(r, j))) {
            u += (mat(r, i) + mat(r, j) > 0);
            intersect += (mat(r, i) + mat(r, j) > 1);
          }
        }
        if (u > 0) {
          result(i, j) = (u - intersect) / u;
        } else {
          result(i, j) = NA_REAL;
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix jaccard_index_rcpp_upper(NumericMatrix mat) {
  std::size_t p = mat.ncol();
  NumericMatrix result(p, p);

  std::size_t total = p * p;
  JacIndexUpper worker(mat, result);

  RcppParallel::parallelFor(0, total, worker);

  return result;
}
