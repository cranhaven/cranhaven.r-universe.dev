#include <RcppParallel.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

struct JacIndex : public RcppParallel::Worker {
  // source matrix
  const RcppParallel::RMatrix<double> mat1;
  const RcppParallel::RMatrix<double> mat2;
  RcppParallel::RMatrix<double> result;

  JacIndex(const NumericMatrix mat1, const NumericMatrix mat2, NumericMatrix result)
    : mat1(mat1), mat2(mat2), result(result) {}

  void operator()(std::size_t begin, std::size_t end) {
    for(std::size_t k = begin; k < end; k++) {
      for(std::size_t l = 0; l < mat2.ncol(); l++) {
        double u = 0;
        double i = 0;
        for(std::size_t j = 0; j < mat1.nrow(); j++) {
          if(!std::isnan(mat1(j, k)) && !std::isnan(mat2(j, l))) {
            u += (mat1(j, k) + mat2(j, l) > 0);
            i += (mat1(j, k) + mat2(j, l) > 1);
          }
        }
        if(u > 0) {
          result(k, l) = (u - i) / u;
        } else {
          result(k, l) = NA_REAL;
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix jaccard_index_rcpp_parallel(NumericMatrix mat1, NumericMatrix mat2) {
  if(mat1.nrow() != mat2.nrow()) {
    stop("Number of rows in input matrices do not match.");
  }

  NumericMatrix result(mat1.ncol(), mat2.ncol());

  JacIndex jacIndex(mat1, mat2, result);
  RcppParallel::parallelFor(0, mat1.ncol(), jacIndex);

  return result;
}
