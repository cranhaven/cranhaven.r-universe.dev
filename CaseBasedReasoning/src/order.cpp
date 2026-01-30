// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

#if RCPP_PARALLEL_USE_TBB

struct parallelOrderMatrix : public Worker {
  const arma::mat& x_;
  const int sortDirection_;
  const std::size_t k_;
  arma::umat& output_;
  parallelOrderMatrix(
    const arma::mat& x,
    const int sortDirection,
    const std::size_t k,
    arma::umat& output
  ) : x_(x), sortDirection_(sortDirection), k_(k), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    arma::uvec order(x_.n_rows);
    for (std::size_t i=begin;i<end;++i) {
      order = arma::sort_index(x_.col(i), "ascend");
      for (std::size_t l=0;l<k_;++l) {
        output_(l, i) = order(l) + 1;
      }
    }
  }
};

arma::umat orderMatrix(arma::mat& x, const int sortDirection, const int k) {
  int nCols = x.n_cols;
  int nRows = k;
  if (k == 0) {
    nRows = x.n_rows;
  } 
  arma::umat output(nRows, nCols);
  output.fill(0);
  parallelOrderMatrix parallelOrderMatrix(x, sortDirection, nRows, output);
  parallelFor(0, nCols, parallelOrderMatrix);
  return output;
}

#else

arma::umat orderMatrix(arma::mat& x, const int sortDirection, const int k) {
  int nCols = x.n_cols;
  int nRows = k;
  if (k == 0) {
    nRows = x.n_rows;
  } 
  arma::umat output(nRows, nCols);
  arma::uvec order(x.n_rows);
  for (std::size_t i; i<nCols; ++i) {
    order = arma::sort_index(x.col(i), sortDirection);
    for (std::size_t l=0;l<nRows;++l) {
      output(l, i) = order(l) + 1;
    }
  }
  return output;
}

#endif

// [[Rcpp::export]]
arma::umat cpp_orderMatrix(arma::mat& x, const int sortDirection, int k = 5) {
  return orderMatrix(x, sortDirection, k);
}

// [[Rcpp::export]]
arma::uvec cpp_orderVector(arma::vec x, const int sortDirection, int k = 0) {
  arma::uvec order = arma::sort_index(x, "ascend") + 1;
  if (k > 0)
    order.resize(k);
  return order;
}
