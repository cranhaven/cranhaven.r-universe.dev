#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Srivastava2005Stat(arma::mat x) {
  double ncol = x.n_cols;
  double nrow = x.n_rows;
  
  return nrow * ((pow(nrow, 2.0) / (ncol * (nrow - 1.0) * (nrow + 2.0))) *
    (trace(x * x) - pow(trace(x), 2.0) / nrow) - 2.0 * trace(x) / ncol + 1.0) / 2.0;
  
}
