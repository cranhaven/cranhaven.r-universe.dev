#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Ahmad2015Stat(arma::mat x) {
  int ncol = x.n_cols;
  int nrow = x.n_rows;
  double c3num = 0;
  double c1num = 0;
  
  for(int i = 0; i < nrow; ++i){
    c1num += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
      x.submat(i, 0, i, ncol - 1).t());
    for(int j = i + 1; j < nrow; ++j){
      c3num += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
        x.submat(j, 0, j, ncol - 1).t() *
        x.submat(i, 0, i, ncol - 1) *
        x.submat(j, 0, j, ncol - 1).t());
    }
  }
  double c3 = c3num * 2.0 / (nrow * (nrow - 1.0));
  
  double c1 = c1num / nrow;

  return nrow * (c3 / ncol - 2.0 * c1 / ncol + 1.0);
}
