#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat calc_cross(arma::mat cross, arma::mat within) {

  int n=cross.n_rows;
  mat sum(1,1,fill::zeros);
  for(int i=0;i<n;i++){
    sum += (cross.row(i)*within)*cross.row(i).t();
  }
  return(sum);
}

// [[Rcpp::export]]
arma::vec arma_mm(const arma::mat& m, const arma::vec& v) {
  return m * v;
}
