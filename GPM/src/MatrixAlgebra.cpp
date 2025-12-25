#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double Eigen(arma::mat A) {
  arma::vec t = arma::eig_sym(A);
return t[0];
}

// [[Rcpp::export]]
arma::mat CppSolve(arma::mat A, arma::mat B) {
  return arma::solve(A, B, arma::solve_opts::fast);
}

// [[Rcpp::export]]
arma::mat LowerChol(arma::mat A) {
  return arma::chol(A, "lower");
}
