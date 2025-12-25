#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

//' @noRd
// [[Rcpp::export(rng = FALSE)]]
arma::mat rev_isObj(arma::vec b, arma::mat X, arma::vec W, arma::mat H,
		    arma::vec E, arma::vec I,arma::vec logT, double Q) {
  arma::mat m1 = X;
  m1.each_col() %= I % E;  
  // H is a symmetric
  arma::mat m2 = normcdf((X * b - logT) / sqrt(sum(X % (X * H), 1))) % W - Q;
  return m1.t() * m2 ;
}

// old version
// arma::mat rev_isObj(arma::vec b, arma::mat X, arma::vec W, arma::mat H,
// 		    arma::vec E, arma::vec I,arma::vec logT, double Q) {
//   arma::mat m1 = X % repmat(I % E, 1, X.n_cols);
//   arma::mat m2 = normcdf((X * b - logT) / sqrt(diagvec(X * H * X.t()))) % W - Q;
//   return m1.t() * m2 ;
// }
