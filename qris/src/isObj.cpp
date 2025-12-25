#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

//' @noRd
// [[Rcpp::export(rng = FALSE)]]
arma::mat isObj(arma::vec b, arma::mat X, arma::vec W, arma::mat H,
                arma::vec I, arma::vec logT, double Q) {
  arma::mat m1 = X;
  m1.each_col() %= I;
  arma::mat m2 = normcdf((X * b - logT) / sqrt(sum(X % (X * H), 1))) % W - Q;
  return m1.t() * m2;
}

// old version
// arma::mat isObj(arma::vec b, arma::mat X, arma::vec W, arma::mat H,
// 		arma::vec I, arma::vec logT, double Q) {
//   arma::mat m1 = X % repmat(I, 1, X.n_cols);
//   arma::mat m2 = normcdf((X * b - logT) / sqrt(diagvec(X * H * X.t()))) % W - Q;
//   return m1.t() * m2;
// }

//' @noRd
// [[Rcpp::export(rng = FALSE)]]
arma::mat isObjL(arma::vec b, arma::mat X, arma::mat H, arma::vec logT) {
  // arma::mat m1 = X % repmat(I, 1, X.n_cols);
  // arma::mat m2 = normcdf((X * b - logT) / sqrt(diagvec(X * H * X.t()))) % W - Q;
  arma::mat m2 = normcdf((X * b - logT) / sqrt(sum(X % (X * H), 1)));
  return m2;
}
