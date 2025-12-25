#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

//' @noRd
// [[Rcpp::export(rng = FALSE)]]
arma::mat Amat(arma::vec b, arma::mat X, arma::vec W_star, arma::mat H,
               arma::vec I,arma::vec logT, double Q) {
  // arma::mat m1 = X % repmat(I, 1, X.n_cols) % repmat(W_star, 1, X.n_cols);
  // arma::mat m2 =  (-X / repmat(sqrt(diagvec(X * H * X.t())), 1, X.n_cols)) %
  //   repmat(normpdf((X * b - logT) / sqrt(diagvec(X * H * X.t()))), 1, X.n_cols);
  arma::mat m1 = X;
  m1.each_col() %= I % W_star;
  arma::mat tmp = sqrt(sum(X % (X * H), 1));
  // arma::mat m2 = (-X / repmat(tmp, 1, X.n_cols)) % repmat(normpdf((X * b - logT) / tmp), 1, X.n_cols);
  arma::mat m2 = -X;
  m2.each_col() /= tmp ;
  m2.each_col() %= normpdf((X * b - logT) / tmp);
  return m1.t() * m2;
}
