#include <RcppArmadillo.h>

unsigned int lexLeading0(const arma::umat& M, unsigned int i, const arma::uvec& b) {
  if(M.n_rows == 1 || i >= M.n_cols) {
    return b(0);
  }
  arma::uvec col_i = M.col(i);
  arma::uvec mx = arma::find(col_i == arma::max(col_i));
  return lexLeading0(M.rows(mx), i + 1, b.elem(mx));
}

// [[Rcpp::export]]
unsigned int lexLeadingArma(const arma::umat& M) {
  return lexLeading0(M, 0, arma::regspace<arma::uvec>(1,  M.n_rows));
}

