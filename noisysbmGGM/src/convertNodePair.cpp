#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec convertNodePair_fast(int i, const arma::vec& j, int p, bool directed = false) {
  if (i > p || any(j > p)) {
    Rcpp::stop("Your index is out of range");
  }

  arma::vec dyads(j.size());
  for (int k = 0; k < j.size(); ++k) {
    if (i == j[k]) {
      dyads[k] = R_NaReal;
    } else {
      dyads[k] = (i < j[k]) ? (0 + ((p - 1) * (p - 1 + 1)) / 2 - ((p - i) * (p - i + 1)) / 2 + (j[k] - i))
        : (0 + ((p - 1) * (p - 1 + 1)) / 2 - ((p - j[k]) * (p - j[k] + 1)) / 2 + (i - j[k]));
    }
  }

  return dyads;
}
