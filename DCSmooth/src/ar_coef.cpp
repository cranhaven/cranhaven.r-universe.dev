// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include "ar_coef.h"
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec ar_coef(const arma::vec ar,
                   const arma::vec ma,
                   const double d,
                   const int k)
{
  int p = ar.n_elem;
  int q = ma.n_elem;
  arma::vec phi = join_cols(arma::ones(1), -ar, arma::zeros(k - p));
  arma::vec theta = join_cols(arma::ones(1), arma::zeros(k));
  for (int i = 1; i <= k; i++)
  {
    if ((i - q) < 0)
    {
      theta(i) = -arma::sum(ma.subvec(0, (q - std::abs(i - q) - 1)) % reverse(theta.subvec(0, (i - 1)))) - (-phi(i));
    } else {
      theta(i) = -arma::sum(ma.subvec(0, (q - 1)) % reverse(theta.subvec((i - q), (i - 1)))) - (-phi(i));
    }
  }

  if (d > 0)
  {
    arma::vec dcoef(k + 1);
    for (int j = 0; j <= k; j++)
    {
      dcoef(j) = std::pow(-1, j) * std::tgamma(d + 1) /
                 (std::tgamma(j + 1) * std::tgamma(d - j + 1));
    }

    arma::vec coef_all(k + 1);
    for (int l = 0; l <= k; l++)
    {
      coef_all(l) = arma::sum(dcoef.subvec(0, l) %
                              arma::reverse(theta.subvec(0, l)));
    }
    
    return coef_all;
  } else {
    return theta;
  }
}
