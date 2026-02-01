#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec Delta_SBM_2_fast(const arma::vec& Z,
                           const arma::mat& Z_matrix,
                           int iStar,
                           int g,
                           int h,
                           int k,
                           int l,
                           int n0,
                           double eta0,
                           double zeta0,
                           int n_kl,
                           int n_kl_test) {

  int n_k = n0 + arma::accu(Z == k);
  int n_l = n0 + arma::accu(Z == l);

  double m_kl;
  if (k == l)
    m_kl = 0.5 * (n_k - n0) * (n_k - n0 - 1);
  else
    m_kl = (n_k - n0) * (n_l - n0);

  double m_kl_test = m_kl - 1 * (k == g) * (n_l - n0 - (Z(iStar - 1) == l)) + 1 * (l == h) * (n_k - n0 - (Z(iStar - 1) == k));

  double eta_kl = eta0 + n_kl;
  double zeta_kl = zeta0 + m_kl - n_kl;
  double eta_kl_test = eta0 + n_kl_test;
  double zeta_kl_test = zeta0 + m_kl_test - n_kl_test;

  arma::vec output(4);
  output(0) = eta_kl;
  output(1) = zeta_kl;
  output(2) = eta_kl_test;
  output(3) = zeta_kl_test;

  return output;
}
