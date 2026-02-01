#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
arma::vec Delta_orange_5_fast(const arma::vec& dataVec,
                              const arma::mat& Z_matrix,
                              int iStar,
                              int g,
                              int h,
                              int k,
                              int l,
                              const arma::vec& I_kl,
                              int n_kl,
                              int n_kl_test,
                              const arma::vec& ind_iStar,
                              const arma::vec& k_add,
                              const arma::vec& l_remove) {
  // -- E_Ikl

  int N=I_kl.n_elem;
  double sum_E_X = 0.0;
  double sum_E_X2 = 0.0;

  for (int i = 0; i < N; i++) {
    if (I_kl(i) != 0) {
      sum_E_X += I_kl(i) * dataVec(i);
      sum_E_X2 += I_kl(i) * dataVec(i) * dataVec(i);
    }
  }
  double E_X = (n_kl == 0) ? 0 : sum_E_X / n_kl;
  double E_X2 = (n_kl == 0) ? 0 : sum_E_X2 / n_kl;

  // -- E_Ikl_test
  double sum_psi_X = 0.0;
  double sum_psi_X2 = 0.0;
  int K=ind_iStar.n_elem;
  for (int j = 0; j < K; j++){
    if (k == g) {
      sum_psi_X -= l_remove(j) * dataVec(ind_iStar[j]-1);
      sum_psi_X2 -= l_remove(j) * dataVec(ind_iStar[j]-1) * dataVec(ind_iStar[j]-1);
    }
    if (l == h) {
      sum_psi_X += k_add(j) * dataVec(ind_iStar[j]-1);
      sum_psi_X2 += k_add(j) * dataVec(ind_iStar[j]-1) * dataVec(ind_iStar[j]-1);
    }
  }
  double E_X2_test = (n_kl_test == 0) ? 0 : (n_kl * E_X2 + sum_psi_X2) / n_kl_test;
  double E_X_test = (n_kl_test == 0) ? 0 : (n_kl * E_X + sum_psi_X) / n_kl_test;

  arma::vec result(8);
  result(0) = E_X;
  result(1) = E_X2;
  result(2) = E_X_test;
  result(3) = E_X2_test;
  result(4) = sum_E_X;
  result(5) = sum_E_X2;
  result(6) = sum_psi_X;
  result(7) = sum_psi_X2;

  return result;
}
