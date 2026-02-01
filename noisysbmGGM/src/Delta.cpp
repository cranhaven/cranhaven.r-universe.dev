#include <RcppArmadillo.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List Delta_fast(const arma::vec& Avec,
                      const arma::mat& A_test,
                      const arma::mat& Z_matrix,
                      int iStar,
                      int g,
                      int h,
                      int k,
                      int l,
                      const arma::uvec& ind_iStar,
                      int n_kl) {


  arma::mat Z1=Z_matrix;
  Z1.shed_col(iStar-1);


  arma::mat l_remove = Avec(ind_iStar).t() % Z1.row(l - 1);
  double delta_l_remove = arma::accu(l_remove);


  arma::mat Atest=A_test;
  Atest.shed_col(iStar - 1);
  arma::mat k_add = Atest % Z1.row(k - 1);

  double delta_k_add = arma::accu(k_add);

  double delta_kl = -1 * (k == g) * delta_l_remove + (l == h) * delta_k_add;

  int n_kl_test = n_kl + delta_kl;

  return Rcpp::List::create(Rcpp::Named("delta_kl") = delta_kl,
                            Rcpp::Named("n_kl_test") = n_kl_test,
                            Rcpp::Named("k_add") = k_add,
                            Rcpp::Named("l_remove") = l_remove);
}
