#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

// using namespace arma;
using namespace Rcpp;

//' @title Get the matrices W and H for the ordinal case
//' @description Get the matrices W and H using the Rcpp package for the ordinal case
//' @details
//' getWH_ord uses the current training data and the estimated coefficient under
//' the ordinal case to obtain the matrices W and H to further get the
//' variance-covariance matrix and minimum eigenvalue. Note that using the Rcpp
//' package can significantly reduce the time of operation and get conclusions
//' faster.
//'
//' @param data A matrix containing the training samples which we will use in
//'   the ordinal case.
//' @param beta A matrix contains the estimated coefficient. Note that the
//'   beta_mat is a n * k matrix which n is the number of the explanatory
//'   variables and k+1 is the number of categories
//' @return a list contains several components including the variance-covariance
//'   matrix, minimum eigenvalue, W and H.
//'
//'
// [[Rcpp::export]]
List getWH_ord(arma::mat data, arma::mat beta) {
  arma::uword N = data.n_rows;
  arma::uword p = beta.n_rows, K = beta.n_cols, Kp = K * p;
  arma::mat X = data.cols(1, p);
  arma::mat predictor = X * beta;
  arma::mat accu_tmp = exp(cumsum(predictor, 1));
  arma::mat phi = accu_tmp.each_col() / (1 + sum(accu_tmp, 1));
  arma::mat tmp = exp(predictor);
  arma::mat theta = tmp / (1 + tmp);
  arma::mat gram(p, p), w(K, K, arma::fill::zeros), h(K, K, arma::fill::zeros);
  arma::mat W(Kp, Kp, arma::fill::zeros), H(Kp, Kp, arma::fill::zeros), inv_sigma(Kp, Kp);

  for (unsigned i = 0; i < N; i = i + 1) {
    gram = trans(X.row(i)) * X.row(i);
    w.zeros();
    for (unsigned k = 0; k < K - 1; k = k + 1) {
      w(k, k + 1) = -phi(i, k) * (1 - theta(i, k)) * theta(i, k + 1);
    }
    w = w.t() + w;
    h = diagmat(phi.row(i) % (1 - theta.row(i)));
    w.diag() = h.diag();
    W += kron(w, gram);
    H += kron(h, gram);
  }

  inv_sigma = H * inv_sympd(W) * H;
  arma::vec eigval = eig_sym(inv_sigma);
  return List::create(Rcpp::Named("inv_sigma") = inv_sigma,
                      Rcpp::Named("eigen_min") = eigval[0],
                      Rcpp::Named("W")         = W,
                      Rcpp::Named("H")         = H);
}

