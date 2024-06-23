#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

// using namespace arma;
using namespace Rcpp;

//' @title Get the most informative subjects from unlabeled dataset for the
//'   ordinal case
//' @description Get the most informative subjects from unlabeled dataset under
//'   the ordinal case
//' @details
//' A_optimal_ord uses the A optimality criterion from the experimental design
//' to choose the most informative subjects under the the ordinal case. We have
//' obtained the variance-covariance matrix based on the current labeled samples
//' which indicates how much information there is. Then we should repeatly
//' calculate the information matrix after we choose a sample from the unlabeled
//' dataset. Once we finish the iteration, we pick the sample which has the most
//' information.
//' @param X A matrix containing all the samples except their labels including
//'   the labeled samples and the unlabeled samples.
//' @param beta A matrix contains the estimated coefficient. Note that the beta
//'   is a n * k matrix which n is the number of the explanatory variables and
//'   k+1 is the number of categories
//' @param W A matrix denotes the inverse information matrix of the coefficient
//'   beta.
//' @param unlabeledIDs A numeric vector for the unique identification of the
//'   unlabeled. dataset.
//' @return a index of the most informative subjects from unlabeled dataset for
//'   the ordinal case
// [[Rcpp::export]]
int A_optimal_ord(arma::mat X, arma::mat beta, arma::mat W, arma::colvec unlabeledIDs) {
  arma::vec::iterator it = unlabeledIDs.begin();
  arma::vec::iterator it_end = unlabeledIDs.end();
  arma::uword i, ind, n=0;
  int p=beta.n_rows, K=beta.n_cols;
  arma::vec trace_val(size(unlabeledIDs), arma::fill::zeros);
  arma::mat x(1, p), tmp(1, K), accu_tmp(1, K), phi(1, K);   // row vec
  arma::mat theta(size(tmp)), w(K, K), gram(p, p);

  for (; it != it_end; ++it) {
    i = (*it) - 1;
    x = X.row(i);
    accu_tmp = exp(cumsum(x * beta, 1));
    phi = accu_tmp.each_col() / (1 + sum(accu_tmp, 1));
    tmp = exp(x * beta);
    theta = tmp/(1 + tmp);
    gram = x.t() * x;
    // h.zeros();
    // h.diag() = phi % (1 - theta);
    w.zeros();
    for (int  k = 0; k < K - 1; k = k + 1) {
      w(k, k + 1) = -phi(k) * (1 - theta(k)) * theta(k + 1);
    }
    w = w.t() + w;
    w.diag() = phi % (1 - theta);
    trace_val(n) = trace(inv_sympd(W + kron(w, gram)));
    ++n;
  }
  // ind = unlabeledIDs(index_max(det_val));
  ind = index_min(trace_val) + 1;
  // cout << "max eigen:" << max(det_val) << std::endl;
  return ind;
}


