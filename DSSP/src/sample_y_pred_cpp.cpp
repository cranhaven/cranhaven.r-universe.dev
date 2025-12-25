#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.sample_y_pred_cpp)]]
arma::mat sample_y_pred_cpp(const Rcpp::List& pars) {
  int n = as<int>(pars["n"]);
  int N = as<int>(pars["N"]);
  int m = as<int>(pars["m"]);
  arma::colvec eta = Rcpp::as<arma::colvec>(pars["eta"]);
  arma::colvec delta = Rcpp::as<arma::colvec>(pars["delta"]);
  arma::colvec ev = Rcpp::as<arma::colvec>(pars["ev"]);
  arma::colvec Y = Rcpp::as<arma::colvec>(pars["Y"]);
  arma::mat v = Rcpp::as<arma::mat>(pars["v"]);
  arma::mat nu = Rcpp::as<arma::mat>(pars["nu"]);
  arma::mat VT = v.t();
  arma::mat samples = arma::randn(N, m);
  arma::mat S = arma::mat(ev.size(), ev.size(), arma::fill::ones);
  arma::mat S_inv = arma::mat(ev.size(), ev.size(), arma::fill::ones);
  arma::mat MU = arma::mat(ev.size(), 1, arma::fill::ones);
  arma::mat SIGMA = arma::mat(ev.size(), ev.size(), arma::fill::ones);
  arma::mat MU1 = arma::mat(n, 1, arma::fill::ones);
  arma::mat MU2 = arma::mat(m, 1, arma::fill::ones);
  arma::mat S11 = arma::mat(n, n, arma::fill::ones);
  arma::mat S12 = arma::mat(n, m, arma::fill::ones);
  arma::mat S21 = arma::mat(m, n, arma::fill::ones);
  arma::mat S22 = arma::mat(m, m, arma::fill::ones);
  arma::mat RES = arma::mat(n, 1, arma::fill::ones);
  arma::mat M_id = arma::mat(m,m,arma::fill::ones);
  arma::mat MU_pred = arma::mat(m, 1, arma::fill::ones);
  arma::mat S_pred = arma::mat(m, m, arma::fill::ones);

  for(int i=0; i<N; ++i) {
    Rcpp::checkUserInterrupt();
    // declare all matrices outside the loop at the right dimensions (try one by one)
    // and then update - same as LAMBDA in sample_delta.cpp
    // Compute S
    S = v*arma::diagmat(1/(1+eta[i]*ev))*VT;
    S_inv = v*arma::diagmat(1+eta[i]*ev)*VT;
    
    // Compute MU
    MU = S*Y;

    // Compute Sigma
    SIGMA = delta[i]*S ;

    // Partition MU and SIGMA
    MU1 = MU.rows(0, n-1);
    MU2 = MU.rows(n, n+m-1);

    S11 = delta[i]*S_inv.submat(0,0,n-1,n-1);
    S12 = SIGMA.submat(0,n,n-1,n+m-1);
    S21 = SIGMA.submat(n,0,n+m-1,n-1);
    S22 = SIGMA.submat(n,n,n+m-1,n+m-1);

    // Compute Residuals
    RES = nu.submat(0, i, n-1, i) - MU1;

    // Compute mu_pred and sigma_pred for y_pred
    MU_pred =  MU2+S21*S11*RES;
    S_pred = delta[i]*M_id+S22+S21*S11*S12;

    samples.row(i) = MU_pred.t() + samples.row(i) * arma::chol(S_pred);
  }
  
  return samples.t();
}
