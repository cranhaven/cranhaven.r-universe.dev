#include <math.h>
#include <cmath>
#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.sample_nu_cpp)]]
arma::mat sample_nu_cpp(const Rcpp::NumericVector& x, const Rcpp::List& pars) {
  // Read in the data Y, eta, delta,EV,V
  arma::vec Y = Rcpp::as<arma::vec>(x);
  arma::vec ETA = Rcpp::as<arma::vec>(pars["eta"]);
  arma::vec DELTA = Rcpp::as<arma::vec>(pars["delta"]);
  arma::colvec EV = Rcpp::as<arma::colvec>(pars["EV"]);
  arma::mat V = Rcpp::as<arma::mat>(pars["V"]);
  int N = ETA.size();
  int M = Y.size();
  // now a loop for computing a vector of betas (rates)
    arma::mat S(N,M,arma::fill::zeros); 
    S.randn(N,M);
    arma::mat SAMPLES(N,M);
    arma::vec R(M);
    arma::vec tVy = V.t()*Y;
    arma::rowvec MU(M);
    arma::rowvec s(M);

  arma::mat LAMBDA(M,M,arma::fill::zeros);
  arma::mat LAMBDASQRT(M,M,arma::fill::zeros);
  arma::vec LAMBDA_DIAG(M,arma::fill::zeros);
  for(int i = 0; i < N; ++i) {
    Rcpp::checkUserInterrupt();
	  LAMBDA_DIAG = arma::vec(1/(1+ETA[i]*EV));
    LAMBDA = arma::diagmat(LAMBDA_DIAG);
    LAMBDASQRT = arma::diagmat(sqrt(DELTA[i]*LAMBDA_DIAG));
    s = S.row(i);
    MU = s*LAMBDASQRT;
    R = MU.t()+LAMBDA*tVy;
    SAMPLES.row(i) = trans(V*R);
  }
  return SAMPLES.t();
}
