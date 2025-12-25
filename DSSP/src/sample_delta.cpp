#include <math.h>
#include <cmath>
#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.sample_delta_cpp)]]
arma::vec sample_delta_cpp(const Rcpp::NumericVector& x, const Rcpp::List& pars) {
  // Read in the data eta,nd,EV,Q,PARS
  // Where V~Ga(0.5*nd,1), then beta/V~IGa(0.5*nd,beta)
  arma::vec ETA = Rcpp::as<arma::vec>(x);
  int N = ETA.size();
  double ND = as<double>(pars["ND"]);
  arma::colvec EV = Rcpp::as<arma::colvec>(pars["EV"]);
  int M = EV.size();
  arma::rowvec Q = Rcpp::as<arma::rowvec>(pars["Q"]);
  arma::vec PARS = Rcpp::as<arma::vec>(pars["PARS"]);
  // now a loop for computing a vector of betas (rates)
  arma::vec Y(N);
  arma::vec V = Rcpp::rgamma(N,0.5*ND+PARS[0],1);
  double BETA;
  arma::mat LAMBDA(M,M,arma::fill::zeros);
  arma::mat B(M,M,arma::fill::zeros);
  for(int i = 0; i < N; ++i) {
    Rcpp::checkUserInterrupt();
    LAMBDA = arma::diagmat(1-1/(1+ETA[i]*EV));
    B = Q*LAMBDA*Q.t();
    BETA = arma::as_scalar(B)*0.5+PARS[1];
    Y[i] = BETA/V[i];
  }
  return Y;
}
