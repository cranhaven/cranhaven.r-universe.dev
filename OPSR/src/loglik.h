#ifndef OPSR_LOGLIK
#define OPSR_LOGLIK

#include <RcppArmadillo.h>

arma::colvec loglik_j(arma::mat& W, arma::mat& X, arma::colvec& y,
                      arma::colvec gamma, double kappa_j_1, double kappa_j,
                      arma::colvec beta_j, double sigma_j, double rho_j,
                      int boundary);

arma::colvec loglik_cpp(Rcpp::NumericVector& theta, arma::field<arma::mat>& W,
                        arma::field<arma::mat>& X, arma::field<arma::colvec>& Y,
                        arma::colvec& weights, int nReg, int nThreads);

#endif
