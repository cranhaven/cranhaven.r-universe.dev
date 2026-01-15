#ifndef __DEPEND_FUNCS__
#define __DEPEND_FUNCS__
#include <RcppArmadillo.h>

arma::vec sample_theta_i_myars(arma::vec x, arma::vec theta0_i, arma::vec y_i,
                               arma::mat inv_sigma, arma::mat A, arma::vec d);
arma::vec sample_theta_i_myars_partial_credit(arma::vec x, arma::vec theta0_i, arma::vec y_i,
                                              arma::mat inv_sigma, arma::mat A, arma::mat D);
  
#endif
