#ifndef __DEPEND_FUNCTION__
#define __DEPEND_FUNCTION__
#include <RcppArmadillo.h>

double obj_func_cpp(arma::mat sigma, arma::mat sigma_hat);
arma::mat calcu_sigma_cmle_cpp(arma::mat theta, double tol = 1e-5);
#endif
