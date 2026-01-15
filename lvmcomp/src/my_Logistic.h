#ifndef __MY_LOGISTIC__
#define __MY_LOGISTIC__
#include <RcppArmadillo.h>

double neg_loglik_logi(arma::mat XX, arma::vec YY, arma::vec beta, double d);
arma::vec neg_loglik_deri(arma::mat XX, arma::vec YY, arma::vec beta, double d);
arma::vec my_Logistic_cpp(arma::mat XX, arma::vec YY, arma::vec beta0, double d0);
arma::vec my_Logistic_cpp_partial(arma::mat XX,  arma::vec YY, arma::vec beta0, arma::vec D0);
#endif

