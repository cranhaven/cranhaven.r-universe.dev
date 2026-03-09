
#ifndef LASSO_DISTRIBUTION_H
#define LASSO_DISTRIBUTION_H

#include <RcppArmadillo.h>

////////////////////////////////////////////////////////////////////////////////

double expit_c(double x);
//Rcpp::List calculate_lasso_dist_stats_c(double a_val, double b_val, double c_val);
//double logSumExp_c(vec vx);
double zlasso_c_v1(double a, double b, double c, bool logarithm);
double zlasso(double a, double b, double c, bool logarithm);
arma::vec dlasso_c_v1(arma::vec x, double a, double b, double c, bool logarithm);
arma::vec dlasso_internal(arma::vec x, double a, double b, double c, bool logarithm);
Rcpp::NumericVector dlasso(Rcpp::NumericVector x, double a, double b, double c, bool logarithm);
arma::vec plasso_c_v1(arma::vec q, double a, double b, double c);
arma::vec plasso_internal(arma::vec q, double a, double b, double c);
Rcpp::NumericVector plasso(Rcpp::NumericVector q, double a, double b, double c);

arma::vec qlasso_fast_c_v1(arma::vec u, double a, double b, double c);
arma::vec qlasso_internal(arma::vec u, double a, double b, double c);
Rcpp::NumericVector qlasso(Rcpp::NumericVector u, double a, double b, double c);
arma::vec rlasso_fast_c_v1(double n, double a, double b, double c);
arma::vec rlasso_internal(double n, double a, double b, double c);
Rcpp::NumericVector rlasso(double n, double a, double b, double c);
double elasso_c_v1(double a, double b, double c);
double elasso(double a, double b, double c);
double vlasso_c_v1(double a, double b, double c);
double vlasso(double a, double b, double c);
arma::vec mlasso_internal(arma::vec a, arma::vec b, arma::vec c);
Rcpp::NumericVector mlasso(Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector c);

////////////////////////////////////////////////////////////////////////////////

#endif
