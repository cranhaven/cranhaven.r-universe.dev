#ifndef OPSR_UTILS
#define OPSR_UTILS

#include <RcppArmadillo.h>

typedef struct theta {
  arma::colvec gamma;
  double kappa_j_1;
  double kappa_j;
  arma::colvec beta_j;
  double sigma_j;
  double rho_j;
} Theta;

Theta make_theta(arma::colvec gamma, double kappa_j_1, double kappa_j,
                 arma::colvec beta_j, double sigma_j, double rho_j);

Theta* make_theta_array(Rcpp::List theta);

void free_theta_array(Theta* theta_array);

Rcpp::List opsr_prepare_coefs(Rcpp::NumericVector& theta, int nReg);
bool opsr_check_omp();
int opsr_max_threads();

#endif
