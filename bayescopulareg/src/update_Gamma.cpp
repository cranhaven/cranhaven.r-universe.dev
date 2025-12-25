#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;



//' Update correlation matrix
//' 
//' This function samples from the posterior correlation matrix given
//' the latent variables given as a matrix Z
//' 
//' @param Z a \eqn{n \times J} \code{matrix} of latent variables
//' @param n number of rows in Z
//' @param v0 hyperparameter giving degrees of freedom for inverse Wishart sample
//' @param v0V0 v0 * hyperparameter giving scale matrix for inverse Wishart sample
//' 
//' @return sampled correlation matrix
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::mat update_Gamma (
  arma::mat const& Z,
  int const& n,
  int const& v0,
  arma::mat const& v0V0
) {
  arma::mat V = riwish( v0 + n, v0V0 + Z.t() * Z );
  arma::vec d = arma::pow( V.diag(), -0.5) ;
  
  V.each_col() %= d;       // % = element-wise multiplication
  V.each_row() %= d.t();
  V.diag().ones();
  
  return V;
}









