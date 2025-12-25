#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;

//' Power prior for \eqn{(\beta, \phi)}
//' 
//' This function returns the log power prior for \eqn{(\beta, \phi)}
//' 
//' @name logPowerPrior_cpp
//' @param y0 \code{vector} of historical responses
//' @param X0 design \code{matrix} for historical data
//' @param beta regression coefficients
//' @param phi dispersion parameter
//' @param b0 power prior parameter, a \code{b0} \eqn{\in (0, 1])}
//' @param distname name of distribution
//' @param linkname name of link function
//' @param n0 historical data sample size
//' 
//' @return scalar giving log prior density
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double logPowerPrior_cpp (
  arma::vec const& y0,
  arma::mat const& X0,
  arma::vec const& beta,
  double const& phi,
  double const& b0,
  std::string const& distname,
  std::string const& linkname,
  int const& n0
) {
  return b0 * loglik_cpp( y0, X0, beta, phi, distname, linkname, n0 );
}


// //' Uniformative conditional prior for beta | phi
// //'
// //' This function returns a log Gaussian prior density on beta with variance phi * c0
// //' where c0 is a hyperparameter.
// //'
// //' @name logCondIndBetaPrior_cpp
// //' @param beta \code{vector} of regression coefficients
// //' @param phi dispersion parameter
// //' @param c0 prior variance multiple
// //'
// //' @return scalar giving log prior density of \code{beta}
// //' @export
// // [[Rcpp::export]]
// double logCondIndBetaPrior_cpp (
//     arma::vec const& beta,
//     double const& phi,
//     double const& c0
// ) {
//   return -0.5 * phi * c0 * arma::dot(beta, beta) ;
// }
// 
// 
// //' Uniformative prior for phi
// //'
// //' This function returns the portion of (beta | phi) that depends on phi and a Gamma
// //' marginal prior for phi
// //'
// //' @name logPhiPrior_cpp
// //' @param beta \code{vector} of regression coefficients
// //' @param phi dispersion parameter
// //' @param c0 prior variance multiple for \code{beta}
// //' @param a0 shape parameter for \code{phi}
// //' @param b0 rate parameter for \code{phi}
// //'
// //' @return scalar giving log prior density of \code{\phi}
// //' @export
// // [[Rcpp::export]]
// double logPhiPrior_cpp (
//     arma::vec const& beta,
//     double const& phi,
//     double const& c0,
//     double const& a0,
//     double const& b0
// ) {
//   return logCondIndBetaPrior_cpp(beta, phi, c0) + (a0 - 1) * log(phi) - b0 * phi;
// }


//' Initial validation prior for (beta, phi)
//' 
//' This function returns the log prior density for (beta, phi) where
//' \eqn{\beta \mid \phi \sim N_p(0, \phi c_0 I)} and
//' \eqn{\phi \sim } Gamma \eqn{ ( \alpha_0, \gamma_0 ) }
//' 
//' @name logInitPrior_cpp
//' @param beta \code{vector} of regression coefficients
//' @param phi dispersion parameter
//' @param c0 prior variance multiple for regression coefficients given inverse dispersion
//' @param alpha0 shape parameter for inverse dispersion
//' @param gamma0 rate parameter for inverse dispersion
//' @param p dimension of \code{beta}
//' @return scalar giving log prior joint density
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double logInitPrior_cpp ( 
  arma::vec const& beta,
  double const& phi,
  double const& c0,
  double const& alpha0,
  double const& gamma0,
  int const& p
) {
  double tau = pow(phi, -1);
  return ( (0.5 * p + alpha0 - 1) * log(tau) - 0.5 * (tau / c0) * arma::dot(beta, beta) - gamma0 * tau );
}


