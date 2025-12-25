#include "bayescopulareg.h"
// #include "posGLM.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;



//' Convert to Normal
//' 
//' This function computes \eqn{h_{ij}^{-1}(y_{ij}) = \Phi^{-1}(F_{ij}(y_{ij} | \beta_j, \phi_j))}
//' 
//' @name conv_to_normal
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi Dispersion parameter. Ignored for binomial and Poisson models
//' @param linkname string giving name of link function. Must be one of \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
//' @param distname name of distribution as a string. Must be one of \code{ c ( "gaussian", "Gamma", "poisson", "binomial" ) ) }
//' @param n number of observations
//' 
//' @return vector of standard normal variables
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::vec conv_to_normal(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& distname,
    std::string const& linkname,
    int const& n
) {
  // Apply CDF to y
  arma::vec z = cdf_cpp(y, X, beta, phi, distname, linkname, n);
  NumericVector zvec = wrap(z);
  zvec = qnorm(zvec);
  z = as<arma::vec>(zvec);
  return z;
}





// [[Rcpp::export]]
List condnormal_cpp(
  arma::mat Z,
  arma::mat Gamma,
  int const& j
) {
  
  // Remove jth column from Z
  Z.shed_col(j);
  double gammajj = Gamma(j,j);
  Gamma.shed_col(j);                           // remove jth column from Gamma
  arma::mat GammajJ = Gamma.row(j);                      // GammajJ = Gamma[j,-j]
  Gamma.shed_row(j);                           // Gamma = Gamma[-j,-j]
  
  arma::mat Q = GammajJ * arma::inv_sympd( arma::symmatu( Gamma ) );     // Q = Gamma[j,-j] %*% solve( Gamma[-j, -j] )
  
  arma::mat condvarmat = gammajj - Q * GammajJ.t();
  double condsd = pow(condvarmat(0), 0.5);
  NumericVector condmean = wrap( Z * Q.t() );
  
  List res = List::create(
    _["condmean"] = condmean,
    _["condsd"] = condsd
  );
  
  return res;
}
