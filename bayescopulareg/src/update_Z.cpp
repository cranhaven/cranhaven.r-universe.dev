#include "bayescopulareg.h"
#include <RcppDist.h>
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;



arma::mat update_Z_continuous(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gamma,
    std::string const& distname,
    const std::string& linkname,
    const int& n,
    int const& j
) {
  // apply Phi^{-1}( F( y | beta, phi ) ) to jth column of Z
  Z.col(j) = conv_to_normal( y, X, beta, phi, distname, linkname, n );
  return Z;
}



arma::mat update_Z_discrete(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gamma,
    std::string const& distname,
    const std::string& linkname,
    const int& n,
    int const& j
) {
  // Get conditional mean and variance of Z's for j-th endpoint
  List cond = condnormal_cpp(Z, Gamma, j);
  NumericVector condmean = cond["condmean"];
  double condsd = cond["condsd"];
  
  // TU and TL are respectively upper and lower values in truncated normal (complete likelihood)
  arma::vec TU = conv_to_normal( y, X, beta, phi, distname, linkname, n );
  arma::vec TL = conv_to_normal( y-1, X, beta, phi, distname, linkname, n );
  
  // Generate Z_{ij} ~ TN( condmean, condsd, TL, TU )
  for ( int i = 0; i < n; i++ ) {
    Z(i,j) = r_truncnorm( condmean(i), condsd, TL(i), TU(i) );
  }
  return Z;
}



typedef arma::mat (*zPtr)(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gamma,
    std::string const& distname,
    const std::string& linkname,
    const int& n,
    int const& j
);


// Points to proper function based on string input
XPtr<zPtr> putzPtrInXPtr( std::string distname ) {
  if ( distname == "gaussian" || distname == "gamma" || distname == "Gamma" )
    return XPtr<zPtr>( new zPtr( &update_Z_continuous ) ) ;
  
  else if ( distname == "binomial" || distname == "poisson" )
    return XPtr<zPtr>( new zPtr( &update_Z_discrete ) ) ;
  
  else
    return XPtr<zPtr>(R_NilValue);
}


// The below function is main function from this file. It uses the function pointer to correctly
// choose the cdf function

//' Get updated Z
//' 
//' This function updates the hidden variables in a GLM copula based on \eqn{\beta, \phi}
//' 
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi Dispersion parameter. Ignored for binomial and Poisson models
//' @param Z current \code{matrix} of latent variables
//' @param Gamma current correlation \code{matrix}
//' @param distname name of distribution as a string. Must be one of \code{ c ( "gaussian", "Gamma", "poisson", "binomial" ) ) }
//' @param linkname string giving name of link function. Must be one of \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
//' @param n number of observations
//' @param j index of which column of Z to update, an integer between 0 and J-1
//' 
//' @return vector applying CDF to \eqn{ y \mid X, \beta, \phi }
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::mat update_Z (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gamma,
    std::string const& distname,
    const std::string& linkname,
    const int& n,
    int const& j
) {
  XPtr<zPtr> xpfun = putzPtrInXPtr( distname );                   // point to function corresponding to distname, save function as xpfun
  zPtr fun = *xpfun;                                              // fun now points to proper function based on distname
  arma::mat res = fun( y, X, beta, phi, Z, Gamma, distname, linkname, n, j );
  return res;
}



