#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;


arma::vec invcdf_gaussian(
    arma::vec const& u,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec mu;
  NumericVector res;

  // compute SD
  double sigma = pow(phi, 0.5);

  // Sample y given U(0,1) using inverse normal CDF
  mu = linkinv_cpp( X * beta, linkname );
  NumericVector unew = wrap(u);
  unew = qnorm( unew, 0.0, 1.0);
  res = mu + sqrt(sigma) * as<arma::vec>(unew);

  return res;
}

arma::vec invcdf_gamma (
    arma::vec const& u,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec mu, b;
  double a;
  mu = linkinv_cpp( X * beta, linkname );

  a = pow(phi, -1);
  b = phi * mu;

  arma::vec res = arma::vec( n, arma::fill::zeros );
  for( int i = 0; i < n; i++ ) {
    res(i) = R::qgamma( u(i), a, b(i), 1, 0 );
  }
  return res;
}



arma::vec invcdf_binomial (
    arma::vec const& u,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec p = linkinv_cpp( X * beta, linkname );
  arma::vec res = arma::vec(n, arma::fill::zeros );
  for ( int i = 0; i < n; i++ ) {
    res(i) = R::qbinom( u(i), 1, p(i), 1, 0 );
  }
  return res;
}



arma::vec invcdf_poisson (
    arma::vec const& u,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec lambda = linkinv_cpp( X * beta, linkname );
  arma::vec res = arma::vec(n, arma::fill::zeros );


  for ( int i = 0; i < n; i++ ) {
    res(i) = R::qpois( u(i), lambda(i), 1, 0 );
  }

  return res;
}





// declare function pointer called invcdfPtr
typedef arma::vec (*invcdfPtr)(
    arma::vec const& u,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
);


// Points to proper function based on string input
XPtr<invcdfPtr> putInvcdfPtrInXPtr( std::string distname ) {
  if ( distname == "gaussian" )
    return XPtr<invcdfPtr>( new invcdfPtr( &invcdf_gaussian ) ) ;

  // else if ( distname == "gamma" || distname == "Gamma" )
  //   return XPtr<invcdfPtr>( new invcdfPtr( &invcdf_gamma ) );
  
  else if ( distname == "binomial" )
    return XPtr<invcdfPtr>( new invcdfPtr( &invcdf_binomial ) );

  else if ( distname == "poisson" )
    return XPtr<invcdfPtr>( new invcdfPtr( &invcdf_poisson ) );
  
  else if ( distname == "Gamma" )
    return XPtr<invcdfPtr>( new invcdfPtr( &invcdf_gamma ) );

  else
    return XPtr<invcdfPtr>(R_NilValue); // runtime error as NULL no XPtr
}




// The below function is main function from this file. It uses the function pointer to correctly
// choose the inverse cdf function

//' Inverse CDF of GLM
//'
//' This function computes the Inverse CDF for each observation y in a GLM
//'
//' @param u a \code{vector} consisting of values between 0 and 1
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi Dispersion parameter. Ignored for binomial and Poisson models
//' @param linkname string giving name of link function. Must be one of \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
//' @param distname name of distribution as a string. Must be one of \code{ c ( "gaussian", "gamma", "poisson", "binomial" ) ) }
//' @param n number of observations
//'
//' @return vector applying CDF to \eqn{ y \mid X, \beta, \phi }
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::vec invcdf_cpp(
    arma::vec const& u,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& distname,
    const std::string& linkname,
    const int& n
) {
  XPtr<invcdfPtr> xpfun = putInvcdfPtrInXPtr( distname );       // point to function corresponding to distname, save function as xpfun
  invcdfPtr fun = *xpfun;                                    // fun now points to proper function based on distname
  arma::vec res = fun( u, X, beta, phi, linkname, n );            // evaluate the log likelihood
  return res;
}





//' Sample GLM copula response variable
//'
//' This function computes the response variable y from a copula GLM given Z, a sample from N(0, Gamma)
//'
//' @param z \code{vector} of \eqn{N(0, 1)} samples from \eqn{Z \sim N(0, \Gamma)}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi Dispersion parameter. Ignored for binomial and Poisson models
//' @param linkname string giving name of link function. Must be one of \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
//' @param distname name of distribution as a string. Must be one of \code{ c ( "gaussian", "gamma", "poisson", "binomial" ) ) }
//' @param n number of observations
//'
//' @return \code{vector} applying inverse CDF
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::vec sample_y (
  arma::vec const& z,
  arma::mat const& X,
  arma::vec const& beta,
  double const& phi,
  std::string const& distname,
  std::string const& linkname,
  int const& n
) {

  // Can obtain y directly from z for gaussian case
  if ( distname == "gaussian" ) {
    arma::vec mu = linkinv_cpp( X * beta, linkname );
    return mu + sqrt(phi) * z;
  }

  // Otherwise; u = pnorm(z) ~ U(0, 1) and then apply inverse CDF of GLM
  NumericVector znew = wrap(z);
  znew = pnorm( znew, 0.0, 1.0 );

  // Return invcdf(u | X, beta, g);
  return invcdf_cpp( as<arma::vec>(znew), X, beta, phi, distname, linkname, n );
}







