#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;


//
// Create functions to compute CDF of GLM for each family
//  
arma::vec cdf_gaussian( 
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
  ) {
  arma::vec mu = linkinv_cpp( X * beta, linkname );
  double sigma = pow(phi, 0.5);
  NumericVector z = wrap( ( y - mu ) / sigma );
  
  NumericVector cdf = pnorm( z, 0.0, 1.0 );
  
  return as<arma::vec>(cdf);
}



arma::vec cdf_gamma (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec mu, b, res;
  double a;
  
  
  mu = linkinv_cpp( X * beta, linkname );
  a = pow( phi, -1 );                      // shape param
  b = phi * mu;                           // scale param
  
  res = arma::vec( n, arma::fill::zeros );
  for ( int i = 0; i < n; i++ ) {
    res(i) = R::pgamma( y(i), a, b(i), 1, 0 );
  }
  return res;
}



arma::vec cdf_binomial (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec p = linkinv_cpp( X * beta, linkname );
  arma::vec res = arma::vec(n, arma::fill::zeros );
  for ( int i = 0; i < n; i++ ) {
    res(i) = R::pbinom( y(i), 1, p(i), 1, 0 );
  }
  return res;
}



arma::vec cdf_poisson (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
) {
  arma::vec lambda = linkinv_cpp( X * beta, linkname );
  arma::vec res = arma::vec(n, arma::fill::zeros );
  for ( int i = 0; i < n; i++ ) {
    res(i) = R::ppois( y(i), lambda(i), 1, 0 );
  }
  return res;
}



// declare function pointer called cdfPtr
typedef arma::vec (*cdfPtr)(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& linkname,
    int const& n
);


// Points to proper function based on string input
XPtr<cdfPtr> putCdfPtrInXPtr( std::string distname ) {
  if ( distname == "gaussian" )
    return XPtr<cdfPtr>( new cdfPtr( &cdf_gaussian ) ) ;
  
  else if ( distname == "gamma" || distname == "Gamma" )
    return XPtr<cdfPtr>( new cdfPtr( &cdf_gamma ) ) ;
  
  else if ( distname == "binomial" )
    return XPtr<cdfPtr>( new cdfPtr( &cdf_binomial ) );
  
  else if ( distname == "poisson" )
    return XPtr<cdfPtr>( new cdfPtr( &cdf_poisson ) );
  
  else
    return XPtr<cdfPtr>(R_NilValue); // runtime error as NULL no XPtr
}




// The below function is main function from this file. It uses the function pointer to correctly
// choose the cdf function

//' CDF of GLM
//' 
//' This function computes the CDF for each observation y in a GLM
//' 
//' @name cdf_cpp
//' @param y response \code{vector}
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
arma::vec cdf_cpp(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    std::string const& distname,
    const std::string& linkname,
    const int& n
  
) {
  XPtr<cdfPtr> xpfun = putCdfPtrInXPtr( distname );       // point to function corresponding to distname, save function as xpfun
  cdfPtr fun = *xpfun;                                    // fun now points to proper function based on distname
  arma::vec res = fun( y, X, beta, phi, linkname, n );            // evaluate the log likelihood
  return res;
}




// //' Compute normal scores for each endpoint
// //' 
// //' Get normal scores for each endpoint based on regression parameters
// //' 
// //' @name get_normal_scores
// //' @param Ymat \code{matrix} of responses
// //' @param Xlist list of design matrices
// //' @param betalist list of regression coefficients
// //' @param phivec vector of dispersion parameters
// //' @param linkname string giving name of link function. Must be one of \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
// //' @param distname name of distribution as a string. Must be one of \code{ c ( "gaussian", "gamma", "poisson", "binomial" ) ) }
// //' 
// //' @return matrix giving normal scores
// //' @keywords internal
// //' @noRd
// // [[Rcpp::export]]
// arma::mat get_normal_scores(
//   arma::mat                       Ymat,
//   List                     const& Xlist,
//   List                     const& betalist,
//   arma::vec                const& phivec,
//   std::vector<std::string> const& distnamevec,
//   std::vector<std::string> const& linknamevec 
// ) {
//   int J = Ymat.n_cols;
//   int n = Ymat.n_rows;
//   for ( int j = 0; j < J; j++ ) {
//     arma::mat Xj       = Xlist[j];
//     arma::vec betaj    = betalist[j];
//     arma::vec zj       = cdf_cpp(Ymat.col(j), Xj, betaj, phivec(j), distnamevec[j], linknamevec[j], n);
//     NumericVector zvec = wrap(zj);
//     zvec               = qnorm(zvec);
//     zj                 = as<arma::vec>(zvec);
//     Ymat.col(j)        = zj;
//   }
//   Ymat.replace(arma::datum::inf, 5.0);
//   Ymat.replace(-1.0 * arma::datum::inf, -5.0);
//   return Ymat;
// }





