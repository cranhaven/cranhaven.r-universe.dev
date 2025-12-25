#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;



double logPost_continuous (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat const& Z,
    arma::mat const& Gammainv,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
) {
  
  // Compute log posterior density = logLikelihood + logPrior
  double logPost = loglik_cpp( y, X, beta, phi, distname, linkname, n ) 
  + logInitPrior_cpp ( beta, phi, c0, alpha0, gamma0, p );
  
  // add on power prior if 0 < b0 <= 1
  if ( b0 > 0 && b0 <= 1 ) {
    logPost += logPowerPrior_cpp ( y0, X0, beta, phi, b0, distname, linkname, n0 );
  }
  
  // add on log likelihood for Z's
  arma::mat ZtZ = Z.t() * Z;
  arma::mat GammainvMinusEye = Gammainv - arma::eye(J, J);
  logPost += -0.5 * ( n * log( arma::det( Gammainv ) ) + arma::accu( GammainvMinusEye % ZtZ ) );   // % = element-wise mult
  
  return logPost;
}





double logPost_discrete (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat const& Z,
    arma::mat const& Gammainv,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
) {


  // First, get augmented log likelihood
    // Get conditional mean and variance of Z's for j-th endpoint
    List cond = condnormal_cpp(Z, arma::inv_sympd( Gammainv ), j);
    NumericVector condmean = cond["condmean"];
    double condsd = cond["condsd"];

    // TU / TL are respectively upper and lower values in truncated normal (complete likelihood)
    NumericVector TU = wrap( cdf_cpp( y  , X, beta, phi, distname, linkname, n) );
    NumericVector TL = wrap( cdf_cpp( y-1, X, beta, phi, distname, linkname, n) );

    TU = qnorm(TU);
    TL = qnorm(TL);

    // Z scores for complete data
    TU = (TU - condmean) / condsd;
    TL = (TL - condmean) / condsd;


  // Initialize logPost to be logLik + logInitialPrior
  double logPost = sum( log( pnorm(TU) - pnorm(TL) ) )
                    + logInitPrior_cpp ( beta, phi, c0, alpha0, gamma0, p );

  // add on power prior if 0 < b0 <= 1
  if ( b0 > 0 && b0 <= 1 ) {
    logPost += logPowerPrior_cpp ( y0, X0, beta, phi, b0, distname, linkname, n0 );
  }

  return logPost;
}





// declare function pointer called logPostPtr
typedef double (*logPostPtr)(
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat const& Z,
    arma::mat const& Gammainv,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
);



// Points to proper function based on dist input
XPtr<logPostPtr> putlogPostPtrInXPtr( std::string distname ) {
  // Call continuous if gaussian or gamma
  if ( distname == "gaussian" || distname == "gamma" || distname == "Gamma" )
    return XPtr<logPostPtr>( new logPostPtr( &logPost_continuous ) ) ;
  
  // Call discrete if binomial or poisson
  else if ( distname == "binomial" || distname == "poisson" )
    return XPtr<logPostPtr>( new logPostPtr( &logPost_discrete ) );
  
  else
    return XPtr<logPostPtr>(R_NilValue);
}



//' Log joint posterior density for (beta, phi)
//' 
//' This function returns the log joint posterior density \eqn{(\beta, \phi)}
//' 
//' @param y response \code{vector} for current data
//' @param X design \code{matrix} for current data
//' @param beta \code{vector} of regression coefficients
//' @param phi dispersion parameter
//' @param Z \code{matrix} giving unobserved variables
//' @param Gammainv inverse correlation \code{matrix}
//' @param distname \code{character} giving which distribution to use
//' @param linkname \code{character} giving which link function to use
//' @param n sample size of current data
//' @param j index of which margin to compute likelihood for, \eqn{0 \le j \le J - 1}
//' @param J number of endpoints of current data,
//' @param p number of covariates in current endpoint,
//' @param c0 variance multiple in \eqn{\beta \mid \phi} normal prior
//' @param alpha0 Gamma distribution shape parameter for prior on \code{phi}
//' @param gamma0 Gamma distribution rate parameter for prior on \code{phi}
//' @param b0 power prior parameter for historical data
//' @param y0 \code{vector} of historical responses
//' @param X0 design \code{matrix} for historical data
//' @param n0 historical data sample size
//' 
//' @return scalar giving log posterior density for (beta, phi)
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double logPost (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat const& Z,
    arma::mat const& Gammainv,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& c0,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
) {
  XPtr<logPostPtr> xpfun = putlogPostPtrInXPtr( distname );       // point to function corresponding to distname, save function as xpfun
  logPostPtr fun = *xpfun;                                            // fun now points to proper function based on distname
  double res = fun(
    y, X, beta, phi, Z, Gammainv, distname, linkname,
    n, j, J, p, c0, alpha0, gamma0, b0, y0, X0, n0);
  return res;
}




