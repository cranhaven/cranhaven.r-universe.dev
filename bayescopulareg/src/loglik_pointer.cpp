#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;

// declare function pointer called loglikPtr
typedef double (*loglikPtr)(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& linkname,
    const int& n
);



//' Gaussian log likelihood
//' 
//' This function takes as its input a respone vector,
//' a design matrix, a vector of regression coefficients,
//' a dispersion parameter, the name of a link function,
//' and number of observations, and it outputs the log likelihood of a 
//' Gaussian GLM
//' 
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi dispersion parameter
//' @param linkname name of link function
//' @param n integer giving number of observations
//' @return scalar giving log likelihood
double loglik_gaussian(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& linkname,
    const int& n
) {
  arma::vec eta = X * beta;
  arma::vec mu = linkinv_cpp( eta, linkname );
  arma::vec yc = y - mu;
  return -0.5 * (n * log(phi) + pow(phi, -1) * arma::dot(yc, yc) );
}



//' Gamma log likelihood
//' 
//' This function takes as its input a respone vector,
//' a design matrix, a vector of regression coefficients,
//' a dispersion parameter, the name of a link function,
//' and number of observations, and it outputs the log likelihood of a 
//' Gamma GLM
//' 
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi dispersion parameter
//' @param linkname name of link function
//' @param n integer giving number of observations
//' @return scalar giving log likelihood
double loglik_gamma(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& linkname,
    const int& n
) {
  arma::vec mu, loglik_i, b;
  double a;
  
  // Obtain linear predictor and mean
  mu = linkinv_cpp( X * beta, linkname );         // mu = a * b
  a = pow(phi, -1);
  b = phi * mu;
  
  loglik_i = (a - 1) * log(y) - y / b - a * log(b);
  return arma::accu(loglik_i) - n * lgamma(a);
}




//' Bernoulli log likelihood
//' 
//' This function takes as its input a respone vector,
//' a design matrix, a vector of regression coefficients,
//' a dispersion parameter, the name of a link function,
//' and number of observations, and it outputs the log likelihood of a 
//' Bernoulli GLM
//' 
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi dispersion parameter. Assumed to be 1 (ignored in function)
//' @param linkname name of link function
//' @param n integer giving number of observations
//' @return scalar giving log likelihood
double loglik_binomial(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& linkname,
    const int& n
) {
  arma::vec eta, p, loglik_i;
  eta = X * beta;
  p = linkinv_cpp( eta, linkname );
  
  loglik_i = y % log(p) + ( 1 - y ) % log(1 - p);
  return arma::accu(loglik_i);
}




//' Poisson log likelihood
//' 
//' This function takes as its input a respone vector,
//' a design matrix, a vector of regression coefficients,
//' a dispersion parameter, the name of a link function,
//' and number of observations, and it outputs the log likelihood of a 
//' Poisson GLM
//' 
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi dispersion parameter. Assumed to be 1 (ignored in function)
//' @param linkname name of link function
//' @param n integer giving number of observations
//' @return scalar giving log likelihood
double loglik_poisson(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& linkname,
    const int& n
) {
  arma::vec eta, lambda, loglik_i;
  
  eta      = X * beta;
  lambda   = linkinv_cpp( eta, linkname );
  
  loglik_i = y % log(lambda) - lambda;
  return arma::accu(loglik_i);
}



// Points to proper function based on string input
XPtr<loglikPtr> putLoglikPtrInXPtr( std::string distname ) {
  if ( distname == "gaussian" )
    return XPtr<loglikPtr>( new loglikPtr( &loglik_gaussian ) ) ;
  
  else if ( distname == "gamma" || distname == "Gamma" )
    return XPtr<loglikPtr>( new loglikPtr( &loglik_gamma ) ) ;
  
  else if ( distname == "binomial" )
    return XPtr<loglikPtr>( new loglikPtr( &loglik_binomial ) );
  
  else if ( distname == "poisson" )
    return XPtr<loglikPtr>( new loglikPtr( &loglik_poisson ) );
  
  else
    return XPtr<loglikPtr>(R_NilValue); // runtime error as NULL no XPtr
}


// The below function is main function from this file. It uses the function pointer to correctly
// choose the log likelihood function

//' Log likelihood of GLM
//' 
//' This function computes log likelihood based on data, parameters, link function, and a string giving
//' the proper distribution
//' 
//' @name loglik_cpp
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi Dispersion parameter. Ignored for binomial and Poisson models
//' @param linkname string giving name of link function. Must be one of \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
//' @param n number of observations
//' @param distname name of distribution as a string. Must be one of \code{ c ( "gaussian", "Gamma", "poisson", "binomial" ) ) }
//' 
//' @return scalar giving log likelihood
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double loglik_cpp(
    const arma::vec& y,
    const arma::mat& X,
    const arma::vec& beta,
    const double& phi,
    const std::string& distname,
    const std::string& linkname,
    const int& n

) {
  XPtr<loglikPtr> xpfun = putLoglikPtrInXPtr( distname );       // point to function corresponding to distname, save function as xpfun
  loglikPtr fun = *xpfun;                                    // fun now points to proper function based on distname
  double l = fun( y, X, beta, phi, linkname, n );            // evaluate the log likelihood
  return l;
}




// // [[Rcpp::export]]
// arma::vec invlink_cpp(
//   arma::vec eta,
//   const std::string& linkname
// ) {
//   if ( linkname == "identity" ) {
//     return eta;
//   }
//   else if ( linkname == "log" ) {
//     return arma::exp(eta);
//   }
//   else if ( linkname == "logit" ) {
//     return arma::pow( 1 + arma::exp(-eta), -1.0);
//   }
//   else if ( linkname == "inverse" ) {
//     return arma::pow(eta, -1.0);
//   }
//   else if ( linkname == "probit" ) {
//     for ( unsigned int i = 0; i < eta.size(); i++ ) {
//       eta(i) = R::pnorm( eta(i), 0.0, 1.0, 1.0, 0.0 );    // R::pnorm( q, mean, sd, lower, log )
//     }
//     return eta;
//   }
//   else if ( linkname == "cauchit" ) {
//     for ( unsigned int i = 0; i < eta.size(); i++ ) {
//       eta(i) = R::pcauchy( eta(i), 0.0, 1.0, 1.0, 0.0 );    // R::pnorm( q, mean, sd, lower, log )
//     }
//     return eta;
//   }
//   else if ( linkname == "cloglog" ) {
//     return ( 1 - arma::exp(-1.0 * arma::exp(eta) ) );
//   }
//   else if ( linkname == "sqrt" ) {
//     return arma::square(eta);
//   }
//   else if ( linkname == "1/mu^2" ) {
//     return arma::pow( arma::sqrt(eta), -1.0 );
//   } else {
//     stop("Invalid link function.");
//   }
//   return eta;
// }
// 
// // [[Rcpp::export]]
// double loglik_glm_cpp(
//     const arma::vec& y,
//     const arma::mat& X,
//     const arma::vec& beta,
//     const double& phi,
//     const std::string& distname,
//     const std::string& linkname
// ) {
//   int n         = y.size();
//   double loglik = 0;
//   arma::vec mu = invlink_cpp( X * beta, linkname );
//   if ( distname == "gaussian" ) {
//     for ( int i = 0; i < n; i++ ) {
//       loglik += R::dnorm( y(i), mu(i), std::sqrt(phi), 1.0 );
//     }
//     return loglik;
//   } 
//   else if ( distname == "binomial" ) {
//     for ( int i = 0; i < n; i++ ) {
//       loglik += R::dbinom( y(i), 1, mu(i), 1.0 );
//     }
//     return loglik;
//   } 
//   else if ( distname == "poisson" ) {
//     for ( int i = 0; i < n; i++ ) {
//       loglik += R::dpois( y(i), mu(i), 1.0 );
//     }
//     return loglik;
//   }
//   else if ( distname == "Gamma" ) {
//     double alpha = 1.0 / phi;
//     mu = phi * mu;
//     for ( int i = 0; i < n; i++ ) {
//       loglik += R::dgamma( y(i), alpha, mu(i), 1.0 );
//     }
//     return loglik;
//   } 
//   else {
//     return R_NegInf;
//   }
// }


