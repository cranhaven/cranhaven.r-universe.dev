#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;

typedef arma::vec (*linkPtr)(const arma::vec& eta);



//' Expit
//' 
//' Inverse logit link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \eqn{e^{eta} / (1 + e^{eta})}
arma::vec linkinv_logit( const arma::vec& eta ) {
  return pow( 1 + exp(-eta) , -1);
}


//' Inverse probit
//' 
//' Inverse probit link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \eqn{\Phi^{-1}(\eta)}
arma::vec linkinv_probit( const arma::vec& eta ) {
  NumericVector eta2 = wrap( eta );
  return pnorm( eta2 );
}

//' Inverse probit
//' 
//' Inverse probit link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \eqn{F^{-1}(eta)} where \eqn{F} is the CDF of a cauchy random variable
arma::vec linkinv_cauchit( const arma::vec& eta ) {
  NumericVector eta2 = wrap(eta);
  NumericVector res = pcauchy( eta2 );
  return as<arma::vec>(res);
}

//' Inverse complentary log-log
//' 
//' Inverse complentary log-log function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \eqn{1 - e^{-e^{eta}}}
arma::vec linkinv_cloglog( const arma::vec& eta ) {
  return 1 - exp(-exp(eta));
}


//' Inverse identity link
//' 
//' Inverse identity link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{eta}
arma::vec linkinv_identity( const arma::vec& eta ) {
  return eta;
}



//' Inverse log link
//' 
//' Inverse log link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \eqn{\exp(eta)}
arma::vec linkinv_log( const arma::vec& eta ){
  return exp(eta);
}


//' Inverse square root link
//' 
//' Inverse square root link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving\eqn{ eta^2 }
arma::vec linkinv_sqrt( const arma::vec& eta ) {
  return arma::pow(eta, 2);
} 


//' Inverse 1/mu^2 link
//' 
//' Inverse link of 1/mu^2 link function
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \code{eta^(-0.5)}
arma::vec linkinv_1mu2 ( const arma::vec& eta ) {
  return arma::pow(eta, -0.5);
}

//' Inverse link function for inverse link
//' 
//' Inverse link function for inverse link
//' 
//' @param eta \code{vector} of linear predictors
//' @return \code{vector} giving \code{eta^(-1)}
arma::vec linkinv_inverse ( const arma::vec& eta ) {
  return arma::pow(eta, -1);
}




// Points to proper function based on string input
XPtr<linkPtr> putLinkPtrInXPtr( std::string linkname ) {
  if ( linkname == "logit" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_logit ) ) ;
  
  else if ( linkname == "probit" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_probit ) ) ;
  
  else if ( linkname == "cauchit" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_cauchit ) );
    
  else if ( linkname == "cloglog" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_cloglog ) );
  
  else if ( linkname == "identity" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_identity ) );\
  
  else if ( linkname == "log" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_log ) );
  
  else if ( linkname == "sqrt" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_sqrt ) );
  
  else if ( linkname == "1/mu^2" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_1mu2 ) );
  
  else if ( linkname == "inverse" )
    return XPtr<linkPtr>( new linkPtr( &linkinv_inverse ) );
  
  else
    return XPtr<linkPtr>(R_NilValue); // runtime error as NULL no XPtr
}


// The below function is main function from this file. It uses the function pointer to correctly
// choose the inverse link function based on string input.

//' Inverse link
//' 
//' This function takes as input a linear predictor eta and the name of a link function,
//' and outputs the inverse link function applied to eta
//' 
//' @param eta \code{vector} of linear predictors
//' @param linkname string giving name of link function. Must be one of
//' \code{ c( "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse" ) }
//' 
//' 
//' @return \code{vector} applying inverse link function to \code{eta}
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::vec linkinv_cpp( arma::vec const& eta, std::string const& linkname ) {
  XPtr<linkPtr> xpfun = putLinkPtrInXPtr( linkname );
  linkPtr linkinv = *xpfun;
  arma::vec y = linkinv(eta);
  return y;
}







