#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;

//' @keywords internal
arma::vec beta_rwmh (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gammainv,
    arma::mat const& S0beta,
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

  arma::vec res = arma::vec(p+1, arma::fill::zeros);   // first element will store acceptance int
  
  // Proposal is MVT(beta, S0beta);
  // arma::vec betanew = arma::mvnrnd( beta, S0beta );
  arma::vec betanew = rmvt(1, beta, S0beta, 5).t();
  
  // Get log posterior for beta, betanew
  double l0 = logPost ( 
    y, X, beta, phi, Z, Gammainv, distname, linkname,
    n, j, J, p, c0, alpha0, gamma0, b0, y0, X0, n0
  );
  
  double l1 = logPost ( 
    y, X, betanew, phi, Z, Gammainv, distname, linkname,
    n, j, J, p, c0, alpha0, gamma0, b0, y0, X0, n0
  );
  
  
  // acceptance probability
  double logR = l1 - l0;
  double prob = std::min( 1.0, exp(logR) );
  
  // Sample binomial(1, p)
  res(0) = R::rbinom(1, prob);
  
  // return beta if acc = 0 and betanew if acc = 1
  res.subvec(1, p) = res(0) * betanew + (1 - res(0)) * beta;
  
  return res;
}



//' @keywords internal
arma::vec phi_rwmh (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat const& Gammainv,
    double const& sigma0logphi,
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
  arma::vec res = arma::vec(2, arma::fill::ones);   // first element will store acceptance indicator
  
  // return (1,1) if discrete
  if ( distname == "binomial" || distname == "poisson" ) {
    return res;
  }

  // Proposal is N( log(phi), sigma0logphi^2 );
  double phinew = exp( R::rnorm( log(phi) , sigma0logphi) );

  // Get log posterior for phi, phinew
  double l0 = logPost ( 
    y, X, beta, phi, Z, Gammainv, distname, linkname,
    n, j, J, p, c0, alpha0, gamma0, b0, y0, X0,
    n0
  );
  
  double l1 = logPost ( 
    y, X, beta, phinew, Z, Gammainv, distname, linkname,
    n, j, J, p, c0, alpha0, gamma0, b0, y0, X0,
    n0
  );

  // acceptance probability
  double logR = l1 - l0;
  double prob = std::min( 1.0, exp(logR) );

  // Sample binomial(1, p)
  res(0) = R::rbinom(1, prob);

  // return beta if acc = 0 and betanew if acc = 1
  res(1) = res(0) * phinew + (1 - res(0)) * phi;
  return res;
}






//' Update GLM parameters for one endpoint
//' 
//' This function samples the conditionals of beta, phi, and Z
//' 
//' @param y response \code{vector}
//' @param X design \code{matrix}
//' @param beta regression coefficient \code{vector}
//' @param phi dispersion parameter
//' @param Z \code{matrix} of latent variables
//' @param Gammainv inverse correlation \code{matrix}
//' @param c0 scalar giving prior value to scale correlation matrix: \eqn{\beta \sim N(0, c_0 \phi S_0) }
//' @param S0beta covariance matrix to sample betas from random walk
//' @param sigma0logphi standard deviation to sample log(phi) from random walk
//' @param distname name of distribution. See \code{?family}
//' @param linkname name of link function. See \code{?family}
//' @param n number of observations
//' @param j index of which endpoint to update. \eqn{0 \le j < J}
//' @param J number of endpoints
//' @param p number of regressors
//' @param alpha0 hyperparameter for shape parameter of phi assuming gamma density. Ignored if jth endpoint is discrete.
//' @param gamma0 hyperparameter for scale parameter of phi assuming gamma density. Ignored if jth endpoint is discrete.
//' @param b0 \emph{optional} power prior parameter. If \code{b0==0}, assumes no power prior being used.
//' @param y0 \emph{optional} historical response vector. Ignored if \code{b0 == 0}
//' @param X0 \emph{optional} historical design matrix. Ignored if \code{b0 == 0}
//' 
//' @return sampled correlation matrix
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List update_params (
    arma::vec const& y,
    arma::mat const& X,
    arma::vec const& beta,
    double const& phi,
    arma::mat Z,
    arma::mat Gammainv,
    double const& c0,
    arma::mat const& S0beta,
    double const& sigma0logphi,
    std::string const& distname,
    std::string const& linkname,
    int const& n,
    int const& j,
    int const& J,
    int const& p,
    double const& alpha0,
    double const& gamma0,
    double const& b0,
    arma::vec const& y0,
    arma::mat const& X0,
    int const& n0
) {

  // Create empty vector to store whether (beta, phi) proposal was accepted
  arma::vec accept(2, arma::fill::zeros);  // 1 for beta, 1 for phi

  // Sample beta
  arma::vec betaprop = beta_rwmh (
    y, X, beta, phi, Z, Gammainv, S0beta,
    distname, linkname, n, j, J, p, c0,
    alpha0, gamma0, b0, y0, X0, n0
  );
  accept(0) = betaprop(0);     // store whether it was accepted
  betaprop.shed_row(0);        // remove first element from betaprop

  // Sample phi
  arma::vec phiprop = phi_rwmh (
    y, X, betaprop, phi, Z, Gammainv, sigma0logphi,
    distname, linkname, n, j, J, p, c0,
    alpha0, gamma0, b0, y0, X0, n0
  );
  accept(1) = phiprop(0);
  
  
  // Update Z based on new (beta, phi)
  Z = update_Z( y, X, betaprop, phiprop(1), Z, Gammainv, distname, linkname, n, j );

  List res = List::create(
    _["beta"] = betaprop,
    _["phi"] = phiprop(1),
    _["accept"] = accept,
    _["Z"] = Z
  );
  return(res);
}





