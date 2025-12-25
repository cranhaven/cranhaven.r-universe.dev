#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;



//' MCMC sample of copula GLM
//' 
//' This function samples from the posterior distribution of a copula GLM
//' 
//' @param ymat \eqn{n \times J} \code{matrix} of response variables
//' @param Xlist \eqn{J}-dimensional list of design matrices
//' @param distnamevec \code{character} vector of length \eqn{J}  giving name of distribution
//' @param linknamevec \code{character} vector of length \eqn{J}  giving name of link function. See \code{help(family)}
//' @param c0vec \code{numeric} vector giving scale hyperparmeter for conditional prior. The prior covariance for beta is c0 * phi * I.
//' @param sigma0logphivec \code{vector} giving random walk variance for log dispersion parameter
//' @param alpha0vec \code{numeric} vector giving shape parameter for the gamma density prior on phi
//' @param gamma0vec \code{numeric} vector giving rate parameter for the gamma density prior on phi
//' @param Gamma \code{matrix} giving starting values for correlation parameters
//' @param v0 integer giving degrees of freedom for Inverse wishart prior on correlation matrix \code{Gamma}
//' @param V0 \code{matrix} giving prior scale parameter for Inverse wishart prior on correlation matrix \code{Gamma}
//' @param b0 scalar between 0 and 1 giving power prior hyperparameter. If \code{b0 == 0}, historical data is ignored.
//' @param y0mat \code{matrix} giving responses for historical data set
//' @param X0list \code{list} giving design matrices for historical data responses
//' @param M number of samples to draw
//' @param beta0list \code{list} of vectors giving starting values for regression coefficients
//' @param phi0vec \code{numeric} vector giving starting values for dispersion parameters
//' 
//' @return sampled correlation matrix
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List sample_copula_cpp (
    arma::mat const& ymat,
    List const& Xlist,
    std::vector<std::string> const& distnamevec,
    std::vector<std::string> const& linknamevec, 
    arma::vec const& c0vec, 
    List const& S0betalist,
    arma::vec sigma0logphivec,
    arma::vec const& alpha0vec, 
    arma::vec const& gamma0vec, 
    arma::mat Gamma,
    int const& v0, 
    arma::mat const& V0, 
    double const& b0, 
    arma::mat const& y0mat, 
    List const& X0list,
    int const& M,
    List beta0list,
    arma::vec phi0vec,
    int const& thin
) {
  // get number of endpoints and number of observations
  int J = ymat.n_cols;
  int n = ymat.n_rows;
  int n0 = y0mat.n_rows;
  
  arma::vec pvec = arma::vec(J, arma::fill::zeros);
  for( int j = 0; j < J; j++ ) {
    arma::mat X = Xlist[j];
    pvec(j) = X.n_cols;
  }
  
  // initialize results
  List betasample(J);
  arma::mat phisample = arma::mat( M, J, arma::fill::zeros );
  arma::mat betaacceptmat = arma::mat( M, J, arma::fill::zeros );
  arma::mat phiacceptmat = arma::mat( M, J, arma::fill::zeros );
  arma::cube Gammasample = arma::cube( J, J, M, arma::fill::zeros );
  double alpha0, gamma0;
  
  // Initialize temp variables
  arma::mat X, Z, S0beta, Gammainv, betamatj;  // INITIALIZE
  std::string distname, linkname;  // INITIALIZE
  
  // Compute scale matrix for Inverse Wishart
  arma::mat v0V0 = v0 * V0;
  
  // Initialize betasample as list of matrices
  
  // Obtain initial Z and initialize betasample as list of matrices
  Z = arma::mat( n, J, arma::fill::randn );
  for ( int j = 0; j < J; j++ ) {
    // First, get initial Z
    arma::vec y = ymat.col(j);
    arma::mat X = Xlist[j];
    arma::vec beta = beta0list[j];
    double phi = phi0vec(j);
    Z = update_Z( y, X, beta, phi, Z, Gamma, distnamevec[j], linknamevec[j], n, j);
    
    alpha0 = alpha0vec(j);
    gamma0 = gamma0vec(j);
    
    // initialize betasample as list of matrices
    betamatj = arma::mat( M, X.n_cols, arma::fill::zeros );
    betasample[j] = betamatj;
  }
  
  // Loop through the M iterations
  for ( int m = 0; m < thin * M; m++ ) {
    
    // Store gamma inverse
    Gammainv = arma::inv_sympd(Gamma);
    
    // Update the beta parameters for each margin;
    for ( int j = 0; j < J; j++ ) {
      arma::vec y = ymat.col(j);
      arma::mat X = Xlist[j];
      arma::vec y0 = y0mat.col(j);
      arma::mat X0 = X0list[j];
      arma::vec beta = beta0list[j];
      double phi = phi0vec(j);
      int p = pvec(j);
      arma::mat S0beta = S0betalist[j];
      double sigma0logphi = sigma0logphivec(j);
      
      
      // Obtain update of (beta, phi) and Z; store as vars. Note if b0 == 0, the historical
      // data is ignored in computing log posterior
      List update = update_params( 
        y, X, beta, phi, Z, Gammainv, c0vec(j), S0beta, sigma0logphi, distnamevec[j], linknamevec[j], n,
        j, J, p, alpha0, gamma0, b0, y0, X0, n0
      );
      
      // Temporarily store sampled values for (m,j) iteration
      arma::vec betanew = update["beta"];
      double phinew = update["phi"];
      arma::mat accept = update["accept"];
      arma::mat Znew = update["Z"];
      
      
      // Replace starting values with newly sampled values for next iteration
      beta0list[j] = betanew;
      phi0vec(j) = phinew;
      
      
      // Store samples
      if ( m % thin == 0 ) {
        int i = m / thin;
        arma::mat betamatj = betasample[j];
        betamatj.row(i) = betanew.t();
        betasample[j] = betamatj;
        
        phisample( i, j ) = phinew;
        betaacceptmat( i, j ) = accept(0);
        phiacceptmat( i, j ) = accept(1);
      }
    }
    
    // Update Gamma based on updated parameters and Z
    arma::mat Gamma = update_Gamma( Z, n, v0, v0V0 );
    if ( m % thin == 0 ) {
      int i = m / thin;
      Gammasample.slice(i) = Gamma;
    }
  }
  
  
  return List::create(
    _["betasample"] = betasample,
    _["phisample"] = phisample,
    _["Gammasample"] = Gammasample,
    _["betaaccept"] = betaacceptmat,
    _["phiaccept"] = phiacceptmat
  );
}




















