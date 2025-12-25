#include "bayescopulareg.h"
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;


//' Posterior predictive sample of copula GLM
//' 
//' Obtain a sample from the posterior predictive density of a copula GLM
//' 
//' @param Xlist a \code{list} of length \eqn{J}, each element is a design \code{matrix}
//' @param distnamevec a \code{character} vector of length \eqn{J} giving the name of the distribution of each endpoint
//' @param linknamevec a \code{character} vector of length \eqn{J} giving the name of the link function of each endpoint
//' @param Gamma a sampled correlation \code{matrix}
//' @param betasample a \eqn{J}-dimensional \code{list} of sampled regression coefficients
//' @param phisample a \code{vector} of sampled dispersion parameters
//' @param n sample size for future data
//' @param J number of endpoints
//' 
//' @return a \eqn{n \times J} matrix of samples from the predictive posterior density
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::mat copula_predict (
  List const& Xlist,
  std::vector<std::string> const& distnamevec,
  std::vector<std::string> const& linknamevec, 
  arma::mat const& Gamma,
  List const& betasample,
  arma::vec const& phisample,
  int const& n,
  int const& J
) {
  
  // Create empty matrix for y's
  arma::mat ymat = arma::mat( n, J, arma::fill::zeros );
  
  // First, sample Z ~ N(0, Gamma)
  arma::mat Z = arma::mat( n, J, arma::fill::randn ); // n x J matrix filled with N(0,1) values
  Z = Z * arma::chol( arma::symmatu( Gamma) , "upper" );               // Z * U where Gamma = U' U and U is upper triangular
  
  // Now, sample y based on (beta, phi)
  for ( int j = 0; j < J; j++ ) {
    arma::mat X = Xlist[j];
    arma::vec beta = betasample[j];
    double phi = phisample(j);
    
    // call sample_y from invcdf.cpp file
    ymat.col(j) = sample_y( Z.col(j), X, beta, phi, distnamevec[j], linknamevec[j], n);
  }
  return ymat;
}




//' List of posterior predictive samples of copula GLM
//' 
//' Obtain a sample from the posterior predictive density of a copula GLM
//' 
//' @param Xlist a \code{J}-dimensional list of design matrices corresponding to new data
//' @param distnamevec a \code{character} vector of length \eqn{J} giving the name of the distribution of each endpoint
//' @param linknamevec a \code{character} vector of length \eqn{J} giving the name of the link function of each endpoint
//' @param betasamplelist a list of length \code{J}. Each element is a list of length \code{M} giving the posterior draws
//' @param phisamplemat a \eqn{M \times J} matrix of sampled dispersion parameters
//' @param Gammaarray a \eqn{J \times J \times M} array of sampled correlation matrices
//' @param n sample size for future data
//' @param J number of endpoints
//' @param M number of samples
//' 
//' @return \code{array} of dimension \code{c(n, J, nsims)} of predictive posterior draws. Each slice corresponds to 1 draw
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
arma::cube copula_predict_all (
    List const& Xlist,
    std::vector<std::string> const& distnamevec,
    std::vector<std::string> const& linknamevec,
    List const& betasamplelist,
    arma::mat const& phisamplemat,
    arma::cube const& Gammaarray,
    int const& n,
    int const& J,
    int const& M
) {
  arma::cube yarray = arma::cube(n, J, M, arma::fill::zeros);
  List betasample = List(J);
  for ( int m = 0; m < M; m++ ) {
    for ( int j = 0; j < J; j++ ) {
      arma::mat betaj = betasamplelist[j];
      betasample[j] = betaj.row(m).t();
    }
    arma::vec phisample = phisamplemat.row(m).t();
    arma::mat Gamma = Gammaarray.slice(m);
    arma::mat ymat = copula_predict( Xlist, distnamevec, linknamevec, Gamma, betasample, phisample, n, J );
    yarray.slice(m) = ymat;
  }
  return yarray;
}





//' List of posterior predictive samples of copula GLM
//' 
//' Obtain a sample from the posterior predictive density of a copula GLM
//' 
//' @param Xlistlist a \code{M}-dimensional \code{list} of lists. The inner list is a list of design matrices of length \code{J}
//' @param distnamevec a \code{character} vector of length \eqn{J} giving the name of the distribution of each endpoint
//' @param linknamevec a \code{character} vector of length \eqn{J} giving the name of the link function of each endpoint
//' @param betasamplelist a list of length \code{M}. Each element is a list of length \code{J} giving the design matrix for each endpoint
//' @param phisamplemat a \eqn{M \times J} matrix of sampled dispersion parameters
//' @param Gammaarray a \eqn{J \times J \times M} array of sampled correlation matrices
//' @param n sample size for future data
//' @param J number of endpoints
//' @param M number of samples
//' 
//' @return a \eqn{n \times J} matrix of samples from the predictive posterior density
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List copula_predict_all_list (
  List const& Xlistlist,
  std::vector<std::string> const& distnamevec,
  std::vector<std::string> const& linknamevec,
  List const& betasamplelist,
  arma::mat const& phisamplemat,
  arma::cube const& Gammaarray,
  int const& n,
  int const& J,
  int const& M
) {
  List ymatlist = List(M);
  for ( int m = 0; m < M; m++ ) {
    List Xlist = Xlistlist[m];    // Xlist[m] = list of J design matrices for m-th sample
    List betasample = betasamplelist[m];
    arma::vec phisample = phisamplemat.row(m);
    arma::mat Gamma = Gammaarray.slice(m);
    
    arma::mat ymat = copula_predict( Xlist, distnamevec, linknamevec, Gamma, betasample, phisample, n, J );
    ymatlist[m] = ymat;
  }
  return ymatlist;
}

