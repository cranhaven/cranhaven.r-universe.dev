#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include "bayescopulareg.h"
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//[[Rcpp::export]]
arma::mat free_to_chol_cpp(
  arma::vec y
) {
  // Compute lower triangular elements of Z matrix = tanh(y)
  y = arma::tanh(y);
  int J = (int) ( 0.5 * (1 + std::sqrt(1.0 + 8.0 * y.size())) );   // inverse of choose(J,2)
  arma::mat Z = arma::mat(J, J, arma::fill::zeros);
  Z.elem(arma::trimatl_ind( arma::size(Z), -1)) = y;
  
  // Iterate through Z matrix to create X matrix, the lower cholesky decomposition
  arma::mat X = Z;
  for ( int j = 1; j < J; j++ ) {
    arma::rowvec temp = X.row(j).head(j);                    // (X[i,1], ... X[i,j-1])
    X(j,j)   = std::sqrt(1 - arma::accu(arma::square(temp)));  // sqrt( 1 - sum(temp^2) )
    for ( int i = j+1; i < J; i++ ) {
      temp = X.row(i).head(j);
      X(i,j) = Z(i,j) * std::sqrt(1 - arma::accu(arma::square(temp)));
    }
  }
  X(0,0) = 1;
  return X;
}

//[[Rcpp::export]]
std::vector<double> chol_to_free_cpp(arma::mat X) {
  int J = X.n_cols;
  arma::mat Z = X;
  Z.diag() = arma::vec(J, arma::fill::zeros);
  for ( int j = 1; j < J; j++ ) {
    for ( int i = j+1; i < J; i++ ) {
      arma::rowvec temp = X.row(i).head(j);
      Z(i,j) = X(i,j) / std::sqrt(1 - arma::accu(arma::square(temp)));
    }
  }
  // get lower triangular elements; take tanh^(-1) and return
  arma::vec y = arma::atanh( Z.elem(arma::trimatl_ind( arma::size(Z), -1)) );
  return as<std::vector<double>>(wrap(y));
}
