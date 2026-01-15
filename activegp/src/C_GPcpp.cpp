# include <RcppArmadillo.h>
// [[Rcpp :: depends ( RcppArmadillo )]]
using namespace Rcpp;

NumericMatrix W_kappa_ij(NumericMatrix design, NumericVector theta, int i1, int i2, int ct);

//' Equivalent of \code{\link[activegp]{C_GP}} using RcppArmadillo
//' @param design A matrix of design points, one in each row
//' @param response A vector of observations at each design point. 
//' @param theta vector of lengthscales
//' @param Ki inverse covariance matrix
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The active subspace matrix C. 
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix C_GP_cpp(NumericMatrix design, NumericVector response, NumericVector theta, NumericMatrix Ki, int ct){
  int d = design.ncol();
  int n = design.nrow();
  arma::mat C_mat(d, d);
  arma::mat Wij(n, n);
  arma::vec Kir;
  arma::mat M(1,1);

  Kir =  as<arma::mat>(Ki) * as<arma::vec>(response);

  for(int i = 0; i < d; i++){
    for(int j = i; j < d; j++){
      Wij = as<arma::mat>(W_kappa_ij(design, theta, i, j, ct));
      // M = Cov(dYi(X),dYj(X))
      M = 1/(theta(i) * theta(i)) * (i == j) - sum(sum(as<arma::mat>(Ki) % Wij)) + Kir.t() * Wij * Kir;

      // M + E(dYi(X)dYj(X))
      C_mat(i, j) = C_mat(j, i) = M(0,0);

    }
  }

  return(wrap(C_mat));
}
