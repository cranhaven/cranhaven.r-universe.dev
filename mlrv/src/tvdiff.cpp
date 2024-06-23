#include <RcppArmadillo.h> 
// [[Rcpp::depends("RcppArmadillo")]]
using namespace arma;
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec Ctvfdiff(arma::vec x, arma::vec d, int burnin)
{
  int n = x.n_elem;
  double s;
  arma::vec xd(n);
  
  for(int i = 0; i <=  burnin; i++){
    xd[i] = x[i];
  }
  for(int i = 0; i < n - burnin; i++){
    s = 1;
    xd[i + burnin] = x[i + burnin];
    for(int j = 1; j <= i + burnin - 1 ; j++){
      s *= (j + d[i] - 1)/j;
      xd[i + burnin] += s * x[i + burnin - j];
    }
    
  }
  return(xd);
}
