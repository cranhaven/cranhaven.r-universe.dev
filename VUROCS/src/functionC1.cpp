# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp ;

// [[Rcpp::export()]]

NumericMatrix funC1(arma::mat F) {
  
  using namespace Rcpp;            
  
  int nrows = F.n_rows;
  int ncols = F.n_cols;
  NumericMatrix xx(nrows,ncols);
  
  for (int j = 1; j < (ncols-1); j++) {
    arma::mat s1  = F(arma::span(1,nrows-1), arma::span(j+1,ncols-1));
    arma::mat s2  = F(arma::span(0,nrows-2), arma::span(0,j-1));
    xx(0,j)       = arma::accu(s1);
    xx(nrows-1,j) = arma::accu(s2);
  }
  
  return xx;
}
