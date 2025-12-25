# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp ;

// [[Rcpp::export()]]

NumericMatrix funD2(arma::mat F) {
  
  using namespace Rcpp;            
  
  int nrows = F.n_rows;
  int ncols = F.n_cols;
  NumericMatrix xx(nrows,ncols);
  
  for (int i = 1; i < (nrows-1); i++) {
    arma::mat s1  = F(arma::span(0,i-1), arma::span(1,ncols-1));
    arma::mat s2  = F(arma::span(i+1,nrows-1), arma::span(0,ncols-2));
    xx(i,0)       = arma::accu(s1);
    xx(i,ncols-1) = arma::accu(s2);
  }
  
  return xx;
}
