# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp ;

// [[Rcpp::export()]]

NumericMatrix funD(arma::mat F) {
  
  using namespace Rcpp;            
  
  int nrows = F.n_rows;
  int ncols = F.n_cols;
  NumericMatrix xx(nrows,ncols);
  
  for (int i = 1; i < (nrows-1); i++) {
    for (int j = 1; j < (ncols-1); j++) {
      arma::mat s1 = F(arma::span(i+1,nrows-1), arma::span(0,j-1));
      arma::mat s2 = F(arma::span(0,i-1), arma::span(j+1,ncols-1));
      xx(i,j) = arma::accu(s1) + arma::accu(s2);
    }
  }
  
  return xx;
}
