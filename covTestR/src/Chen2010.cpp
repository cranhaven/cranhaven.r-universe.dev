#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Chen2010Stat(arma::mat x) {
  int ncol = x.n_cols;
  int nrow = x.n_rows;
  double quadraNum = 0;
  double bilinearoffNum = 0;
  double bilinearcubeNum = 0;
  double bilinearsquareNum = 0;
  double bilinearquadNum = 0;
  
  for(int i = 0; i < nrow; ++i){
    quadraNum += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
      x.submat(i, 0, i, ncol - 1).t());
    for(int j = i + 1; j < nrow; ++j){
      bilinearoffNum += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
        x.submat(j, 0, j, ncol - 1).t());
      
      bilinearsquareNum += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
        x.submat(j, 0, j, ncol - 1).t() *
        x.submat(i, 0, i, ncol - 1) *
        x.submat(j, 0, j, ncol - 1).t());
      for(int k = j + 1; k < nrow; ++k){
        bilinearcubeNum += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
          x.submat(j, 0, j, ncol - 1).t() *
          x.submat(j, 0, j, ncol - 1) *
          x.submat(k, 0, k, ncol - 1).t());
        for(int l = k + 1; l < nrow; ++l){
          bilinearquadNum += arma::as_scalar(x.submat(i, 0, i, ncol - 1) *
            x.submat(j, 0, j, ncol - 1).t() *
            x.submat(k, 0, k, ncol - 1) *
            x.submat(l, 0, l, ncol - 1).t());
        }
      }
    }
  }
  
  double bilinearquad = bilinearquadNum * 24.0 / (R::gammafn(nrow + 1.0) / R::gammafn(nrow - 3.0));
  
  double bilinearsquare = bilinearsquareNum * 2.0 / (R::gammafn(nrow + 1.0) / R::gammafn(nrow - 1.0));
  
  double bilinearcube = bilinearcubeNum * 6.0 / (R::gammafn(nrow + 1.0) / R::gammafn(nrow - 2.0));
  
  double bilinearoff = bilinearoffNum * 2.0 / (R::gammafn(nrow + 1.0) / R::gammafn(nrow - 1.0));
  
  double quadra = quadraNum / nrow;
  
  return nrow * (bilinearsquare / ncol -
              2.0 * bilinearcube / ncol +
              bilinearquad / ncol -
              2.0 * quadra / ncol +
              2.0 * bilinearoff / ncol +
              1.0) / 2.0;
}
