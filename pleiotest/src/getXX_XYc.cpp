#include <Rmath.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List getXX_XYc(IntegerVector x, List xrows_bags, List y_bags, IntegerMatrix N, List which_trait_bags) {
  NumericVector xx(N.nrow());
  int ntr = N.ncol() - 1;
  NumericMatrix xy(N.nrow(), ntr);

  for (int i = 0; i < N.nrow(); i++){
    IntegerVector xrows_i = as<IntegerVector>(xrows_bags[i]) - 1;
    IntegerVector trait_i = as<IntegerVector>(which_trait_bags[i]) - 1;
    IntegerVector x_i = x[xrows_i];
    NumericMatrix y_i = as<NumericMatrix>(y_bags[i]);
    double xtx = 0;
    double xt1 = 0;
    IntegerVector xt2(ntr);
    NumericVector yt1(ntr);
    NumericVector xty(ntr);
    IntegerVector n2(ntr);
    int n = 0;
    for (int j = 0; j < x_i.size(); j++){
      if(arma::is_finite(x_i[j])){
        n++;
        for (int k1 = 0; k1 < y_i.ncol(); k1++){
          n2[k1] += 1;
          yt1[k1] += y_i(j, k1);
        }
        if(x_i[j] == 1){
          xt1++;
          xtx++;
          for (int k2 = 0; k2 < y_i.ncol(); k2++){
            xt2[k2] += 1;
            xty[k2] += y_i(j, k2);
          }
        }
        if(x_i[j] == 2){
          xt1 += 2;
          xtx += 4;
          for (int k3 = 0; k3 < y_i.ncol(); k3++){
            xt2[k3] += 2;
            xty[k3] += 2 * y_i(j, k3);
          }
        }
      }
    }
    for (int k4 = 0; k4 < ntr; k4++){
      double res = xty[k4] - (xt2[k4] * yt1[k4]) / n2[k4];
      if(arma::is_finite(res)){
        xy(i, trait_i[k4]) = res;

      }
    }
    xx[i] = (xtx - (xt1 * xt1) / n);
  }
  return List::create(Named("xx") = xx, Named("xy") = xy);
}

