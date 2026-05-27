#include <Rcpp.h>
# include "fittingmeasure.h"
using namespace Rcpp;

///' Sort a vector in descendin order 
///'
// [[Rcpp::export]]
Rcpp::NumericVector dbl_sort(Rcpp::NumericVector x) {
    Rcpp::NumericVector res = Rcpp::clone(x);
    res.sort(true);
    return res;
}

