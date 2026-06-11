#include <Rcpp.h>



Rcpp::List RcppImputeSOM(
    Rcpp::NumericMatrix data,
    Rcpp::NumericMatrix missData,
    Rcpp::NumericMatrix codes,
    Rcpp::ExpressionVector distanceFunctions,
    Rcpp::NumericMatrix neighbourhoodDistances,
    int neighbourhoodFct,
    Rcpp::NumericVector alphas,
    Rcpp::NumericVector radii,
    int numEpochs,
    bool bool_impute,                                                              
    Rcpp::IntegerVector missingCol,                                               
    Rcpp::IntegerVector missingRow); 



// [[Rcpp::export]]
Rcpp::List RcppMap(
    Rcpp::NumericMatrix data,   /* objects to be mapped */
    Rcpp::NumericMatrix codes,
    Rcpp::ExpressionVector distanceFunctions);
    
