#ifndef SQUATQTSSAMPLE_H
#define SQUATQTSSAMPLE_H

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame mean_qts_impl(const Rcpp::List &qts_list);
// [[Rcpp::export]]
Rcpp::DataFrame median_qts_impl(const Rcpp::List &qts_list);

#endif /* SQUATQTSSAMPLE_H */
