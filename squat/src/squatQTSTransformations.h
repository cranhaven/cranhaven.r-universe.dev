#ifndef SQUATQTSTRANSFORMATIONS_H
#define SQUATQTSTRANSFORMATIONS_H

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame qts2dts_impl(
    const Rcpp::DataFrame &first_qts,
    const Rcpp::DataFrame &second_qts
);

// [[Rcpp::export]]
Rcpp::DataFrame qts2nts_impl(
    const Rcpp::DataFrame &qts,
    const bool disable_normalization = false
);

// [[Rcpp::export]]
Rcpp::DataFrame qts2ats_impl(
    const Rcpp::DataFrame &qts,
    const bool disable_normalization = false
);

// [[Rcpp::export]]
Rcpp::DataFrame qts2avts_impl(
    const Rcpp::DataFrame &qts,
    const bool body_frame = false
);

// [[Rcpp::export]]
Rcpp::DataFrame qts2aats_impl(
    const Rcpp::DataFrame &qts
);


#endif /* SQUATQTSTRANSFORMATIONS_H */
