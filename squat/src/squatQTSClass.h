#ifndef SQUATQTSCLASS_H
#define SQUATQTSCLASS_H

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame reorient_qts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame normalize_qts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame derivative_qts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame log_qts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame exp_qts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::List centring_qts_impl(const Rcpp::DataFrame &qts, const bool standardize = false);

// [[Rcpp::export]]
Rcpp::DataFrame resample_qts_impl(
    const Rcpp::DataFrame &qts,
    double tmin = NA_REAL,
    double tmax = NA_REAL,
    const unsigned int nout = 0
);

// [[Rcpp::export]]
Rcpp::DataFrame smooth_qts_impl(
    const Rcpp::DataFrame &qts,
    const double alpha = 0.5
);

// [[Rcpp::export]]
Rcpp::DataFrame hemispherize_qts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame moving_average_qts_impl(
    const Rcpp::DataFrame &qts,
    const unsigned int window_size
);

// [[Rcpp::export]]
Rcpp::DataFrame multiply_qts_impl(
    const Rcpp::DataFrame &qts_left,
    const Rcpp::DataFrame &qts_right
);

// [[Rcpp::export]]
Rcpp::DataFrame inverse_qts_impl(
    const Rcpp::DataFrame &qts
);

#endif /* SQUATQTSCLASS_H */
