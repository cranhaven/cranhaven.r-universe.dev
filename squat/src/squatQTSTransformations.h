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
Rcpp::DataFrame qts2aats_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame qts2rpyts_impl(const Rcpp::DataFrame &qts);

// [[Rcpp::export]]
Rcpp::DataFrame rpyts2qts_impl(const Rcpp::DataFrame &rpyts);

// https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
void GetRPYAngles(
    const double &w, const double &x, const double &y, const double &z,
    double &roll, double &pitch, double &yaw
);

#endif /* SQUATQTSTRANSFORMATIONS_H */
