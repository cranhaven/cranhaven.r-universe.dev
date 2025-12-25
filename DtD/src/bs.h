#ifndef BS_H
#define BS_H

#include "RcppArmadillo.h"
#include <array>

double BS_call_cpp(
    const double, const double, const double, const double, const double);

double BS_call_cpp_inv(
    const double, const double, const double, const double, const double,
    const double,
    double, double, double);

arma::vec get_underlying_cpp(
    const arma::vec&, const arma::vec&, const arma::vec&, const arma::vec&,
    const arma::vec&, const double);

struct est_result {
  double mu, vol;
  bool success;
  unsigned int n_iter;

  est_result():
    mu (std::numeric_limits<double>::quiet_NaN()),
    vol(std::numeric_limits<double>::quiet_NaN()),
    success(false), n_iter(0L) {}
  };

est_result est_iterative(
    const arma::vec&, const arma::vec&, const arma::vec&, const arma::vec &,
    const arma::vec&, double vol, const double, const double);

est_result mle(
    const arma::vec&, const arma::vec&, const arma::vec&, const arma::vec &,
    const arma::vec&, double vol, const double, const double);

#endif // BS_H
