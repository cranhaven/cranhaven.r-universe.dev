// Copyright (C)  2024  Ching-Chuan Chen
//
// This file is part of RcppLbfgsBlaze.
//
// RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
// under the terms of the MIT License. You should have received
// a copy of MIT License along with RcppLbfgsBlaze.
// If not, see https://opensource.org/license/mit.

#include <RcppBlaze.h>
#include <lbfgs.h>
using Rcpp::_;

struct LogisticData {
  size_t n;
  size_t n_padded;
  size_t p;
  std::unique_ptr<double[], blaze::Deallocate> x_data;
  std::unique_ptr<double[], blaze::Deallocate> y_data;
  std::unique_ptr<double[], blaze::Deallocate> eta_data;
  std::unique_ptr<double[], blaze::Deallocate> phat_data;

  LogisticData(
    size_t n_, size_t n_padded_, size_t p_,
    std::unique_ptr<double[], blaze::Deallocate> X_ptr,
    std::unique_ptr<double[], blaze::Deallocate> y_ptr,
    std::unique_ptr<double[], blaze::Deallocate> eta_ptr,
    std::unique_ptr<double[], blaze::Deallocate> phat_ptr
  ): n(n_), n_padded(n_padded_), p(p_),
  x_data(std::move(X_ptr)), y_data(std::move(y_ptr)),
  eta_data(std::move(eta_ptr)), phat_data(std::move(phat_ptr)) {}
};

static double getLogisticLikelihoodGrad(
  void *instance,
  const lbfgs::BlazeVector &coef,
  lbfgs::BlazeVector &grad
) {
  LogisticData *logisticData = reinterpret_cast<LogisticData*>(instance);
  lbfgs::BlazeMatrix X((logisticData->x_data).get(), logisticData->n, logisticData->p, logisticData->n_padded);
  lbfgs::BlazeVector y((logisticData->y_data).get(), logisticData->n, logisticData->n_padded);
  lbfgs::BlazeVector eta((logisticData->eta_data).get(), logisticData->n, logisticData->n_padded);
  lbfgs::BlazeVector phat((logisticData->phat_data).get(), logisticData->n, logisticData->n_padded);
  eta = blaze::max(blaze::min(X * coef, 30.0), -30.0);
  phat = 1.0/(1.0 + blaze::exp(-eta));
  double fx = -blaze::dot(eta, y) - blaze::sum(blaze::log(1.0 - phat));
  grad = blaze::trans(X) * (phat - y);
  return fx;
}

//' Logistic Regression Fitting Using L-BFGS Algorithm
//'
//' This function leverage \code{blaze} and \code{LBFGS-Blaze} to efficiently fit logistic regression.
//'
//' @param X The model matrix.
//' @param y The response vector.
//' @return A list of L-BFGS optimization result.
//'
//' @examples
//' X <- matrix(rnorm(5000), 1000)
//' coef <- runif(5, -3, 3)
//' y <- sapply(1 / (1 + exp(-X %*% coef)), function(p) rbinom(1, 1, p), USE.NAMES = FALSE)
//'
//' fit <- fastLogisticModel(X, y)
//' @export
// [[Rcpp::export]]
Rcpp::List fastLogisticModel(Rcpp::NumericMatrix X, Rcpp::NumericVector y) {
  // initialize coef
  size_t p = (size_t) X.ncol();
  std::size_t p_padded = blaze::nextMultiple<std::size_t>(p, blaze::SIMDTrait<double>::size);
  std::unique_ptr<double[], blaze::Deallocate> coef_data(blaze::allocate<double>(p_padded));
  lbfgs::BlazeVector coef(coef_data.get(), p, p_padded);
  coef = 0.0;

  // allocate memory for y
  size_t n = (size_t) y.size();
  std::size_t n_padded = blaze::nextMultiple<std::size_t>(n, blaze::SIMDTrait<double>::size);
  std::unique_ptr<double[], blaze::Deallocate> y_data(blaze::allocate<double>(n_padded));
  lbfgs::BlazeVector y_(y_data.get(), n, n_padded);
  RcppBlaze::copyToCustomVector(y, y_);

  // allocate memory for X
  std::unique_ptr<double[], blaze::Deallocate> x_data(blaze::allocate<double>(n_padded * p));
  lbfgs::BlazeMatrix X_(x_data.get(), n, p, n_padded);
  RcppBlaze::copyToCustomMatrix(X, X_);

  // allocate memory for eta and phat
  std::unique_ptr<double[], blaze::Deallocate> eta_data(blaze::allocate<double>(n_padded));
  std::unique_ptr<double[], blaze::Deallocate> phat_data(blaze::allocate<double>(n_padded));
  lbfgs::BlazeVector eta(eta_data.get(), n, n_padded);
  lbfgs::BlazeVector phat(phat_data.get(), n, n_padded);

  // Set the minimization parameters
  lbfgs::lbfgs_parameter_t params;
  params.delta = 1.0e-5;

  // get logistic data
  LogisticData ld(
      n, n_padded, p, std::move(x_data),
      std::move(y_data), std::move(eta_data), std::move(phat_data)
  );

  // Start minimization
  double final_value = 0.0;
  int result = lbfgs::lbfgs_optimize(coef, final_value, getLogisticLikelihoodGrad, nullptr, nullptr, &ld, params);

  return Rcpp::List::create(
    _["coefficients"] = coef,
    _["fitted.values"] = phat,
    _["linear.predictors"] = eta,
    _["loglikelihood"] = -final_value,
    _["converged"] = result == 0
  );
}
