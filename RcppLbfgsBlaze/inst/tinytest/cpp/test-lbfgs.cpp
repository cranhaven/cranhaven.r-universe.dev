// Copyright (C)  2024  Ching-Chuan Chen
//
// This file is part of RcppLbfgsBlaze.
//
// RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
// under the terms of the MIT License. You should have received
// a copy of MIT License along with RcppLbfgsBlaze.
// If not, see https://opensource.org/license/mit.

// [[Rcpp::depends(RcppBlaze)]]
// [[Rcpp::depends(RcppLbfgsBlaze)]]
#include <RcppBlaze.h>
#include <lbfgs.h>
using Rcpp::_;

class Problem1 {
public:
  Rcpp::List run(Rcpp::NumericVector init) {
    double final_value;

    // set init vector
    int n = init.size();
    std::size_t padded_size = blaze::nextMultiple<std::size_t>(n, blaze::SIMDTrait<double>::size);
    std::unique_ptr<double[], blaze::Deallocate> data(blaze::allocate<double>(padded_size));
    lbfgs::BlazeVector x(data.get(), n, padded_size);
    RcppBlaze::copyToCustomVector(init, x);

    // Set the minimization parameters
    lbfgs::lbfgs_parameter_t params;
    params.g_epsilon = 1.0e-8;
    params.delta = 1.0e-8;

    // Start minimization
    int result = lbfgs::lbfgs_optimize(x, final_value, cost_function, nullptr, nullptr, this, params);

    return Rcpp::List::create(
      _["value"] = final_value,
      _["par"] = x,
      _["lbfgs_result_code"] = result
    );
  }

private:
  static double cost_function(void *instance, const lbfgs::BlazeVector &x, lbfgs::BlazeVector &g) {
    double fx = 100 * (x[1] - x[0] * x[0]) * (x[1] - x[0] * x[0]) + (1 - x[0]) * (1 - x[0]);
    g[0] = -400 * x[0] * (x[1] - x[0] * x[0]) - 2 * (1 - x[0]);
    g[1] = 200 * (x[1] - x[0] * x[0]);
    return fx;
  }
};

// [[Rcpp::export]]
Rcpp::List test_problem1(Rcpp::NumericVector init) {
  int n = init.size();
  if (n != 2) {
    Rcpp::stop("The length of init must be 2");
  }

  Problem1 instance;
  return instance.run(init);
}

// [[Rcpp::export]]
Rcpp::List test_problem2(Rcpp::NumericVector init) {
  int n = init.size();
  if (n != 2) {
    Rcpp::stop("The length of init must be 2");
  }

  // set init vector
  std::size_t padded_size = blaze::nextMultiple<std::size_t>(n, blaze::SIMDTrait<double>::size);
  std::unique_ptr<double[], blaze::Deallocate> data(blaze::allocate<double>(padded_size));
  lbfgs::BlazeVector x(data.get(), n, padded_size);
  RcppBlaze::copyToCustomVector(init, x);

  // Set objective function
  const lbfgs::lbfgs_evaluate_t obj_func = [](void* instance, const lbfgs::BlazeVector &x, lbfgs::BlazeVector &g) -> double {
    double fx = (x[0] + 2 * x[1] - 7) * (x[0] + 2 * x[1] - 7) + (2 * x[0] + x[1] - 5) * (2 * x[0] + x[1] - 5);
    g[0] = 10 * x[0] + 8 * x[1] - 34;
    g[1] = 8 * x[0] + 10 * x[1] - 38;
    return fx;
  };

  // Set the minimization parameters
  lbfgs::lbfgs_parameter_t params;
  params.g_epsilon = 1.0e-8;
  params.delta = 1.0e-8;

  // Start minimization
  double final_value;
  int result = lbfgs::lbfgs_optimize(x, final_value, obj_func, nullptr, nullptr, nullptr, params);

  return Rcpp::List::create(
    _["value"] = final_value,
    _["par"] = x,
    _["lbfgs_result_code"] = result
  );
}

class Problem3 {
public:
  Rcpp::List run(lbfgs::BlazeVector &x) {
    double final_value;

    // Set the minimization parameters
    lbfgs::lbfgs_parameter_t params;
    params.g_epsilon = 1.0e-8;
    params.delta = 1.0e-8;

    // Start minimization
    int result = lbfgs::lbfgs_optimize(x, final_value, cost_function, nullptr, nullptr, this, params);

    return Rcpp::List::create(
      _["value"] = final_value,
      _["par"] = x,
      _["lbfgs_result_code"] = result
    );
  }

private:
  static double cost_function(void *instance, const lbfgs::BlazeVector &x, lbfgs::BlazeVector &g) {
    const std::size_t n = x.size();
    double fx = 0.0, t1, t2;
    for (std::size_t i = 0UL; i < n; i += 2) {
      t1 = 1.0 - x[i];
      t2 = 10.0 * (x[i + 1] - x[i] * x[i]);
      g[i + 1] = 20.0 * t2;
      g[i] = -2.0 * (x[i] * g[i + 1UL] + t1);
      fx += t1 * t1 + t2 * t2;
    }
    return fx;
  }
};

// [[Rcpp::export]]
Rcpp::List test_problem3(int n) {
  std::size_t padded_size = blaze::nextMultiple<std::size_t>((size_t)n, blaze::SIMDTrait<double>::size);
  std::unique_ptr<double[], blaze::Deallocate> data(blaze::allocate<double>(padded_size));
  lbfgs::BlazeVector x(data.get(), (size_t)n, padded_size);

  // Set the initial guess
  for (std::size_t i = 0; i < (size_t)n; i += 2) {
    x[i] = -1.2;
    x[i + 1] = 1.0;
  }

  Problem3 instance;
  return instance.run(x);
}
