/*
 * Copyright Constantino Antonio Garcia 2017
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include <array>
#include <cmath>
#include <iostream>
#include <lbfgsb_cpp/lbfgsb.hpp>

using namespace lbfgsb;

double get_objective(const std::array<double, 2>& x, std::array<double, 2>& grad) {
  grad[0] = 2 * (x[0] - 0.5);
  grad[1] = 2 * (x[1] - 1);
  return std::pow(x[0] - 0.5, 2) + std::pow(x[1] - 1, 2);
}

int main() {
  const std::array<double, 2> lb{-2, -2};
  const std::array<double, 2> ub{2, 2};
  // 0 if unbounded,
  // 1 if only a lower bound,
  // 2 if both lower and upper bounds,
  // 3 if only an upper bound.
  const std::array<int, 2> bound_type{2, 2};

  Optimizer optimizer{lb.size()};
  // Can adjust many optimization configs.
  // E.g. `iprint`, `factr`, `pgtol`, `max_iter`, `max_fun`, `time_limit_sec`
  optimizer.iprint = 1;
  std::array<double, 2> x0{2, 3};
  auto result = optimizer.minimize(get_objective, x0, data(lb), data(ub), bound_type.data());
  result.print();

  // (0.5, 1) => 0
  std::cout << "x0: (" << x0[0] << ", " << x0[1] << ")" << std::endl;

  return 0;
}
