/*
 Copyright (C) 2021 Zdravko Botev and Leo Belzile

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "pnorm.h"
#include "qtnorm.h"
#include <limits.h>
#include <cmath>

namespace {
inline double qfunc(double const x){
  return std::exp(x * x / 2 + pnorm_std(x, false, true));
}
} // namespace

double qtnorm(double const x, double lower, double upper){
  if(x >= 1)
    return upper;
  else if(x <= 0)
    return lower;
  else if(upper < 0)
    return -qtnorm(1 - x, -upper, -lower);

  double const q_lower{qfunc(lower)},
               q_upper{std::isinf(upper) ? 0 : qfunc(upper)};

  lower *= lower;
  upper *= upper;
  double z{std::sqrt(lower - 2 * log(1 + x * expm1((lower - upper) / 2)))},
       err{std::numeric_limits<double>::infinity()};

  constexpr unsigned max_step{100};
  unsigned step{};

  while(err > 1e-10 && step++ < max_step){
    double const step
      {-qfunc(z)
        + (1 - x) * std::exp((z *  z - lower) / 2) * q_lower
        + x * std::exp((z * z - upper) / 2) * q_upper};
    z -= step;
    err = std::abs(step);
  }

  return z;
}
