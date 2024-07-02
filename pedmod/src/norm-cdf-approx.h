/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998	    Ross Ihaka
 *  Copyright (C) 2000-2013 The R Core Team
 *  Copyright (C) 2003	    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef NORM_CDF_APPROX_H
#define NORM_CDF_APPROX_H
#include <cmath>

namespace PNORM_APROX_IMPL {

constexpr double h = 0.0304370378105459,
                 hinv = 1. / h,
                 xmin = -6.36134090240406,
                 xinf = -37.5193;
constexpr int n_pts = 210L;
                 
double fastncdf_pos_intrap(double const x, int const i) noexcept;

inline double fastncdf_pos(double const x) noexcept {
  // bounds checks
  if(x >= xmin){
    int const i = static_cast<int>((x - xmin) * hinv);

    if(i < n_pts - 1){
      return fastncdf_pos_intrap(x, i);
    } else
      return 0.5;

  } else if(x < xinf)
    return 0;

  // i < 0 and x >= xinf
  constexpr double p[6] = {
    0.21589853405795699,
    0.1274011611602473639,
    0.022235277870649807,
    0.001421619193227893466,
    2.9112874951168792e-5,
    0.02307344176494017303
  };
  constexpr double q[5] = {
    1.28426009614491121,
    0.468238212480865118,
    0.0659881378689285515,
    0.00378239633202758244,
    7.29751555083966205e-5
  };

  double const y = -x;
  double xsq = 1.0 / (x * x),
    xnum = p[5] * xsq,
    xden = xsq;
  for (int i = 0; i < 4; ++i) {
    xnum = (xnum + p[i]) * xsq;
    xden = (xden + q[i]) * xsq;
  }
  double temp = xsq * (xnum + p[4]) / (xden + q[4]);
  constexpr double M_1_SQRT_2PI_LOC =	0.398942280401432677939946059934;	/* 1/sqrt(2pi) */
  temp = (M_1_SQRT_2PI_LOC - temp) / y;

  xsq = std::trunc(x * 16) / 16;
  double const del = (x - xsq) * (x + xsq);
  return std::exp(-xsq * xsq * 0.5 -del * 0.5) * temp;
}

} // namespace PNORM_APROX_IMPL

/** normal CDF approximations using monotone cubic interpolation. */
inline double pnorm_approx(double const x) noexcept {
  return x >= 0. 
    ? .5 - PNORM_APROX_IMPL::fastncdf_pos(-x) + .5 
    : PNORM_APROX_IMPL::fastncdf_pos(x);
}

#endif
