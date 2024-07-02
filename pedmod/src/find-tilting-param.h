#ifndef FIND_TILTING_PARAM_H
#define FIND_TILTING_PARAM_H

#include <vector>

struct find_tilting_param_res {
  std::vector<double> tilting_param;
  bool success;
  bool is_interior;
};

/**
 * Finds the tilting parameter as suggested by
 *
 *  https://doi.org/10.1111/rssb.12162
 *
 * The cholesky argument should be the Cholesky decomposition of the covariance
 * matrix scaled to have a unit diagonal. The limits should be adjusted
 * accordingly. The limits may contain -Inf or Inf to indicate that the lower or
 * upper limits are not restricted.
 *
 * The last parameter is relative convergence threshold for the optimization
 * method.
 */
find_tilting_param_res find_tilting_param
  (size_t const dim, double const * const lower_limits,
   double const * const upper_limits, double const * const cholesky,
   double const rel_eps = 1e-8);

#endif
