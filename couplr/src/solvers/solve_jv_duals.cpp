// src/solvers/solve_jv_duals.cpp
// Pure C++ Jonker-Volgenant solver with dual variables - NO Rcpp dependencies.
// Thin wrapper that prepares the cost source, delegates to detail::jv_core(),
// and also returns the row/column potentials.

#include "solve_jv_duals_impl.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <cmath>
#include <utility>

namespace lap {

DualResult solve_jv_duals(const CostMatrix& cost, bool maximize, bool warm_start) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) {
        DualResult result;
        result.solution = LapResult({}, 0.0, "optimal");
        return result;
    }
    lap::require_rows_fit_cols(n, m);

    CostMatrix work = prepare_for_solve(cost, maximize);
    ensure_each_row_has_option(work.mask, n, m);

    return jv_duals_detail::jv_duals_from(work, cost,
                                          /*report_negated=*/false,
                                          /*solved_negated=*/maximize,
                                          warm_start);
}

DualResult solve_jv_duals(const LazyCostMatrix& cost) {
    return solve_jv_duals_prepared(cost);
}

}  // namespace lap
