// src/solvers/solve_push_relabel.cpp
// Pure C++ Goldberg-Tarjan cost-scaling push-relabel LAP solver.
// NO Rcpp dependencies.
//
// The network is the flow model's one-to-one design and the search is
// solve_min_cost_flow_push_relabel(). What belongs to this solver is how the
// cost matrix is prepared for a scaling algorithm, and what it calls a
// shortfall.

#include "solve_push_relabel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../flow/flow_compile.h"
#include "../flow/flow_push_relabel.h"

#include <algorithm>
#include <cmath>
#include <utility>
#include <vector>

namespace lap {

LapResult solve_push_relabel(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    CostMatrix work = prepare_for_solve(cost, maximize);
    ensure_each_row_has_option(work.mask, n, m);

    // Cost scaling stops at eps < 1/(n+1) because two distinct integer totals
    // differ by at least 1. That bound says nothing about real costs: two
    // assignments can differ by less than n * eps and the phases end on the
    // wrong one with nothing to report. Scaling the allowed costs to integers
    // first is what makes the bound apply. Only allowed cells are touched, so
    // the forbidden sentinel is left as it is, and the total below is computed
    // from the original costs either way.
    double max_abs_cost = 0.0;
    bool all_integer = true;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!work.allowed(i, j)) continue;
            const double v = work.at(i, j);
            max_abs_cost = std::max(max_abs_cost, std::abs(v));
            if (all_integer && std::abs(v - std::round(v)) > 1e-9) {
                all_integer = false;
            }
        }
    }
    if (!all_integer && max_abs_cost > 0.0) {
        const double scale = 1e6 / max_abs_cost;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (work.allowed(i, j)) {
                    work.at(i, j) = std::round(work.at(i, j) * scale);
                }
            }
        }
    }

    SourceOracle<CostMatrix> oracle(work);
    CompiledDesign design =
        compile_one_to_one(oracle, std::vector<CategoryConstraint>());

    FlowOptions opts;
    opts.return_potentials = false;
    const FlowResult res = solve_min_cost_flow_push_relabel(design.problem, opts);

    if (res.status == "iteration_limit") {
        LAP_THROW("push_relabel: the scaling phases did not settle, so the "
                  "eps-optimality invariant no longer holds");
    }

    std::vector<int> assignment(static_cast<std::size_t>(n), -1);
    const BlockArcRange& block = design.problem.block_arcs[0];
    for (int64_t k = 0; k < block.n_arcs; ++k) {
        if (res.flow[static_cast<std::size_t>(block.first_arc + k)] <= 0) continue;
        const std::pair<int32_t, int32_t>& rc =
            block.rc[static_cast<std::size_t>(k)];
        assignment[static_cast<std::size_t>(rc.first)] =
            static_cast<int>(rc.second);
    }

    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        const int j = assignment[static_cast<std::size_t>(i)];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        const double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
