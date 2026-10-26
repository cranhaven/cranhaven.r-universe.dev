// src/solvers/solve_csflow.cpp
// Pure C++ Cost-Scaling Flow LAP solver - NO Rcpp dependencies
//
// The network is the flow model's one-to-one design and the search is
// solve_min_cost_flow(). What belongs to this solver is what always did: how
// the cost matrix is prepared, the relaxation predicate its answers were
// produced under, and what it calls a shortfall.

#include "solve_csflow.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../flow/flow_assign.h"
#include <cmath>
#include <string>
#include <vector>

namespace lap {

LapResult solve_csflow(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    // A path relaxes on a strict improvement of more than 1e-18, which is the
    // predicate every answer this solver has returned was chosen under.
    FlowOptions opts;
    opts.relax_eps = 1e-18;
    opts.return_potentials = false;

    SourceOracle<CostMatrix> oracle(work);
    const AssignmentFlow flow = solve_assignment_flow(oracle, opts);

    if (flow.n_matched < n) {
        LAP_THROW_INFEASIBLE(std::string("Only matched ") +
                             std::to_string(flow.n_matched) +
                             " out of " + std::to_string(n) + " rows");
    }

    std::vector<int> assignment = flow.match;

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
