// src/solvers/solve_ssp.cpp
// Pure C++ Successive Shortest Path LAP solver - NO Rcpp dependencies
//
// The network is the flow model's one-to-one design and the search is
// solve_min_cost_flow(), which is successive shortest paths already. What
// belongs to this solver is the orientation it works in, the relaxation
// predicate its answers were produced under, and what it calls a shortfall.

#include "solve_ssp.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../flow/flow_assign.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace lap {

LapResult solve_ssp(const CostMatrix& cost, bool maximize) {
    const int n0 = static_cast<int>(cost.nrow);
    const int m0 = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Auto-transpose if n > m
    bool transposed = false;
    CostMatrix work = cost;
    int n = n0, m = m0;

    if (n0 > m0) {
        transposed = true;
        // Transpose the matrix
        work = CostMatrix(m0, n0);
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                work.at(j, i) = cost.at(i, j);
                work.mask[static_cast<size_t>(flat_index(j, i, n0))] =
                    cost.mask[static_cast<size_t>(flat_index(i, j, m0))];
            }
        }
        n = static_cast<int>(work.nrow);  // now m0
        m = static_cast<int>(work.ncol);  // now n0
    }

    // Prepare working costs (negated if maximize)
    CostMatrix work_costs = prepare_for_solve(work, maximize);

    // Check feasibility
    ensure_each_row_has_option(work_costs.mask, n, m);

    // A path relaxes on any strict improvement, with no slack. That is a
    // different predicate from the one csflow uses, and it selects a different
    // shortest path where two are equally cheap, so it stays with this solver.
    FlowOptions opts;
    opts.relax_eps = 0.0;
    opts.return_potentials = false;

    SourceOracle<CostMatrix> oracle(work_costs);
    const AssignmentFlow flow = solve_assignment_flow(oracle, opts);

    if (flow.n_matched < n) {
        LAP_THROW_INFEASIBLE("Could not send full flow");
    }

    const std::vector<int>& match_work = flow.match;

    // Verify matching and compute total cost using ORIGINAL costs (in work orientation)
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = match_work[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!work.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = work.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    // Map back to original orientation if we transposed
    std::vector<int> assignment;
    if (!transposed) {
        // No transpose: direct mapping (0-based)
        assignment = match_work;
    } else {
        // Transposed: work is m0 x n0; match_work length m0
        // match_work[i] (orig col i) -> matched to j (orig row j)
        assignment.assign(n0, -1);
        for (int i = 0; i < m0; ++i) {
            int j = match_work[i];
            if (j >= 0) {
                assignment[j] = i;  // original row j -> original col i
            }
        }
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
