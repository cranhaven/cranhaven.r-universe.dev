// src/solvers/solve_auction.cpp
// Pure C++ Auction LAP solver - NO Rcpp dependencies
//
// All public variants (basic, Gauss-Seidel, scaled) run one shared
// epsilon-scaling forward-auction core. The bidding ends on an assignment that
// is epsilon-optimal under the final prices, within n*eps of the optimum, and
// on real-valued costs no fixed final epsilon closes that gap: two assignments
// can differ by less than any n*eps. The core therefore hands its assignment
// and prices to repair_eps_optimal() (core/lap_eps_repair.h), which corrects
// the prices into exact column potentials and cancels any cheaper reassignment
// it finds on the way. Epsilon-scaling supplies prices close enough that the
// correction is short; the repair supplies the optimality.
//
// Rectangular problems (n < m) are padded to a square graph with dummy rows.
// The padding matters for correctness, not just balance: with warm-started
// prices carried across phases, a person facing no competition for its object
// would otherwise flee it when the matching is rebuilt at the next (smaller)
// epsilon, so the auction oscillates instead of converging. Dummy rows supply
// that competition.
//
// The variants differ only in bidding discipline and exposed parameters:
//   - solve_auction        : queue drain, default schedule
//   - solve_auction_gs     : Gauss-Seidel sweep, default schedule
//   - solve_auction_scaled : queue drain with a caller-chosen alpha schedule
//   - solve_csa            : Goldberg-Kennedy CSA-Q, queue drain with alpha 10
//                            and the fourth-best row search (solve_csa.cpp)
//
// The dense (CostMatrix) and lazy (LazyCostMatrix) entry points share one
// templated bidding loop, auction_core_impl<CostSourceT> in solve_auction_core.h. Dense padding
// still materializes a padded CostMatrix copy exactly as before (unchanged
// behavior/performance); lazy padding uses PaddedCostView to fake the extra
// dummy rows without copying the underlying feature data.

#include "solve_auction_core.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

using auction_detail::auction_core_lazy;
using auction_detail::run_auction_core;
using auction_detail::terminal_epsilon_options;

// Shared epsilon-scaling core (dense CostMatrix); see EpsilonScalingOptions.
// Returns a LapResult over the ORIGINAL (unpadded) rows.
static LapResult auction_core(const CostMatrix& cost, bool maximize,
                              EpsilonScalingOptions options,
                              EpsilonScalingStats* stats = nullptr) {
    const int n0 = static_cast<int>(cost.nrow);
    const int m0 = static_cast<int>(cost.ncol);

    if (n0 == 0) return LapResult({}, 0.0, "optimal");
    lap::require_rows_fit_cols(n0, m0);
    if (options.alpha <= 1.0) options.alpha = 7.0;

    // Pad rectangular problems to square with dummy rows (see file header).
    const bool needs_padding = (n0 < m0);
    int n = n0, m = m0;
    CostMatrix padded;

    if (needs_padding) {
        n = m0;
        padded = CostMatrix(n, m);
        double dummy_cost = 0.0;
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                padded.at(i, j) = cost.at(i, j);
                padded.mask[static_cast<size_t>(lap::flat_index(i, j, m))] =
                    cost.mask[static_cast<size_t>(lap::flat_index(i, j, m))];
                if (cost.allowed(i, j) && std::isfinite(cost.at(i, j))) {
                    dummy_cost = std::max(dummy_cost, std::abs(cost.at(i, j)));
                }
            }
        }
        // Dummy rows: high cost (minimize) / very low profit (maximize), allowed
        // everywhere so they absorb the surplus columns.
        dummy_cost = (dummy_cost + 1.0) * m * 10.0;
        for (int i = n0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                padded.at(i, j) = maximize ? -dummy_cost : dummy_cost;
                padded.mask[static_cast<size_t>(lap::flat_index(i, j, m))] = 1;  // allowed
            }
        }
    }
    const CostMatrix& base = needs_padding ? padded : cost;

    // Prepare working costs (negated if maximize, forbidden excluded via mask).
    CostMatrix work = prepare_for_solve(base, maximize);

    auto core = run_auction_core(work, cost, options);
    if (stats != nullptr) *stats = EpsilonScalingStats{core.iter, core.row_scans};

    // Verify the ORIGINAL rows and total on the ORIGINAL costs.
    std::vector<int> assignment(n0, -1);
    double total = 0.0;
    for (int i = 0; i < n0; ++i) {
        int j = core.a_of_i[i];
        if (j < 0) LAP_THROW_INFEASIBLE("Could not find full matching");
        if (!cost.allowed(i, j)) LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        double c = cost.at(i, j);
        if (!std::isfinite(c)) LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        assignment[i] = j;
        total += c;
    }
    return LapResult(std::move(assignment), total, "optimal");
}


LapResult solve_epsilon_scaling(const CostMatrix& cost, bool maximize,
                                const EpsilonScalingOptions& options,
                                EpsilonScalingStats* stats) {
    return auction_core(cost, maximize, options, stats);
}

// Basic auction algorithm (queue drain, epsilon-scaled)
LapResult solve_auction(const CostMatrix& cost, bool maximize, double eps_in) {
    return auction_core(cost, maximize, terminal_epsilon_options(eps_in));
}

// Gauss-Seidel auction algorithm (sweep discipline, epsilon-scaled)
LapResult solve_auction_gs(const CostMatrix& cost, bool maximize, double eps_in,
                           long long* out_bids) {
    EpsilonScalingOptions options = terminal_epsilon_options(eps_in);
    options.gauss_seidel = true;
    EpsilonScalingStats stats;
    LapResult result = auction_core(cost, maximize, options, &stats);
    if (out_bids != nullptr) *out_bids = stats.bids;
    return result;
}

// Scaled-epsilon auction with custom parameters (queue drain)
LapResult solve_auction_scaled_params(const CostMatrix& cost, bool maximize,
                                       double initial_epsilon_factor,
                                       double alpha,
                                       double final_epsilon) {
    EpsilonScalingOptions options;
    options.initial_epsilon_factor = initial_epsilon_factor;
    options.alpha = alpha;
    options.final_epsilon = final_epsilon;
    return auction_core(cost, maximize, options);
}

// Scaled-epsilon auction. Maps the named schedule to a numeric alpha and
// forwards to solve_auction_scaled_params with default epsilon bounds.
LapResult solve_auction_scaled(const CostMatrix& cost, bool maximize,
                                const std::string& schedule) {
    double alpha = 7.0;
    if (schedule == "pow2" || schedule == "halves") {
        alpha = 4.0;
    }
    return solve_auction_scaled_params(cost, maximize,
                                       /*initial_epsilon_factor=*/1.0,
                                       alpha,
                                       /*final_epsilon=*/-1.0);
}

// Lazy cost-source overload: basic auction (queue drain, epsilon-scaled).
// `maximize` is baked into `cost` at construction; `eps_in` becomes the
// terminal epsilon, matching solve_auction()'s dense contract.
LapResult solve_auction(const LazyCostMatrix& cost, double eps_in) {
    return auction_core_lazy(cost, terminal_epsilon_options(eps_in));
}

}  // namespace lap
