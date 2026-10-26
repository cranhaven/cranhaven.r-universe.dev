// src/solvers/solve_auction.h
// Pure C++ Auction LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include "../core/lap_lazy_types.h"
#include <string>

namespace lap {

// How a bidder finds its cheapest and second-cheapest columns.
//   FullScan   : scan the row's adjacency list on every bid.
//   FourthBest : Goldberg-Kennedy's k-th best heuristic with k = 4. A scan
//                keeps the row's three cheapest arcs and the fourth-smallest
//                partial reduced cost K. Prices only fall, so an arc left out
//                stays at or above K, and while two kept arcs are still at or
//                below K they are the row's two cheapest.
enum class RowSearch { FullScan, FourthBest };

// Parameters of the epsilon-scaling core shared by the auction solvers and
// the cost-scaling assignment solver.
struct EpsilonScalingOptions {
    double initial_epsilon_factor = 1.0;  // multiplies the starting epsilon
    double alpha = 7.0;                   // epsilon divisor per phase (> 1)
    double final_epsilon = -1.0;          // <= 0 reads it off the costs
    bool gauss_seidel = false;            // false: LIFO stack of active rows
    RowSearch row_search = RowSearch::FullScan;
};

// Work counters of one solve.
struct EpsilonScalingStats {
    long long bids = 0;       // bids (double-push operations), all phases
    long long row_scans = 0;  // full scans of a row's adjacency list
};

// Solve LAP with the epsilon-scaling core under explicit options.
// Throws as the auction solvers below.
LapResult solve_epsilon_scaling(const CostMatrix& cost, bool maximize,
                                const EpsilonScalingOptions& options,
                                EpsilonScalingStats* stats = nullptr);

// Solve LAP using basic auction algorithm
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   eps: Epsilon parameter for bidding. If <= 0, uses adaptive epsilon based on cost spread
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction(const CostMatrix& cost, bool maximize = false, double eps = -1.0);

// Solve LAP using scaled-epsilon auction algorithm
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   schedule: Scaling schedule - "alpha7" (default), "pow2", "halves"
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction_scaled(const CostMatrix& cost, bool maximize = false,
                                const std::string& schedule = "alpha7");

// Solve LAP using Gauss-Seidel auction algorithm
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   eps: Epsilon parameter for bidding. If <= 0, uses adaptive epsilon based on cost spread
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol (auto-transposes)
//   ConvergenceException if iteration limit exceeded
// out_bids (optional): receives the cumulative number of bids across all
// epsilon-scaling phases, a convergence diagnostic exposed by the R interface.
LapResult solve_auction_gs(const CostMatrix& cost, bool maximize = false, double eps = -1.0,
                           long long* out_bids = nullptr);

// Solve LAP using scaled-epsilon auction with custom parameters
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   initial_epsilon_factor: Multiplier for initial epsilon (default 1.0)
//   alpha: Epsilon reduction factor each phase (default 7.0)
//   final_epsilon: Stopping epsilon (if <= 0, a hundredth of the typical gap between a
//                  row's cheapest distinct costs; see epsilon_schedule())
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction_scaled_params(const CostMatrix& cost, bool maximize = false,
                                       double initial_epsilon_factor = 1.0,
                                       double alpha = 7.0,
                                       double final_epsilon = -1.0);

// Lazy cost-source overload of the basic (queue-drain) auction. `maximize`
// is already baked into `cost`'s internal negate flag at construction, so
// there is no separate parameter here (matching solve_jv(LazyCostMatrix)'s
// convention). Handles rectangular problems via PaddedCostView, without
// materializing a dense padded copy.
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction(const LazyCostMatrix& cost, double eps = -1.0);

}  // namespace lap
