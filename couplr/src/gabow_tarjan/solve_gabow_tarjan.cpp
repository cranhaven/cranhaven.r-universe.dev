// solve_gabow_tarjan.cpp
// Gabow-Tarjan LAP solver implementation with R interface

#include <Rcpp.h>
#include <cmath>                // for std::round, std::llround, std::fabs
#include <string>
#include <algorithm>
#include <limits>
#include "utils_gabow_tarjan.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

namespace {

void compute_duals_from_matching(const CostMatrix& cost,
                                 const MatchVec& row_match,
                                 DualVec& y_u,
                                 DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    constexpr long long INF = std::numeric_limits<long long>::max() / 4;

    y_u.assign(n, 0);
    y_v.assign(m, 0);

    if (n == 0 || m == 0) return;

    std::vector<int> matched_col_for_row(n, NIL);
    std::vector<int> row_for_col(m, NIL);
    for (int i = 0; i < n; ++i) {
        int j = row_match[i];
        if (j >= 0 && j < m && cost[i][j] < BIG_INT) {
            matched_col_for_row[i] = j;
            row_for_col[j] = i;
        }
    }

    // Difference constraints for matched columns under 1-feasibility:
    // u[i] - u[k] <= c(i, match[k]) + 1 - c(k, match[k]).
    // Bellman-Ford from a zero super-source gives feasible row potentials.
    std::vector<long long> u(n, 0);
    for (int iter = 0; iter < n - 1; ++iter) {
        bool changed = false;
        for (int k = 0; k < n; ++k) {
            int jk = matched_col_for_row[k];
            if (jk == NIL || cost[k][jk] >= BIG_INT) {
                continue;
            }
            for (int i = 0; i < n; ++i) {
                if (cost[i][jk] >= BIG_INT) {
                    continue;
                }
                long long w = cost[i][jk] + 1 - cost[k][jk];
                if (u[i] > u[k] + w) {
                    u[i] = u[k] + w;
                    changed = true;
                }
            }
        }
        if (!changed) break;
    }

    y_u = u;

    for (int j = 0; j < m; ++j) {
        int i = row_for_col[j];
        if (i != NIL) {
            y_v[j] = cost[i][j] - y_u[i];
        } else {
            long long best = INF;
            for (int r = 0; r < n; ++r) {
                if (cost[r][j] < BIG_INT) {
                    best = std::min(best, cost[r][j] + 1 - y_u[r]);
                }
            }
            y_v[j] = (best < INF) ? best : 0;
        }
    }
}

} // namespace

/**
 * Gabow-Tarjan LAP solver implementation
 * 
 * Solves the linear assignment problem using the Gabow-Tarjan bit-scaling
 * algorithm. Worst-case complexity is O(n^{3/4} * m * log(n*C)) per the
 * 1989 paper, where C is the maximum cost and m is the number of finite
 * edges. For dense graphs this is sub-cubic in n.
 * 
 * @param cost Cost matrix (NumericMatrix from R)
 * @param maximize If true, solve maximum weight matching instead
 * @return R List with assignment, cost, row_duals, col_duals
 */
Rcpp::List solve_gabow_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize) {
    const int n = cost.nrow();
    const int m = cost.ncol();
    
    // Convert R matrix to C++ cost matrix with integer costs
    CostMatrix cost_matrix(n, std::vector<long long>(m));

    // Maximization is minimization of the negated costs. The negation is exact
    // in floating point, so it happens before anything is measured and comes
    // back out of the duals at the end.
    const double sign = maximize ? -1.0 : 1.0;

    // Range of the costs the solver will see, and whether they are all (near)
    // integers. A forbidden pair carries the sentinel rather than a cost and
    // takes part in neither.
    bool all_integer = true;
    bool any_finite = false;
    double lo = 0.0;
    double hi = 0.0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double val = cost(i, j);
            if (!R_finite(val)) continue;
            val *= sign;

            if (!any_finite) {
                lo = hi = val;
                any_finite = true;
            } else {
                if (val < lo) lo = val;
                if (val > hi) hi = val;
            }

            // Check if val is almost an integer
            double rounded = std::round(val);
            if (std::fabs(val - rounded) > 1e-9) {
                all_integer = false;
            }
        }
    }

    // K is the multiplier that separates the optimum from any 1-optimal
    // matching: the instance's own size plus one on a square problem, and twice
    // the short side plus one on a rectangular one, where the dummy side is one
    // row of the capacity of the padding.
    const int short_side = (n < m) ? n : m;
    const long long multiplier =
        (n == m) ? (static_cast<long long>(n) + 1)
                 : (2LL * static_cast<long long>(short_side) + 1);

    // Costs enter the solver shifted to start at zero. Every matching the
    // solver may return covers the same number of pairs, min(n, m) real ones
    // with the dummy row holding the remaining columns for nothing, so one
    // constant subtracted from every cost moves every candidate by the same
    // amount and leaves the optimum where it was. Shifting here rather than
    // relying on the inner shift, which only pulls costs up to zero, means the
    // fixed-point scale below has the whole span of the costs to work with
    // instead of their distance from the origin.
    double shift = any_finite ? lo : 0.0;
    if (all_integer) shift = std::round(shift);
    const double range = any_finite ? (hi - lo) : 0.0;

    // The inner routine forms K * (c - min_c) in long long and reads any value
    // at or above BIG_INT (1e15) as a forbidden edge. It then works with
    // reduced costs c - y_u - y_v and with sums of those along augmenting
    // paths, which stay within a small multiple of that product. The integer
    // costs handed to it therefore have to satisfy
    //
    //     K * (c_hi - c_lo) <= BIG_INT / 8,
    //
    // which holds every scaled cost clear of the sentinel and leaves the
    // multiple and the path sums inside 64-bit: at the limit an instance of
    // 10^4 units sums to 4e18 against an LLONG_MAX of 9.2e18. Costs that get
    // scaled are placed an order further in, so the conversion never lands on
    // the boundary; costs used as they are have to meet the bound themselves.
    constexpr long long REPRESENTABLE_PRODUCT = BIG_INT / 8;
    constexpr long long SCALED_PRODUCT = BIG_INT / 100;
    const double mult_d = static_cast<double>(multiplier);
    const double representable_span =
        static_cast<double>(REPRESENTABLE_PRODUCT) / mult_d;
    const double scaled_span = static_cast<double>(SCALED_PRODUCT) / mult_d;

    // Scaling factor for floating-point → integer
    double scale_factor = 1.0;

    if (all_integer) {
        // Integer costs go in as they are, so the bound is a property of the
        // input and no scale can arrange it.
        if (range > representable_span) {
            Rcpp::stop("gabow_tarjan: integer cost range (" +
                       std::to_string(range) + ") exceeds what bit-scaling can "
                       "represent on a " + std::to_string(n) + " x " +
                       std::to_string(m) + " instance (limit " +
                       std::to_string(representable_span) + "); use method = "
                       "'jv' or 'auction'.");
        }
    } else if (range > 0.0) {
        scale_factor = scaled_span / range;
        // The product scale_factor * range is scaled_span by construction; the
        // factor itself is what leaves the double range when the costs are
        // spread over a span far below one.
        constexpr double MAX_SCALE = 1e300;
        if (!(scale_factor <= MAX_SCALE)) scale_factor = MAX_SCALE;
    }

    // Fill integer cost matrix (with rounding, not truncation)
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double val = cost(i, j);
            if (R_finite(val)) {
                // The shift puts the finite costs on [0, range] and the scale
                // holds the product inside the bound above, so llround has a
                // value it can hold. An all-integer matrix keeps the scale at
                // one and the shift at an integer, so its costs stay exact.
                cost_matrix[i][j] =
                    std::llround(scale_factor * (val * sign - shift));
            } else {
                cost_matrix[i][j] = BIG_INT; // forbidden
            }
        }
    }

    // State for inner solver
    MatchVec row_match(n, NIL);
    MatchVec col_match(m, NIL);
    DualVec y_u(n, 0);
    DualVec y_v(m, 0);
    
    // Solve using Gabow–Tarjan bit-scaling algorithm
    solve_gabow_tarjan_inner(cost_matrix, row_match, col_match, y_u, y_v);

    // Warm-started scaling preserves the matching across phases. Recompute the
    // final dual certificate for small/certificate-oriented calls. Large
    // assignment() calls discard duals, so avoid adding an extra cubic pass to
    // the performance path.
    if (n <= 100) {
        compute_duals_from_matching(cost_matrix, row_match, y_u, y_v);
    }
    
    // Convert matching to 1-based R vectors
    Rcpp::IntegerVector row_match_R(n);
    Rcpp::IntegerVector col_match_R(m);
    for (int i = 0; i < n; ++i) {
        row_match_R[i] = (row_match[i] != NIL) ? (row_match[i] + 1) : NA_INTEGER;
    }
    for (int j = 0; j < m; ++j) {
        col_match_R[j] = (col_match[j] != NIL) ? (col_match[j] + 1) : NA_INTEGER;
    }

    // Compute total cost using the centralized helper (THE SINGLE SOURCE OF TRUTH)
    // This ensures consistent cost semantics across all solvers:
    //   - Always uses original unmodified cost matrix
    //   - Works for both minimize and maximize
    //   - Ignores dummy columns automatically
    double total_cost = compute_total_cost(cost, row_match_R);

    // Count matched rows (for diagnostics)
    int n_matched = 0;
    for (int i = 0; i < n; ++i) {
        if (row_match[i] != NIL) {
            ++n_matched;
        }
    }

    // Convert duals back to original scale. The shift returns on the side that
    // holds exactly one pair per unit, which is the short one: adding it there
    // keeps u_i + v_j = c_ij on the pairs where it was tight, and leaves the
    // long side's sign condition, v_j <= 0 on a column no unit was paired with,
    // where the solver put it.
    const bool rows_are_short = (n <= m);
    const double u_shift = rows_are_short ? shift : 0.0;
    const double v_shift = rows_are_short ? 0.0 : shift;
    Rcpp::NumericVector u_R(n);
    Rcpp::NumericVector v_R(m);
    for (int i = 0; i < n; ++i) {
        double val = static_cast<double>(y_u[i]) / scale_factor + u_shift;
        u_R[i] = maximize ? -val : val;
    }
    for (int j = 0; j < m; ++j) {
        double val = static_cast<double>(y_v[j]) / scale_factor + v_shift;
        v_R[j] = maximize ? -val : val;
    }
    
    // Build result list matching the standard lap_solve API
    // Required fields: "match" and "total_cost" (used by assignment() function in R/lap_solve.R:121,134)
    return Rcpp::List::create(
        // Standard API (required by assignment())
        Rcpp::Named("match")      = row_match_R,
        Rcpp::Named("total_cost") = total_cost,

        // Extra fields for diagnostic/debugging
        Rcpp::Named("row_match")  = row_match_R,
        Rcpp::Named("col_match")  = col_match_R,
        Rcpp::Named("row_duals")  = u_R,
        Rcpp::Named("col_duals")  = v_R,
        Rcpp::Named("u")          = u_R,
        Rcpp::Named("v")          = v_R,
        Rcpp::Named("n_matched")  = n_matched,
        Rcpp::Named("method")     = "gabow_tarjan"
    );
}
