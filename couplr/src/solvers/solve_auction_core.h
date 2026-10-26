// src/solvers/solve_auction_core.h
// The epsilon-scaling bidding core the auction solvers share, templated on the
// cost source, for a translation unit that runs it on a source of its own.
// solve_auction.cpp holds the entry points for CostMatrix and LazyCostMatrix;
// see its header comment for the method.
#pragma once

#include "solve_auction.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../core/lap_cost_view.h"
#include "../core/lap_eps_repair.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {
namespace auction_detail {

// A bid ends its phase within eps of the best price; the terminal eps is set to
// this fraction of the typical spacing among a row's cheapest distinct costs.
inline constexpr double EPS_FINAL_OF_ROW_GAP = 1e-2;
inline constexpr int ROW_GAP_VALUES = 4;

struct EpsilonSchedule {
    double max_abs_cost;  // over every row, dummy rows included
    double start;
    double terminal;
};

// Start and terminal epsilon of the bidding, both read off the costs, so that
// multiplying every cost by a constant or adding one to all of them leaves the
// bids unchanged.
//
// The start is the span of the real rows' costs. The end is a fraction of the
// median, over real rows, of the mean gap between a row's four smallest
// distinct costs. Bidding stops on prices within eps of exact potentials, and
// what the repair then has to correct grows with how many of a row's options
// sit within eps of the one it holds. That count is set by how closely costs
// crowd the bottom of a row, not by their range: a heavy-tailed row spans
// orders of magnitude more than the gaps between its cheapest entries. Dummy
// rows padding a rectangular problem are constant along the row and are left
// out of both.
//
// The end is floored at four ulps of the largest cost magnitude. A bid forms a
// reduced cost at that magnitude, a dummy row's included, and an eps below its
// spacing is lost in the rounding: the bidder cannot tell columns apart and
// prices fall in steps the reduced costs do not register.
template <typename CostSourceT>
EpsilonSchedule epsilon_schedule(const CostSourceT& work, int real_rows,
                                        const std::vector<int64_t>& row_ptr,
                                        const std::vector<int>& cols,
                                        double initial_epsilon_factor) {
    const int n = static_cast<int>(work.nrow);
    double max_abs_cost = 0.0;
    double lo = std::numeric_limits<double>::infinity();
    double hi = -std::numeric_limits<double>::infinity();
    std::vector<double> row_gaps;
    row_gaps.reserve(static_cast<size_t>(real_rows));

    for (int i = 0; i < n; ++i) {
        const bool real = i < real_rows;
        double smallest[ROW_GAP_VALUES];
        int held = 0;
        for (int64_t k = row_ptr[i]; k < row_ptr[i + 1]; ++k) {
            const double c = work.at(i, cols[k]);
            if (!std::isfinite(c)) continue;
            if (std::abs(c) > max_abs_cost) max_abs_cost = std::abs(c);
            if (!real) continue;
            if (c < lo) lo = c;
            if (c > hi) hi = c;

            bool seen = false;
            for (int q = 0; q < held && !seen; ++q) seen = (smallest[q] == c);
            if (seen || (held == ROW_GAP_VALUES && c >= smallest[held - 1])) continue;
            int p = (held < ROW_GAP_VALUES) ? held++ : ROW_GAP_VALUES - 1;
            while (p > 0 && smallest[p - 1] > c) {
                smallest[p] = smallest[p - 1];
                --p;
            }
            smallest[p] = c;
        }
        if (real && held >= 2) {
            row_gaps.push_back((smallest[held - 1] - smallest[0]) / (held - 1));
        }
    }

    const double span = (hi > lo) ? hi - lo : 1.0;
    double row_gap = span;
    if (!row_gaps.empty()) {
        auto mid = row_gaps.begin() + static_cast<std::ptrdiff_t>(row_gaps.size() / 2);
        std::nth_element(row_gaps.begin(), mid, row_gaps.end());
        row_gap = *mid;
    }

    const double ulp = std::nextafter(max_abs_cost, std::numeric_limits<double>::infinity())
                       - max_abs_cost;
    const double eps_final = std::max(row_gap * EPS_FINAL_OF_ROW_GAP, 4.0 * ulp);
    const double eps_start = std::max(span * initial_epsilon_factor, eps_final);
    return EpsilonSchedule{max_abs_cost, eps_start, eps_final};
}

struct AuctionCoreResult {
    std::vector<int> a_of_i;  // padded size n: person -> object (0-based)
    long long iter;
    long long row_scans;
};

// Number of arcs whose partial reduced cost the fourth-best heuristic ranks
// on a scan; it keeps all but the last.
inline constexpr int KTH_BEST = 4;

// Shared epsilon-scaling bidding loop. `work` must already be
// square, padded, and "prepared" (forbidden reads as BIG via at(), negated
// if the caller wants maximize) -- CostMatrix via prepare_for_solve(), or a
// LazyCostMatrix/PaddedCostView<LazyCostMatrix> which bake that in at
// construction. Returns the assignment over ALL padded rows; callers extract
// the real (unpadded) rows and verify/report using their own original-cost
// object.
//
// A bid is Goldberg and Kennedy's double-push with implicit row prices: the
// row takes its cheapest column w, displacing w's holder onto the stack, and
// w's price drops to the row's second-cheapest reduced cost less eps. Prices
// never rise, within a phase or across phases, which is what lets the
// fourth-best search keep a row's cached arcs between its bids.
template <typename CostSourceT>
AuctionCoreResult auction_core_impl(const CostSourceT& work, int real_rows,
                                           const EpsilonScalingOptions& options) {
    const int n = static_cast<int>(work.nrow);
    const int m = static_cast<int>(work.ncol);
    const double inf = std::numeric_limits<double>::infinity();

    ensure_each_row_has_option(work);
    std::vector<int64_t> row_ptr;
    std::vector<int> cols;
    build_allowed(work, row_ptr, cols);

    const EpsilonSchedule schedule = epsilon_schedule(work, real_rows, row_ptr, cols,
                                                      options.initial_epsilon_factor);
    const double max_abs_cost = schedule.max_abs_cost;
    const double alpha = options.alpha;
    double epsilon = schedule.start;
    const double eps_final =
        (options.final_epsilon > 0.0) ? options.final_epsilon : schedule.terminal;
    const double price_bound = std::max(1e12, max_abs_cost * n * 1000.0);

    std::vector<double> price(m, 0.0);
    std::vector<int> a_of_i(n, -1), i_of_j(m, -1);
    long long row_scans = 0;

    // Minimize reduced cost (cost - price); the bidder decreases the winning
    // object's price so contenders see it as more expensive.
    auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        ++row_scans;
        best_rc = inf;
        second_rc = inf;
        best_j = -1;
        for (int64_t k = row_ptr[i]; k < row_ptr[i + 1]; ++k) {
            int j = cols[k];
            double rc = work.at(i, j) - price[j];
            if (rc < best_rc) { second_rc = best_rc; best_rc = rc; best_j = j; }
            else if (rc < second_rc) { second_rc = rc; }
        }
    };

    // Fourth-best cache: per row, the adjacency positions of up to
    // KTH_BEST - 1 arcs in ascending order of reduced cost at the last scan,
    // and the KTH_BEST-th smallest reduced cost then (inf when the row has no
    // more arcs than it keeps, so the cache holds the whole row).
    const bool fourth_best = options.row_search == RowSearch::FourthBest;
    std::vector<int64_t> kept_arc(fourth_best ? static_cast<size_t>(n) * (KTH_BEST - 1) : 0);
    std::vector<int> kept_count(fourth_best ? n : 0, 0);
    std::vector<double> kth_rc(fourth_best ? n : 0, inf);

    auto scan_keep = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        ++row_scans;
        double rank_rc[KTH_BEST];
        int64_t rank_arc[KTH_BEST];
        int held = 0;
        for (int64_t k = row_ptr[i]; k < row_ptr[i + 1]; ++k) {
            const double rc = work.at(i, cols[k]) - price[cols[k]];
            if (held == KTH_BEST && rc >= rank_rc[KTH_BEST - 1]) continue;
            int p = (held < KTH_BEST) ? held++ : KTH_BEST - 1;
            while (p > 0 && rank_rc[p - 1] > rc) {
                rank_rc[p] = rank_rc[p - 1];
                rank_arc[p] = rank_arc[p - 1];
                --p;
            }
            rank_rc[p] = rc;
            rank_arc[p] = k;
        }
        const int keep = std::min(held, KTH_BEST - 1);
        const size_t base = static_cast<size_t>(i) * (KTH_BEST - 1);
        for (int q = 0; q < keep; ++q) kept_arc[base + q] = rank_arc[q];
        kept_count[i] = keep;
        kth_rc[i] = (held == KTH_BEST) ? rank_rc[KTH_BEST - 1] : inf;

        best_rc = rank_rc[0];
        best_j = cols[rank_arc[0]];
        second_rc = (held >= 2) ? rank_rc[1] : inf;
    };

    auto find_best_kept = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        const int keep = kept_count[i];
        if (keep > 0) {
            const size_t base = static_cast<size_t>(i) * (KTH_BEST - 1);
            const double bound = kth_rc[i];
            best_rc = inf;
            second_rc = inf;
            int64_t best_k = -1;
            int at_or_below = 0;
            for (int q = 0; q < keep; ++q) {
                const int64_t k = kept_arc[base + q];
                const double rc = work.at(i, cols[k]) - price[cols[k]];
                if (rc <= bound) ++at_or_below;
                if (rc < best_rc || (rc == best_rc && k < best_k)) {
                    second_rc = best_rc;
                    best_rc = rc;
                    best_k = k;
                } else if (rc < second_rc) {
                    second_rc = rc;
                }
            }
            if (at_or_below >= 2 || bound == inf) {
                best_j = cols[best_k];
                return;
            }
        }
        scan_keep(i, best_rc, second_rc, best_j);
    };

    double eps_cur = eps_final;
    const long long max_iter = static_cast<long long>(n) * m * 200 + 1000;
    long long iter = 0;

    auto bid_person = [&](int i) -> int {
        double best_rc, second_rc;
        int best_j;
        if (fourth_best) find_best_kept(i, best_rc, second_rc, best_j);
        else find_best(i, best_rc, second_rc, best_j);
        if (best_j < 0) LAP_THROW_INFEASIBLE("Person has no valid objects");

        double gamma;
        if (!std::isfinite(second_rc)) {
            gamma = eps_cur;  // only one option
        } else {
            gamma = second_rc - best_rc;
            if (gamma > price_bound) gamma = price_bound;
            if (gamma < 0.0) gamma = 0.0;
        }
        double new_price = price[best_j] - (gamma + eps_cur);
        if (new_price < -price_bound) new_price = -price_bound;
        price[best_j] = new_price;

        int old = i_of_j[best_j];
        i_of_j[best_j] = i;
        a_of_i[i] = best_j;
        if (old != -1 && old != i) a_of_i[old] = -1;
        return old;
    };

    for (;;) {
        epsilon = std::max(epsilon / alpha, eps_final);
        eps_cur = epsilon;

        std::fill(a_of_i.begin(), a_of_i.end(), -1);
        std::fill(i_of_j.begin(), i_of_j.end(), -1);

        if (!options.gauss_seidel) {
            std::vector<int> queue;
            queue.reserve(n);
            for (int i = 0; i < n; ++i) queue.push_back(i);
            while (!queue.empty()) {
                int i = queue.back();
                queue.pop_back();
                int old = bid_person(i);
                if (old != -1) queue.push_back(old);
                if (++iter > max_iter)
                    LAP_THROW_CONVERGENCE("Auction: iteration guard exceeded");
            }
        } else {
            bool converged = false;
            while (!converged) {
                converged = true;
                for (int i = 0; i < n; ++i) {
                    if (a_of_i[i] >= 0 && i_of_j[a_of_i[i]] == i) continue;
                    converged = false;
                    bid_person(i);
                    if (++iter > max_iter)
                        LAP_THROW_CONVERGENCE("Auction (Gauss-Seidel): iteration guard exceeded");
                }
            }
        }

        if (epsilon <= eps_final) break;
    }

    detail::repair_eps_optimal(work,row_ptr, cols, i_of_j, a_of_i, price);

    return AuctionCoreResult{std::move(a_of_i), iter, row_scans};
}

// Run the bidding core and name a guard trip correctly.
//
// The iteration guard fires on two different inputs: one whose allowed edges
// admit no perfect matching, where rows displace each other forever over a
// column set too small for all of them, and one that is feasible and merely
// slow. ensure_each_row_has_option() does not separate them -- every row can
// have an option and Hall's condition still fail, which is what
// [[1, Inf], [1, Inf]] is -- so the matching is checked here, on the path
// that already failed. A run that converged pays nothing for this.
template <typename CostSourceT, typename OriginalT>
AuctionCoreResult run_auction_core(const CostSourceT& work,
                                          const OriginalT& original,
                                          const EpsilonScalingOptions& options) {
    try {
        return auction_core_impl(work, static_cast<int>(original.nrow), options);
    } catch (const ConvergenceException&) {
        if (!has_valid_matching_view(original)) {
            LAP_THROW_INFEASIBLE("Could not find full matching: the allowed "
                                 "edges admit no complete assignment");
        }
        throw;
    }
}

// Prepared cost-source variant. `cost` is already "prepared" (negate/caliper/
// max_distance baked in at construction) -- no prepare_for_solve() step.
template <typename Source>
LapResult auction_core_lazy(const Source& cost, EpsilonScalingOptions options) {
    const int64_t n0 = cost.nrow;
    const int64_t m0 = cost.ncol;

    if (n0 == 0) return LapResult({}, 0.0, "optimal");
    lap::require_rows_fit_cols(n0, m0);
    if (options.alpha <= 1.0) options.alpha = 7.0;

    const bool needs_padding = (n0 < m0);

    if (!needs_padding) {
        auto core = run_auction_core(cost, cost, options);

        std::vector<int> assignment(static_cast<size_t>(n0), -1);
        double total = 0.0;
        for (int64_t i = 0; i < n0; ++i) {
            int j = core.a_of_i[static_cast<size_t>(i)];
            if (j < 0) LAP_THROW_INFEASIBLE("Could not find full matching");
            if (!cost.allowed(i, j)) LAP_THROW_INFEASIBLE("Chosen forbidden edge");
            double c = cost.at(i, j);
            if (cost.is_negated()) c = -c;
            if (!std::isfinite(c)) LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
            assignment[static_cast<size_t>(i)] = j;
            total += c;
        }
        return LapResult(std::move(assignment), total, "optimal");
    }

    // Rectangular: find the dummy cost (largest allowed |cost|) via one pass
    // over the real rows -- mirrors the dense padding loop's dummy_cost scan,
    // expressed via cost_if_allowed() instead of raw array access.
    double dummy_cost = 0.0;
    for (int64_t i = 0; i < n0; ++i) {
        for (int64_t j = 0; j < m0; ++j) {
            double c = 0.0;
            if (!cost_if_allowed(cost, i, j, c)) continue;
            if (std::isfinite(c)) dummy_cost = std::max(dummy_cost, std::abs(c));
        }
    }
    dummy_cost = (dummy_cost + 1.0) * static_cast<double>(m0) * 10.0;
    // cost.at() already negates for maximize; the dummy rows must match sign
    // convention (very expensive to route flow through, in whichever
    // direction "expensive" means under the source's own negation).
    if (cost.is_negated()) dummy_cost = -dummy_cost;

    PaddedCostView<Source> padded(cost, n0, dummy_cost);
    auto core = run_auction_core(padded, cost, options);

    std::vector<int> assignment(static_cast<size_t>(n0), -1);
    double total = 0.0;
    for (int64_t i = 0; i < n0; ++i) {
        int j = core.a_of_i[static_cast<size_t>(i)];
        if (j < 0) LAP_THROW_INFEASIBLE("Could not find full matching");
        if (!cost.allowed(i, j)) LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        double c = cost.at(i, j);
        if (cost.is_negated()) c = -c;
        if (!std::isfinite(c)) LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        assignment[static_cast<size_t>(i)] = j;
        total += c;
    }
    return LapResult(std::move(assignment), total, "optimal");
}

// An explicit epsilon becomes the terminal epsilon (the requested precision).
inline EpsilonScalingOptions terminal_epsilon_options(double eps_in) {
    EpsilonScalingOptions options;
    if (std::isfinite(eps_in) && eps_in > 0.0) options.final_epsilon = eps_in;
    return options;
}

}  // namespace auction_detail
}  // namespace lap
