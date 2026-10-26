// src/flow/flow_tree_pricing.h
// Pricing the omitted pairs through the ball tree instead of over the grid.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// price_block() answers two things in one O(nrow * ncol) pass: the smallest
// reduced cost each row offers, and the keep_per_row most negative it offers.
// This answers the same two from the same duals, and returns the same
// BlockPricing, by descending the tree and skipping any subtree whose members
// cannot hold the answer.
//
// One bound does both jobs. For a subtree S and a row i,
//
//     min_{j in S} cbar_ij  >=  cost_lo(i, S) - u_i - max_{j in S} v_j
//
// where cost_lo is the ball's bound on the cost. A subtree is entered when
// that bound falls below the larger of the row's best reduced cost so far and
// -tol: below the first because it could improve the minimum, below the second
// because it could hold a violator. Everything else is skipped unread.
//
// The termination test survives the pruning exactly. A subtree skipped at -tol
// has no member pricing below -tol, so n_violators counts every violator the
// grid scan would have counted, and zero still means the restricted answer is
// optimal for the complete problem. The kept set survives it too: the tree
// visits a row's violators in leaf order and price_block() offers them with j
// ascending, so they are sorted back into that order before being offered,
// which is what settles a tie at the keep_per_row boundary the same way.
//
// What does not survive is n_scanned and n_evaluated. Those count work, and
// the work is the point.
#pragma once

#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"
#include "flow_balltree.h"
#include "flow_candidates.h"
#include "flow_pricing.h"
#include "flow_topk.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <utility>
#include <vector>

namespace lap {

namespace detail {

// The smallest reduced cost an admissible column of node `id` could carry, and
// infinite when the node holds no reachable column at all.
inline double node_cbar_lo(const BallTree& tree, const LazyCostMatrix& src,
                           const double* q_whitened, double q_g,
                           const double* q_original, int64_t qi, int32_t id,
                           double ui) {
    const double floor_c =
        node_cost_floor(tree, src, q_whitened, q_g, q_original, qi, id);
    if (!(floor_c < std::numeric_limits<double>::infinity())) {
        return std::numeric_limits<double>::infinity();
    }
    // The cost floor already carries the tree's allowance. The two
    // subtractions that turn it into a reduced cost are their own rounding,
    // and the duals are the solver's numbers rather than the tree's, so the
    // result is stepped down once for each.
    const double less_u = next_down(floor_c - ui);
    return next_down(less_u - tree.max_v[static_cast<std::size_t>(id)]);
}

}  // namespace detail

// Price every pair the candidate set omits, against duals `u` and `v`, through
// the tree. `tree` is taken by reference because its per-node largest column
// dual is what moved since the last round and is refreshed here.
template <class Accept = AcceptAll>
BlockPricing price_tree(const LazyCostMatrix& src, BallTree& tree,
                        const std::vector<double>& u,
                        const std::vector<double>& v,
                        CandidateSet& cand,
                        int keep_per_row,
                        double tol,
                        const Accept& accept = Accept()) {
    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;

    if (cand.nrow() != nrow || cand.ncol() != ncol) {
        LAP_THROW_DIMENSION("price_tree: candidate set is " +
                            std::to_string(cand.nrow()) + " x " +
                            std::to_string(cand.ncol()) + ", source is " +
                            std::to_string(nrow) + " x " + std::to_string(ncol));
    }
    if (static_cast<int64_t>(u.size()) != nrow) {
        LAP_THROW_DIMENSION("price_tree: " + std::to_string(u.size()) +
                            " row duals for " + std::to_string(nrow) + " rows");
    }
    if (static_cast<int64_t>(v.size()) != ncol) {
        LAP_THROW_DIMENSION("price_tree: " + std::to_string(v.size()) +
                            " column duals for " + std::to_string(ncol) + " columns");
    }
    if (tree.empty() || static_cast<int64_t>(tree.n_units) != ncol) {
        LAP_THROW_DIMENSION("price_tree: tree holds " +
                            std::to_string(tree.n_units) + " columns, source has " +
                            std::to_string(ncol));
    }

    constexpr double kInf = std::numeric_limits<double>::infinity();

    BlockPricing out;
    out.row_min.assign(static_cast<std::size_t>(nrow > 0 ? nrow : 0), kInf);
    if (nrow <= 0 || ncol <= 0) return out;

    refresh_max_v(tree, v);

    detail::RowTopK keep(nrow, keep_per_row);
    std::vector<char> is_cand(static_cast<std::size_t>(ncol), 0);
    std::vector<double> q(static_cast<std::size_t>(tree.n_vars), 0.0);
    std::vector<std::pair<double, int32_t>> row_violators;
    std::vector<std::pair<double, int32_t>> stack;

    for (int64_t i = 0; i < nrow; ++i) {
        const int32_t* rb = cand.row_begin(i);
        const int64_t  rn = cand.row_size(i);
        for (int64_t t = 0; t < rn; ++t) {
            is_cand[static_cast<std::size_t>(rb[t])] = 1;
        }

        const double ui = u[static_cast<std::size_t>(i)];
        const double* x = src.left_row(i);
        const double q_g = whiten_point(tree, x, q.data());

        double rmin = kInf;
        int64_t rmin_j = -1;
        row_violators.clear();

        stack.clear();
        const double root_lb =
            detail::node_cbar_lo(tree, src, q.data(), q_g, x, i, 0, ui);
        if (root_lb < kInf) stack.emplace_back(root_lb, 0);

        while (!stack.empty()) {
            const double lb = stack.back().first;
            const int32_t id = stack.back().second;
            stack.pop_back();

            // The row's best has only tightened since this node was pushed, so
            // the bound is re-tested against it here rather than where it was
            // measured.
            const double threshold = rmin > -tol ? rmin : -tol;
            if (lb >= threshold) {
                if (lb < out.proven_floor) out.proven_floor = lb;
                continue;
            }

            if (tree.is_leaf(id)) {
                const int32_t end = tree.hi[static_cast<std::size_t>(id)];
                for (int32_t t = tree.lo[static_cast<std::size_t>(id)]; t < end; ++t) {
                    const int32_t j = tree.perm[static_cast<std::size_t>(t)];
                    if (is_cand[static_cast<std::size_t>(j)]) continue;
                    ++out.n_scanned;
                    double c = 0.0;
                    if (!src.admissible(i, static_cast<int64_t>(j), c)) continue;
                    ++out.n_evaluated;
                    const double cbar = c - ui - v[static_cast<std::size_t>(j)];
                    // The tie goes to the smaller column, which is the one an
                    // ascending scan of the same pairs would have kept.
                    if (cbar < rmin || (cbar == rmin && j < rmin_j)) {
                        rmin = cbar;
                        rmin_j = j;
                    }
                    if (cbar < -tol && accept(i, static_cast<int64_t>(j), c)) {
                        ++out.n_violators;
                        row_violators.emplace_back(cbar, j);
                    }
                }
                continue;
            }

            const int32_t l = tree.left[static_cast<std::size_t>(id)];
            const int32_t r = tree.right[static_cast<std::size_t>(id)];
            const double lb_l =
                detail::node_cbar_lo(tree, src, q.data(), q_g, x, i, l, ui);
            const double lb_r =
                detail::node_cbar_lo(tree, src, q.data(), q_g, x, i, r, ui);

            // The weaker bound is pushed first so the stronger one is taken
            // first: the row's best tightens on the promising side, and the
            // other side is often skipped by the time it is reached.
            // A child that is not pushed is never visited again, and every pair
            // under it prices at or above its own bound, so that bound is what
            // the scan can still claim for them.
            if (lb_l <= lb_r) {
                if (lb_r < threshold) stack.emplace_back(lb_r, r);
                else if (lb_r < out.proven_floor) out.proven_floor = lb_r;
                if (lb_l < threshold) stack.emplace_back(lb_l, l);
                else if (lb_l < out.proven_floor) out.proven_floor = lb_l;
            } else {
                if (lb_l < threshold) stack.emplace_back(lb_l, l);
                else if (lb_l < out.proven_floor) out.proven_floor = lb_l;
                if (lb_r < threshold) stack.emplace_back(lb_r, r);
                else if (lb_r < out.proven_floor) out.proven_floor = lb_r;
            }
        }

        out.row_min[static_cast<std::size_t>(i)] = rmin;
        if (rmin < out.min_reduced_cost) {
            out.min_reduced_cost = rmin;
            out.arg_i = i;
            out.arg_j = rmin_j;
        }

        std::sort(row_violators.begin(), row_violators.end(),
                  [](const std::pair<double, int32_t>& a,
                     const std::pair<double, int32_t>& b) {
                      return a.second < b.second;
                  });
        for (const std::pair<double, int32_t>& pv : row_violators) {
            keep.offer(i, pv.first, pv.second);
        }

        for (int64_t t = 0; t < rn; ++t) {
            is_cand[static_cast<std::size_t>(rb[t])] = 0;
        }
    }

    keep.emit([&out](int32_t i, int32_t j, double cbar) {
        out.violators.push_back(PricedPair{i, j, cbar});
    });
    cand.note_evaluated(out.n_evaluated);
    // The evaluated pairs are bounded below by the observed minimum and the
    // skipped ones by the bounds they were skipped against, so the smaller of
    // the two holds over every omitted admissible pair.
    if (out.min_reduced_cost < out.proven_floor) {
        out.proven_floor = out.min_reduced_cost;
    }
    return out;
}

// The one call a round makes. A source whose metric a ball bounds is priced
// through its tree, and one whose metric it does not is priced over the grid,
// which is the same answer for more work rather than a different answer.
template <class Accept = AcceptAll>
BlockPricing price_pairs(const LazyCostMatrix& src, BallTree& tree,
                         const std::vector<double>& u,
                         const std::vector<double>& v,
                         CandidateSet& cand,
                         int keep_per_row,
                         double tol,
                         const Accept& accept = Accept()) {
    if (tree.empty()) return price_block(src, u, v, cand, keep_per_row, tol, accept);
    return price_tree(src, tree, u, v, cand, keep_per_row, tol, accept);
}

}  // namespace lap
