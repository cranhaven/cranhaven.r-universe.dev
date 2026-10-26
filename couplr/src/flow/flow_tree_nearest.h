// src/flow/flow_tree_nearest.h
// The cheapest columns of one row, over the ball tree instead of over the row.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The feasibility re-seed asks a deficient row for its `width` cheapest
// admissible columns outside N(S), and answers it by reading every column. That
// is the same question the pricer asks with both duals at zero, so it gets the
// same descent: a subtree whose cost floor is above the worst column kept so
// far holds nothing that can enter the answer, and is skipped unread.
//
// The kept set is the one the row scan would have kept, ties included. A scan
// offers columns with j ascending and RowTopK admits on the key alone, so what
// it leaves behind is the `width` smallest columns under (cost, j) taken
// lexicographically. The descent reaches columns in leaf order instead, so it
// selects under that same lexicographic order itself, and hands the result over
// with j ascending. The floor is tested strictly against the worst kept cost
// for the same reason: a column whose cost ties that worst still wins the pair
// when its index is smaller, so a node that could hold one is entered.
#pragma once

#include "../core/lap_lazy_types.h"
#include "flow_balltree.h"
#include "flow_topk.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <utility>
#include <vector>

namespace lap {

// What a row's scan cost, whether it read the row or descended a tree.
struct RowScanWork {
    int64_t n_scanned = 0;
    int64_t n_evaluated = 0;
};

namespace detail {

// Ordering on (cost, column) which is what a scan offering columns in
// ascending order leaves behind.
inline bool cheaper_column(const std::pair<double, int32_t>& a,
                           const std::pair<double, int32_t>& b) {
    if (a.first != b.first) return a.first < b.first;
    return a.second < b.second;
}

}  // namespace detail

// Put the cheapest admissible columns of row `i` outside `skip` into slot
// `slot` of `keep`, as many as `keep` has room for.
inline void tree_cheapest_outside(const LazyCostMatrix& src, const BallTree& tree,
                                  int64_t i, const std::vector<char>& skip,
                                  detail::RowTopK& keep, int64_t slot,
                                  RowScanWork& work) {
    const int32_t width = keep.capacity();
    if (width <= 0 || tree.empty()) return;

    constexpr double kInf = std::numeric_limits<double>::infinity();

    const double* x = src.left_row(i);
    std::vector<double> q(static_cast<std::size_t>(tree.n_vars), 0.0);
    const double q_g = whiten_point(tree, x, q.data());

    // A max-heap under the same order the answer is selected by, so its front
    // is the column an incoming one has to beat.
    std::vector<std::pair<double, int32_t>> best;
    best.reserve(static_cast<std::size_t>(width));

    std::vector<std::pair<double, int32_t>> stack;
    const double root = node_cost_floor(tree, src, q.data(), q_g, x, i, 0);
    if (root < kInf) stack.emplace_back(root, 0);

    while (!stack.empty()) {
        const double floor_c = stack.back().first;
        const int32_t id = stack.back().second;
        stack.pop_back();

        // Nothing bounds the descent until the answer is full, and once it is,
        // the bound has only tightened since this node was pushed.
        if (static_cast<int32_t>(best.size()) == width && floor_c > best.front().first) {
            continue;
        }

        if (tree.is_leaf(id)) {
            const int32_t end = tree.hi[static_cast<std::size_t>(id)];
            for (int32_t t = tree.lo[static_cast<std::size_t>(id)]; t < end; ++t) {
                const int32_t j = tree.perm[static_cast<std::size_t>(t)];
                if (skip[static_cast<std::size_t>(j)]) continue;
                ++work.n_scanned;
                double c = 0.0;
                if (!src.admissible(i, static_cast<int64_t>(j), c)) continue;
                ++work.n_evaluated;
                const std::pair<double, int32_t> entry(c, j);
                if (static_cast<int32_t>(best.size()) < width) {
                    best.push_back(entry);
                    std::push_heap(best.begin(), best.end(), detail::cheaper_column);
                } else if (detail::cheaper_column(entry, best.front())) {
                    std::pop_heap(best.begin(), best.end(), detail::cheaper_column);
                    best.back() = entry;
                    std::push_heap(best.begin(), best.end(), detail::cheaper_column);
                }
            }
            continue;
        }

        const int32_t l = tree.left[static_cast<std::size_t>(id)];
        const int32_t r = tree.right[static_cast<std::size_t>(id)];
        const double fl = node_cost_floor(tree, src, q.data(), q_g, x, i, l);
        const double fr = node_cost_floor(tree, src, q.data(), q_g, x, i, r);

        const double limit = static_cast<int32_t>(best.size()) == width
                                 ? best.front().first
                                 : kInf;
        const auto push = [&](double f, int32_t child) {
            if (f < kInf && !(f > limit)) stack.emplace_back(f, child);
        };

        // The weaker floor is pushed first so the stronger one is taken first:
        // the answer fills from the near side, and the far side is often
        // skipped by the time it is reached.
        if (fl <= fr) {
            push(fr, r);
            push(fl, l);
        } else {
            push(fl, l);
            push(fr, r);
        }
    }

    std::sort(best.begin(), best.end(),
              [](const std::pair<double, int32_t>& a,
                 const std::pair<double, int32_t>& b) { return a.second < b.second; });
    for (const std::pair<double, int32_t>& e : best) {
        keep.offer(slot, e.first, e.second);
    }
}

}  // namespace lap
