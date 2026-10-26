// src/flow/flow_row_search.h
// The two questions the implicit loop asks a cost source about a row.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// A round prices the pairs the master omits, and a round that came back short
// of its flow asks a deficient row for its cheapest columns. Both read a row
// against every column, and both are answerable from a bound when the source
// carries geometry to bound with. This is where a source says which it is:
// the primary template reads the row, and a specialisation answers from
// whatever structure it holds.
//
// One object per solve holds that structure, so a tree is built once and both
// questions are put to it. A source with no specialisation gets the scan, which
// is the same answer for more work rather than a different answer.
#pragma once

#include "../core/lap_cost_source.h"
#include "../core/lap_lazy_types.h"
#include "flow_balltree.h"
#include "flow_candidates.h"
#include "flow_pricing.h"
#include "flow_topk.h"
#include "flow_tree_nearest.h"
#include "flow_tree_pricing.h"

#include <cstdint>
#include <vector>

namespace lap {

namespace row_search_detail {

// The row scan, which is what a source with nothing to prune by answers with
// and what a specialisation falls back to when its structure is unavailable.
template <class Source>
void scan_cheapest_outside(const Source& src, int64_t i,
                           const std::vector<char>& skip,
                           detail::RowTopK& keep, int64_t slot,
                           RowScanWork& work) {
    const int64_t ncol = src.ncol;
    for (int64_t j = 0; j < ncol; ++j) {
        if (skip[static_cast<std::size_t>(j)]) continue;
        ++work.n_scanned;
        double c = 0.0;
        if (!cost_if_allowed(src, i, j, c)) continue;
        ++work.n_evaluated;
        keep.offer(slot, c, static_cast<int32_t>(j));
    }
}

}  // namespace row_search_detail

template <class Source>
struct RowSearch {
    explicit RowSearch(const Source&) {}

    template <class Accept = AcceptAll>
    BlockPricing price(const Source& src, const std::vector<double>& u,
                       const std::vector<double>& v, CandidateSet& cand,
                       int keep_per_row, double tol,
                       const Accept& accept = Accept()) {
        return price_block(src, u, v, cand, keep_per_row, tol, accept);
    }

    void cheapest_outside(const Source& src, int64_t i,
                          const std::vector<char>& skip,
                          detail::RowTopK& keep, int64_t slot,
                          RowScanWork& work) {
        row_search_detail::scan_cheapest_outside(src, i, skip, keep, slot, work);
    }
};

// Covariates above which a ball stops paying for a distance whose cost is
// linear in their number. The bound a ball gives loosens as the dimension
// grows, and a Euclidean distance is cheap enough that a loose bound costs
// more in bookkeeping than the evaluations it saves: measured through the
// front door it is 2.6x at two covariates, level at six and a loss at eight,
// where the descent still evaluates 2.4x fewer pairs. A Mahalanobis distance
// is quadratic in the same number, dear enough to pay for the bound at every
// dimension measured.
constexpr int64_t kBallTreeLinearVarLimit = 6;

inline bool ball_tree_pays(const LazyCostMatrix& src) {
    if (!metric_has_ball_bound(src.metric())) return false;
    if (src.metric() == DistanceMetric::Mahalanobis) return true;
    return src.n_vars() <= kBallTreeLinearVarLimit;
}

// Coordinates and a metric, so both questions come off a ball tree over the
// columns. An empty tree is a valid state and means the scan: a metric no ball
// bounds, a covariance with no Cholesky factor, or a dimension the bound stops
// paying at.
template <>
struct RowSearch<LazyCostMatrix> {
    BallTree tree;

    explicit RowSearch(const LazyCostMatrix& src) {
        if (ball_tree_pays(src)) tree = build_ball_tree(src);
    }

    template <class Accept = AcceptAll>
    BlockPricing price(const LazyCostMatrix& src, const std::vector<double>& u,
                       const std::vector<double>& v, CandidateSet& cand,
                       int keep_per_row, double tol,
                       const Accept& accept = Accept()) {
        return price_pairs(src, tree, u, v, cand, keep_per_row, tol, accept);
    }

    void cheapest_outside(const LazyCostMatrix& src, int64_t i,
                          const std::vector<char>& skip,
                          detail::RowTopK& keep, int64_t slot,
                          RowScanWork& work) {
        if (tree.empty()) {
            row_search_detail::scan_cheapest_outside(src, i, skip, keep, slot, work);
            return;
        }
        tree_cheapest_outside(src, tree, i, skip, keep, slot, work);
    }
};

// Every row's `keep.capacity()` cheapest admissible columns, one query per row
// to whatever `search` holds. Rows are slots of `keep` by their own index.
template <class Source>
void cheapest_per_row(const Source& src, RowSearch<Source>& search,
                      detail::RowTopK& keep, RowScanWork& work) {
    const std::vector<char> skip(static_cast<std::size_t>(src.ncol > 0 ? src.ncol : 0), 0);
    for (int64_t i = 0; i < src.nrow; ++i) {
        search.cheapest_outside(src, i, skip, keep, i, work);
    }
}

}  // namespace lap
