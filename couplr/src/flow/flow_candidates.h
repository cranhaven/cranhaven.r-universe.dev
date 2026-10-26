// src/flow/flow_candidates.h
// The pairs a restricted master is allowed to use.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// A restricted master is a FlowProblem whose bipartite block holds a fraction of
// the pairs its cost source admits. Which fraction is a question that outlives
// any one solve: the pricing loop grows the set round by round, and a caliper
// path sweeps the same set across a sequence of problems. So the set is its own
// object rather than a field of FlowProblem.
//
// It is also a different question from BlockArcRange's. That records where a
// block's arcs landed in the arc array; this records which pairs are in. The two
// agree after an expansion and diverge the moment a pair the source forbids is
// offered, because a forbidden pair is in the candidate set and has no arc.
//
// Storage is one flat per-row CSR over column indices, held sorted within each
// row, which is what makes membership a binary search rather than a scan and
// what lets a pricing sweep walk a row's candidates in step with a column scan.
#pragma once

#include "../core/lap_error.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

namespace lap {

class CandidateSet {
public:
    using Pair = std::pair<int32_t, int32_t>;

    CandidateSet(int64_t nrow, int64_t ncol) : nrow_(nrow), ncol_(ncol) {
        if (nrow < 0 || ncol < 0) {
            LAP_THROW_DIMENSION("CandidateSet: negative dimensions");
        }
        ptr_.assign(static_cast<std::size_t>(nrow) + 1u, 0);
    }

    int64_t nrow() const { return nrow_; }
    int64_t ncol() const { return ncol_; }
    int64_t n_arcs() const { return ptr_.back(); }

    int64_t row_size(int64_t i) const {
        check_row(i);
        return ptr_[static_cast<std::size_t>(i) + 1u] - ptr_[static_cast<std::size_t>(i)];
    }

    // Row i's columns, ascending. Invalidated by the next add_pairs(). Both
    // ends are null on an empty set, because data() on an empty vector may be
    // null and offsetting a null pointer is undefined even by zero.
    const int32_t* row_begin(int64_t i) const {
        check_row(i);
        if (idx_.empty()) return nullptr;
        return idx_.data() + ptr_[static_cast<std::size_t>(i)];
    }
    const int32_t* row_end(int64_t i) const {
        check_row(i);
        if (idx_.empty()) return nullptr;
        return idx_.data() + ptr_[static_cast<std::size_t>(i) + 1u];
    }

    bool contains(int64_t i, int64_t j) const {
        check_pair(i, j);
        return std::binary_search(row_begin(i), row_end(i), static_cast<int32_t>(j));
    }

    // Merge `pairs` in and report the ones that were not already there, in row
    // order, which is what add_block_arcs() takes. Duplicates within `pairs`
    // report once. A pair the cost source forbids is still a candidate: whether
    // it becomes an arc is the expansion's decision, and re-offering it every
    // round is the waste the set exists to prevent.
    //
    // One rebuild pass, O(nrow + n_arcs + k log k), whatever k is. Inserting
    // into a CSR shifts everything after the insertion point, so a per-row
    // insert repeated over the rows of one pricing round would cost
    // O(nrow * n_arcs); the phase 0 prototype rebuilt for the same reason.
    std::vector<Pair> add_pairs(const std::vector<Pair>& pairs) {
        std::vector<Pair> want(pairs);
        for (const Pair& p : want) check_pair(p.first, p.second);
        std::sort(want.begin(), want.end());
        want.erase(std::unique(want.begin(), want.end()), want.end());
        if (want.empty()) return {};

        std::vector<Pair> added;
        added.reserve(want.size());

        std::vector<int64_t> new_ptr(ptr_.size(), 0);
        std::vector<int32_t> new_idx;
        new_idx.reserve(idx_.size() + want.size());

        std::size_t w = 0;
        for (int64_t i = 0; i < nrow_; ++i) {
            std::size_t a  = static_cast<std::size_t>(ptr_[static_cast<std::size_t>(i)]);
            std::size_t ae = static_cast<std::size_t>(ptr_[static_cast<std::size_t>(i) + 1u]);
            const std::size_t w0 = w;
            while (w < want.size() && want[w].first == i) ++w;

            std::size_t b = w0;
            while (a < ae && b < w) {
                if (idx_[a] < want[b].second) {
                    new_idx.push_back(idx_[a++]);
                } else if (want[b].second < idx_[a]) {
                    new_idx.push_back(want[b].second);
                    added.push_back(want[b]);
                    ++b;
                } else {
                    new_idx.push_back(idx_[a++]);
                    ++b;
                }
            }
            while (a < ae) new_idx.push_back(idx_[a++]);
            while (b < w) {
                new_idx.push_back(want[b].second);
                added.push_back(want[b]);
                ++b;
            }
            new_ptr[static_cast<std::size_t>(i) + 1u] = static_cast<int64_t>(new_idx.size());
        }

        ptr_.swap(new_ptr);
        idx_.swap(new_idx);
        return added;
    }

    // Pairs whose cost was computed, summed over every round the set has seen.
    // It is a property of the search rather than of the set, and it lives here
    // because the set is the object that survives the solve it is reported on.
    void note_evaluated(int64_t n) { evaluated_ += n; }
    int64_t edges_evaluated() const { return evaluated_; }

private:
    void check_row(int64_t i) const {
        if (i < 0 || i >= nrow_) {
            LAP_THROW_DIMENSION("CandidateSet: row " + std::to_string(i) +
                                " outside [0, " + std::to_string(nrow_) + ")");
        }
    }
    void check_pair(int64_t i, int64_t j) const {
        check_row(i);
        if (j < 0 || j >= ncol_) {
            LAP_THROW_DIMENSION("CandidateSet: column " + std::to_string(j) +
                                " outside [0, " + std::to_string(ncol_) + ")");
        }
    }

    int64_t nrow_ = 0;
    int64_t ncol_ = 0;
    std::vector<int64_t> ptr_;  // size nrow_ + 1
    std::vector<int32_t> idx_;  // ascending within each row
    int64_t evaluated_ = 0;
};

}  // namespace lap
