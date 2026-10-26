// src/flow/flow_topk.h
// The k smallest keys per row, in one allocation.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// Two passes over a problem want the same thing from a row. Block pricing keeps
// the most negative reduced costs a row offers; the feasibility reseed keeps the
// cheapest columns a deficient row can reach. Both are "the k smallest keys per
// row" over a stream that is consumed as it is produced, and both run over every
// row of the problem, so a vector of per-row heaps -- one allocation per row --
// is what neither can afford.
//
// One flat max-heap per row inside a single buffer: a row's largest kept key
// sits at its front, which is the one an incoming key has to beat.
#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

namespace lap {
namespace detail {

class RowTopK {
public:
    RowTopK(int64_t nrow, int keep)
        : keep_(keep > 0 ? keep : 0)
        , count_(static_cast<std::size_t>(nrow > 0 ? nrow : 0), 0) {
        if (keep_ > 0 && nrow > 0) {
            heap_.resize(static_cast<std::size_t>(nrow) * static_cast<std::size_t>(keep_));
        }
    }

    // How many entries a row keeps, which is what a producer that selects
    // before offering has to select.
    int32_t capacity() const { return keep_; }

    void offer(int64_t i, double key, int32_t j) {
        if (keep_ == 0) return;
        const std::ptrdiff_t off =
            static_cast<std::ptrdiff_t>(i) * static_cast<std::ptrdiff_t>(keep_);
        const auto beg = heap_.begin() + off;
        int32_t& n = count_[static_cast<std::size_t>(i)];
        if (n < keep_) {
            beg[n] = Entry(key, j);
            ++n;
            std::push_heap(beg, beg + n);
        } else if (key < beg->first) {
            std::pop_heap(beg, beg + n);
            beg[n - 1] = Entry(key, j);
            std::push_heap(beg, beg + n);
        }
    }

    // Hand every row's kept entries to `out(i, j, key)`, rows ascending and
    // columns ascending within a row. Reorders the heaps, so it runs once at
    // the end.
    template <class Fn>
    void emit(Fn&& out) {
        if (keep_ == 0) return;
        for (std::size_t i = 0; i < count_.size(); ++i) {
            const int32_t n = count_[i];
            if (n == 0) continue;
            const auto beg = heap_.begin() +
                static_cast<std::ptrdiff_t>(i) * static_cast<std::ptrdiff_t>(keep_);
            std::sort(beg, beg + n, [](const Entry& a, const Entry& b) {
                return a.second < b.second;
            });
            for (int32_t t = 0; t < n; ++t) {
                out(static_cast<int32_t>(i), beg[t].second, beg[t].first);
            }
        }
    }

private:
    using Entry = std::pair<double, int32_t>;  // max-heap on the key

    int32_t keep_ = 0;
    std::vector<Entry> heap_;
    std::vector<int32_t> count_;
};

}  // namespace detail
}  // namespace lap
