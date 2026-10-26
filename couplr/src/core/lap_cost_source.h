// src/core/lap_cost_source.h
// One question where the cost-source concept asks two.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The concept is at(i, j), allowed(i, j), nrow and ncol, and a loop over pairs
// asks admissibility and then cost: `if (!src.allowed(i, j)) continue;` then
// `src.at(i, j)`. For a stored matrix that is two array reads. For a source
// that computes its costs it can be much worse: LazyCostMatrix under a finite
// max_distance tests admissibility *with* a distance, and at() tests it again
// before returning one, so the pair costs three evaluations of the distance
// where it needs one. Measured at 2.2x on a 2,000 x 10,000 pricing round.
//
// cost_if_allowed() is the one question. A source that can answer both at once
// says so by exposing
//
//     bool admissible(int64_t i, int64_t j, double& cost) const;
//
// and everything else falls back to allowed() then at(), which is what the
// concept already guarantees. Adding a source to the fast path is that one
// method on the source, with nothing to change at any call site.
#pragma once

#include <cstdint>
#include <type_traits>
#include <utility>

namespace lap {

namespace detail {

template <class T, class = void>
struct has_admissible : std::false_type {};

template <class T>
struct has_admissible<
    T, std::void_t<decltype(std::declval<const T&>().admissible(
           std::declval<int64_t>(), std::declval<int64_t>(),
           std::declval<double&>()))>> : std::true_type {};

}  // namespace detail

// Whether (i, j) is admissible, writing its cost to `cost` when it is and
// leaving `cost` untouched when it is not.
template <class Source>
inline bool cost_if_allowed(const Source& src, int64_t i, int64_t j, double& cost) {
    if constexpr (detail::has_admissible<Source>::value) {
        return src.admissible(i, j, cost);
    } else {
        if (!src.allowed(i, j)) return false;
        cost = src.at(i, j);
        return true;
    }
}

}  // namespace lap
