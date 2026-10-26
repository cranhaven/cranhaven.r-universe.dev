// src/core/lap_utils.h
// Pure C++ utility functions for LAP solvers - NO Rcpp dependencies
#pragma once

#include "lap_types.h"
#include "lap_error.h"
#include "lap_lazy_types.h"
#include <vector>
#include <string>
#include <utility>

namespace lap {

// String key for match vectors (useful for deduplication in k-best)
// e.g., {2, 0, 1} -> "2,0,1"
std::string match_to_key(const std::vector<int>& match);

// Build CSR-style "allowed" structure from a mask
// mask: 0 = forbidden, nonzero = allowed (opposite of internal convention!)
// row_ptr: size n+1, row_ptr[i] to row_ptr[i+1] gives range in cols
// cols: allowed column indices (0-based) for each row
// row_ptr is int64_t because the CUMULATIVE allowed-edge count can reach
// n*m in the all-allowed case (e.g. a dense Euclidean matrix with no
// calipers), which overflows `int` past the same ~46,341-square threshold
// as CostMatrix's flat index -- even though individual column values in
// `cols` stay `int`-valued (a single row/column count never approaches
// INT_MAX on its own).
void build_allowed(const std::vector<int>& mask, int64_t n, int64_t m,
                   std::vector<int64_t>& row_ptr, std::vector<int>& cols);

// Generic cost-source overload: enumerates allowed pairs via the source's
// own allowed(i,j)/nrow/ncol instead of a raw mask array. Covers
// LazyCostMatrix and decorators over it (e.g. PaddedCostView<LazyCostMatrix>)
// with one definition -- header-only since the set of concrete cost-source
// types (any decorator wrapping any base) isn't closed/known here.
template <typename CostSourceT>
void build_allowed(const CostSourceT& cost,
                   std::vector<int64_t>& row_ptr, std::vector<int>& cols) {
    const int64_t n = cost.nrow;
    const int64_t m = cost.ncol;
    row_ptr.assign(static_cast<size_t>(n + 1), 0);

    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            if (cost.allowed(i, j)) ++row_ptr[static_cast<size_t>(i + 1)];
        }
    }
    for (int64_t i = 1; i <= n; ++i) {
        row_ptr[static_cast<size_t>(i)] += row_ptr[static_cast<size_t>(i - 1)];
    }

    cols.assign(static_cast<size_t>(row_ptr.back()), -1);
    std::vector<int64_t> fill = row_ptr;
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            if (cost.allowed(i, j)) {
                cols[static_cast<size_t>(fill[static_cast<size_t>(i)]++)] = static_cast<int>(j);
            }
        }
    }
}

// Check that each row has at least one allowed edge
// Throws InfeasibleException if any row has no options
void ensure_each_row_has_option(const std::vector<int>& mask, int64_t n, int64_t m);

// Generic cost-source overload: checks the source's own allowed(i,j). See
// build_allowed() above for why this is a header-only template.
template <typename CostSourceT>
void ensure_each_row_has_option(const CostSourceT& cost) {
    for (int64_t i = 0; i < cost.nrow; ++i) {
        bool has_option = false;
        for (int64_t j = 0; j < cost.ncol; ++j) {
            if (cost.allowed(i, j)) {
                has_option = true;
                break;
            }
        }
        if (!has_option) {
            throw InfeasibleException("Infeasible: row " + std::to_string(i + 1) +
                                     " has no allowed edges");
        }
    }
}

// Check if cost matrix is feasible (each row has at least one finite value)
// Returns true if feasible, false otherwise (does not throw)
bool is_feasible(const CostMatrix& cost);

// Check if a matching result is valid (no forbidden edges chosen)
// match: 0-based column indices, -1 = unmatched
bool is_valid_matching(const CostMatrix& cost, const std::vector<int>& match);

// Check if a perfect matching exists using augmenting paths
// More thorough than is_feasible() - actually tries to find a matching.
//
// Generic over the cost source, so a lazy source answers the same question as
// a dense one. Header-only for the same reason build_allowed() is: the source
// type is not known here.
namespace detail {
// DFS for an augmenting path from row `u` over the allowed-edge adjacency.
inline bool dfs_augment(int u, const std::vector<std::vector<int>>& adj,
                        std::vector<int>& match_v, std::vector<bool>& visited) {
    for (int v : adj[static_cast<size_t>(u)]) {
        if (visited[static_cast<size_t>(v)]) continue;
        visited[static_cast<size_t>(v)] = true;
        if (match_v[static_cast<size_t>(v)] < 0 ||
            dfs_augment(match_v[static_cast<size_t>(v)], adj, match_v, visited)) {
            match_v[static_cast<size_t>(v)] = u;
            return true;
        }
    }
    return false;
}
}  // namespace detail

template <typename CostSourceT>
bool has_valid_matching_view(const CostSourceT& cost) {
    // DFS-augmenting-path feasibility check: already O(n*m) with per-row DFS
    // overhead, impractical well before n/m individually approach INT_MAX, so
    // truncation here is intentional.
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) return true;
    if (n > m) return false;

    std::vector<std::vector<int>> adj(static_cast<size_t>(n));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (cost.allowed(i, j) && std::isfinite(cost.at(i, j))) {
                adj[static_cast<size_t>(i)].push_back(j);
            }
        }
        if (adj[static_cast<size_t>(i)].empty()) return false;
    }

    std::vector<int> match_v(static_cast<size_t>(m), -1);
    int matched = 0;
    for (int u = 0; u < n; ++u) {
        std::vector<bool> visited(static_cast<size_t>(m), false);
        if (detail::dfs_augment(u, adj, match_v, visited)) ++matched;
    }
    return matched == n;
}

bool has_valid_matching(const CostMatrix& cost);

// Compute total cost from a cost matrix and assignment
// match: 0-based column indices, -1 = unmatched
// Returns sum of cost[i, match[i]] for all matched rows
double compute_total_cost(const CostMatrix& cost, const std::vector<int>& match);

// Compute total cost using original cost matrix (for when we have transformed costs)
// original_cost: the untransformed cost matrix
// match: 0-based column indices, -1 = unmatched
double compute_total_cost(const CostMatrix& original_cost,
                          const CostMatrix& work_cost,
                          const std::vector<int>& match);

// Negate costs for maximization (returns new matrix)
CostMatrix negate_costs(const CostMatrix& cost);

// Mark every cell priced at or above the forbidden sentinel as forbidden.
//
// A matrix that has been through prepare_for_solve() carries BIG in the cells
// its mask calls forbidden, and R draws the same line (is.finite(x) & x <
// BIG_COST, in R/matching_constraints.R), so a cost that large says there is no
// edge there whatever the mask says. Run on the matrix before any negation, or
// the sentinel is no longer the largest value in it.
void forbid_sentinel_costs(CostMatrix& cost);

// Prepare cost matrix for solving (handles maximization, padding for rectangular)
// Returns: prepared CostMatrix (negated if maximize, padded if needed)
CostMatrix prepare_for_solve(const CostMatrix& cost, bool maximize);

// Convert 0-based match to 1-based (for R interface)
std::vector<int> to_one_based(const std::vector<int>& match);

// Convert 1-based match to 0-based (from R interface)
std::vector<int> to_zero_based(const std::vector<int>& match);

}  // namespace lap
