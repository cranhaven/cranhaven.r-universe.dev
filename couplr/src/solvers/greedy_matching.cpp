// src/solvers/greedy_matching.cpp
// Pure C++ Greedy Matching - NO Rcpp dependencies

#include "greedy_matching.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <algorithm>
#include <vector>
#include <queue>
#include <limits>
#include <cmath>

namespace lap {

// Candidate pair for greedy matching
struct CandidatePair {
    int row;
    int col;
    double cost;

    CandidatePair(int r, int c, double w) : row(r), col(c), cost(w) {}

    // For priority queue (min-heap)
    bool operator>(const CandidatePair& other) const {
        return cost > other.cost;
    }
};

// ==============================================================================
// Greedy matching: sorted pairs strategy
// ==============================================================================
// Collect all valid pairs, sort by cost, greedily assign

LapResult greedy_matching_sorted(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Greedy is a partial-matching heuristic: when n > m it matches min(n, m)
    // rows and leaves the rest unmatched, rather than rejecting the problem.

    // Collect all valid candidate pairs
    std::vector<CandidatePair> candidates;
    candidates.reserve(n * m);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            double c = cost.at(i, j);

            // Skip forbidden pairs: non-finite, masked, or the large-finite
            // BIG sentinel the matching layer uses for forbidden edges. Unlike
            // the optimal solvers (which avoid huge costs by minimizing), greedy
            // would otherwise pick a BIG edge when it is a row's only option
            // instead of leaving that row unmatched.
            if (!std::isfinite(c) || !cost.allowed(i, j) || c >= BIG) {
                continue;
            }

            // For maximize, negate cost for sorting
            double sort_cost = maximize ? -c : c;
            candidates.push_back(CandidatePair(i, j, sort_cost));
        }
    }

    // Sort candidates by cost (ascending)
    std::sort(candidates.begin(), candidates.end(),
              [](const CandidatePair& a, const CandidatePair& b) {
                  return a.cost < b.cost;
              });

    // Track which rows/cols are matched
    std::vector<bool> row_matched(n, false);
    std::vector<bool> col_matched(m, false);
    std::vector<int> assignment(n, -1);  // 0-based, -1 = unmatched

    double total_cost = 0.0;
    int n_matched = 0;

    // Greedily assign pairs
    for (const auto& pair : candidates) {
        if (!row_matched[pair.row] && !col_matched[pair.col]) {
            // Match this pair
            assignment[pair.row] = pair.col;
            row_matched[pair.row] = true;
            col_matched[pair.col] = true;

            // Get actual cost (not negated)
            double actual_cost = cost.at(pair.row, pair.col);
            total_cost += actual_cost;
            n_matched++;

            // Early exit if all rows matched
            if (n_matched == n || n_matched == m) {
                break;
            }
        }
    }

    // Check feasibility (at least try to match all rows)
    // For greedy, we accept partial matchings, but warn if infeasible
    if (n_matched == 0) {
        LAP_THROW_INFEASIBLE("Could not find any valid matches");
    }

    return LapResult(std::move(assignment), total_cost, "optimal");
}

// ==============================================================================
// Greedy matching: row-best strategy
// ==============================================================================
// For each unmatched row, find its best available column

LapResult greedy_matching_row_best(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Greedy is a partial-matching heuristic: when n > m it matches min(n, m)
    // rows and leaves the rest unmatched, rather than rejecting the problem.

    std::vector<bool> col_matched(m, false);
    std::vector<int> assignment(n, -1);  // 0-based, -1 = unmatched

    double total_cost = 0.0;
    int n_matched = 0;

    // For each row, find best available column
    for (int i = 0; i < n; i++) {
        int best_col = -1;
        double best_cost = maximize ? -std::numeric_limits<double>::infinity()
                                    : std::numeric_limits<double>::infinity();

        // Find best available column for this row
        for (int j = 0; j < m; j++) {
            if (col_matched[j]) continue;

            double c = cost.at(i, j);

            // Skip forbidden pairs
            if (!std::isfinite(c) || !cost.allowed(i, j) || c >= BIG) {
                continue;
            }

            // Check if this is better
            bool is_better = maximize ? (c > best_cost) : (c < best_cost);
            if (is_better) {
                best_cost = c;
                best_col = j;
            }
        }

        // If found a valid match, assign it
        if (best_col >= 0) {
            assignment[i] = best_col;
            col_matched[best_col] = true;
            total_cost += best_cost;
            n_matched++;
        }
    }

    // Check feasibility
    if (n_matched == 0) {
        LAP_THROW_INFEASIBLE("Could not find any valid matches");
    }

    return LapResult(std::move(assignment), total_cost, "optimal");
}

// ==============================================================================
// Greedy matching: priority queue strategy
// ==============================================================================
// Use priority queue for efficient selection (good for very large problems)

LapResult greedy_matching_pq(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Greedy is a partial-matching heuristic: when n > m it matches min(n, m)
    // rows and leaves the rest unmatched, rather than rejecting the problem.

    // Min-heap priority queue
    std::priority_queue<CandidatePair, std::vector<CandidatePair>,
                       std::greater<CandidatePair>> pq;

    // Build priority queue
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            double c = cost.at(i, j);

            if (!std::isfinite(c) || !cost.allowed(i, j) || c >= BIG) {
                continue;
            }

            double sort_cost = maximize ? -c : c;
            pq.push(CandidatePair(i, j, sort_cost));
        }
    }

    // Track matched units
    std::vector<bool> row_matched(n, false);
    std::vector<bool> col_matched(m, false);
    std::vector<int> assignment(n, -1);  // 0-based, -1 = unmatched

    double total_cost = 0.0;
    int n_matched = 0;

    // Greedily assign from priority queue
    int max_matches = (n < m) ? n : m;
    while (!pq.empty() && n_matched < max_matches) {
        CandidatePair pair = pq.top();
        pq.pop();

        if (!row_matched[pair.row] && !col_matched[pair.col]) {
            assignment[pair.row] = pair.col;
            row_matched[pair.row] = true;
            col_matched[pair.col] = true;

            double actual_cost = cost.at(pair.row, pair.col);
            total_cost += actual_cost;
            n_matched++;
        }
    }

    // Check feasibility
    if (n_matched == 0) {
        LAP_THROW_INFEASIBLE("Could not find any valid matches");
    }

    return LapResult(std::move(assignment), total_cost, "optimal");
}

// ==============================================================================
// Main dispatcher
// ==============================================================================

LapResult greedy_matching(const CostMatrix& cost, bool maximize,
                          const std::string& strategy) {
    if (strategy == "sorted") {
        return greedy_matching_sorted(cost, maximize);
    } else if (strategy == "row_best") {
        return greedy_matching_row_best(cost, maximize);
    } else if (strategy == "pq") {
        return greedy_matching_pq(cost, maximize);
    } else {
        LAP_THROW("Unknown greedy strategy: " + strategy);
    }
}

}  // namespace lap
