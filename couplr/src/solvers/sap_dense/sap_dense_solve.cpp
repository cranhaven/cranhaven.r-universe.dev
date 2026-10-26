// src/solvers/sap_dense/sap_dense_solve.cpp
// Pure C++ successive shortest path LAP solver, dense scan - NO Rcpp dependencies

#include "sap_dense_solve.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils.h"
#include <vector>
#include <limits>
#include <cmath>
#include <algorithm>

namespace lap {

namespace {

// Internal constants
constexpr int UNASSIGNED = -1;
constexpr double INF_COST = std::numeric_limits<double>::infinity();

// Matching state carried across augmentations
struct MatchingState {
    int n;                          // number of rows
    int m;                          // number of columns
    std::vector<int> row_to_col;    // row i assigned to column row_to_col[i], or UNASSIGNED
    std::vector<int> col_to_row;    // column j assigned to row col_to_row[j], or UNASSIGNED
    std::vector<double> row_price;  // dual variable u[i]
    std::vector<double> col_price;  // dual variable p[j]

    MatchingState(int n_, int m_)
        : n(n_), m(m_),
          row_to_col(n_, UNASSIGNED),
          col_to_row(m_, UNASSIGNED),
          row_price(n_, 0.0),
          col_price(m_, 0.0) {}

    int matching_size() const {
        int count = 0;
        for (int i = 0; i < n; ++i) {
            if (row_to_col[i] != UNASSIGNED) ++count;
        }
        return count;
    }

    bool is_complete() const {
        return matching_size() == n;
    }
};

// Find any unassigned row
inline int find_unassigned_row(const MatchingState& state) {
    for (int i = 0; i < state.n; ++i) {
        if (state.row_to_col[i] == UNASSIGNED) {
            return i;
        }
    }
    return UNASSIGNED;
}

// SSP augmentation using Dijkstra with potential updates
// Returns true if an augmenting path was found
bool ssp_augment_once(
    const CostMatrix& cost,
    MatchingState& state
) {
    const int n = state.n;
    const int m = state.m;

    // Find an unassigned row to start from
    int source_row = find_unassigned_row(state);
    if (source_row == UNASSIGNED) {
        return false;
    }

    // Dijkstra arrays
    std::vector<double> dist(m, INF_COST);        // Distance to each column
    std::vector<int> pred_col(m, UNASSIGNED);     // Predecessor column (or -1 if from source row)
    std::vector<bool> col_done(m, false);         // Column finalized
    std::vector<bool> row_in_tree(n, false);      // Rows in shortest path tree

    // Source row is in the tree
    row_in_tree[source_row] = true;

    // Initialize: distances from source_row to all columns
    // Reduced cost = cost[src,j] - u[src] - v[j]
    for (int j = 0; j < m; ++j) {
        if (cost.allowed(source_row, j)) {
            double c = cost.at(source_row, j);
            if (std::isfinite(c)) {
                dist[j] = c - state.row_price[source_row] - state.col_price[j];
                pred_col[j] = -1;  // Came from source row
            }
        }
    }

    // Dijkstra main loop
    int target_col = UNASSIGNED;
    double target_dist = INF_COST;

    while (true) {
        // Find minimum unfinalized column
        int best_col = UNASSIGNED;
        double best_dist = INF_COST;
        for (int j = 0; j < m; ++j) {
            if (!col_done[j] && dist[j] < best_dist) {
                best_dist = dist[j];
                best_col = j;
            }
        }

        if (best_col == UNASSIGNED || best_dist >= INF_COST) {
            break;  // No more reachable columns
        }

        col_done[best_col] = true;

        // Check if unassigned -> found shortest augmenting path
        if (state.col_to_row[best_col] == UNASSIGNED) {
            target_col = best_col;
            target_dist = best_dist;
            break;
        }

        // Column is matched - add its row to the tree and relax edges
        int matched_row = state.col_to_row[best_col];
        row_in_tree[matched_row] = true;

        for (int j = 0; j < m; ++j) {
            if (col_done[j]) continue;
            if (!cost.allowed(matched_row, j)) continue;

            double c = cost.at(matched_row, j);
            if (!std::isfinite(c)) continue;

            // Reduced cost from matched_row to j
            double rc = c - state.row_price[matched_row] - state.col_price[j];
            double new_dist = best_dist + rc;

            if (new_dist < dist[j]) {
                dist[j] = new_dist;
                pred_col[j] = best_col;
            }
        }
    }

    if (target_col == UNASSIGNED) {
        return false;  // No augmenting path found
    }

    // Update potentials using Johnson's reweighting
    double delta = target_dist;

    // Update column potentials for all reached columns
    for (int j = 0; j < m; ++j) {
        if (col_done[j]) {
            state.col_price[j] -= (delta - dist[j]);
        }
    }

    // Update row potentials for all rows in the tree
    state.row_price[source_row] += delta;

    for (int i = 0; i < n; ++i) {
        if (row_in_tree[i] && i != source_row) {
            int matched_col = state.row_to_col[i];
            if (matched_col != UNASSIGNED && col_done[matched_col]) {
                state.row_price[i] += (delta - dist[matched_col]);
            }
        }
    }

    // Augment along the path
    int j = target_col;

    while (j != UNASSIGNED) {
        int prev_col = pred_col[j];
        int reaching_row;

        if (prev_col == -1) {
            reaching_row = source_row;
        } else {
            reaching_row = state.col_to_row[prev_col];
        }

        // Unassign reaching_row from its current column (if any)
        if (state.row_to_col[reaching_row] != UNASSIGNED) {
            int old_col = state.row_to_col[reaching_row];
            state.col_to_row[old_col] = UNASSIGNED;
        }

        // Assign reaching_row to j
        state.row_to_col[reaching_row] = j;
        state.col_to_row[j] = reaching_row;

        j = prev_col;
    }

    return true;
}

}  // anonymous namespace

// Main solver implementation
LapResult solve_sap_dense(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    // Initialize matching state with zero prices
    MatchingState state(n, m);

    // Build the matching from scratch: each augmentation raises its size by 1
    int min_size = std::min(n, m);
    int augmentations = 0;
    int max_augmentations = min_size * 2;  // Safety limit

    while (state.matching_size() < min_size) {
        bool found = ssp_augment_once(work, state);
        if (!found) {
            LAP_THROW_INFEASIBLE("Could not find complete matching");
        }
        augmentations++;

        // Safety check
        if (augmentations > max_augmentations) {
            LAP_THROW_CONVERGENCE("Too many augmentations - possible infinite loop");
        }
    }

    // Build assignment: row -> column (0-based)
    std::vector<int> assignment = state.row_to_col;

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
