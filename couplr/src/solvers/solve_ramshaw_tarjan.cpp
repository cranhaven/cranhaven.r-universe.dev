// src/solvers/solve_ramshaw_tarjan.cpp
// Pure C++ Ramshaw-Tarjan Algorithm for Rectangular Assignment Problems - NO Rcpp dependencies

#include "solve_ramshaw_tarjan.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

// Helper: Transpose a cost matrix
static CostMatrix transpose_cost_matrix(const CostMatrix& cost) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    CostMatrix result(m, n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            result.at(j, i) = cost.at(i, j);
            result.mask[static_cast<size_t>(flat_index(j, i, n))] =
                cost.mask[static_cast<size_t>(flat_index(i, j, m))];
        }
    }

    return result;
}

LapResult solve_ramshaw_tarjan(const CostMatrix& cost, bool maximize) {
    const int n0 = static_cast<int>(cost.nrow);
    const int m0 = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n0 == 0 || m0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Ensure rows <= cols (transpose if needed)
    bool transposed = false;
    CostMatrix C = cost;
    int n = n0, m = m0;

    if (n0 > m0) {
        C = transpose_cost_matrix(cost);
        n = m0;
        m = n0;
        transposed = true;
    }

    // Build cost matrix (negate for maximize)
    std::vector<std::vector<double>> W(n, std::vector<double>(m));
    std::vector<std::vector<bool>> allowed(n, std::vector<bool>(m, false));
    bool has_finite = false;

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double v = C.at(i, j);
            if (std::isfinite(v)) {
                has_finite = true;
                W[i][j] = maximize ? -v : v;
                allowed[i][j] = true;
            } else {
                W[i][j] = BIG;
                allowed[i][j] = false;
            }
        }
    }

    if (!has_finite) {
        LAP_THROW_INFEASIBLE("No finite costs found");
    }

    // Check feasibility: each row must have at least one allowed column
    for (int i = 0; i < n; ++i) {
        bool has_option = false;
        for (int j = 0; j < m; ++j) {
            if (allowed[i][j]) {
                has_option = true;
                break;
            }
        }
        if (!has_option) {
            LAP_THROW_INFEASIBLE("Row " + std::to_string(i) + " has no valid assignments");
        }
    }

    // Dual potentials
    std::vector<double> u(n, 0.0);  // Row potentials
    std::vector<double> v(m, 0.0);  // Column potentials

    // Initialize row potentials: u[i] = min_j W[i][j]
    for (int i = 0; i < n; ++i) {
        double min_cost = BIG;
        for (int j = 0; j < m; ++j) {
            if (allowed[i][j] && W[i][j] < min_cost) {
                min_cost = W[i][j];
            }
        }
        u[i] = min_cost;
    }

    // Matching arrays
    std::vector<int> row_to_col(n, -1);
    std::vector<int> col_to_row(m, -1);

    // Reduced cost lambda
    auto reduced_cost = [&](int i, int j) -> double {
        return W[i][j] - u[i] - v[j];
    };

    // Augment matching one row at a time using Dijkstra
    for (int start_row = 0; start_row < n; ++start_row) {
        // Dijkstra to find shortest augmenting path from start_row
        std::vector<double> dist(m, std::numeric_limits<double>::infinity());
        std::vector<int> prev_col(m, -1);   // Previous column in path
        std::vector<int> prev_row(m, -1);   // Row that led to this column
        std::vector<bool> col_visited(m, false);

        // Priority queue: (distance, column)
        std::priority_queue<std::pair<double, int>,
                            std::vector<std::pair<double, int>>,
                            std::greater<std::pair<double, int>>> pq;

        // Initialize: edges from start_row to all columns
        for (int j = 0; j < m; ++j) {
            if (allowed[start_row][j]) {
                double rc = reduced_cost(start_row, j);
                if (rc < dist[j]) {
                    dist[j] = rc;
                    prev_row[j] = start_row;
                    pq.push({dist[j], j});
                }
            }
        }

        int end_col = -1;  // Unmatched column we find

        while (!pq.empty()) {
            auto [d, j] = pq.top();
            pq.pop();

            if (col_visited[j]) continue;
            col_visited[j] = true;

            // If this column is unmatched, we found augmenting path
            if (col_to_row[j] < 0) {
                end_col = j;
                break;
            }

            // Otherwise, extend through the matched row
            int matched_row = col_to_row[j];
            for (int jj = 0; jj < m; ++jj) {
                if (!col_visited[jj] && allowed[matched_row][jj]) {
                    double rc = reduced_cost(matched_row, jj);
                    double new_dist = dist[j] + rc;
                    if (new_dist < dist[jj]) {
                        dist[jj] = new_dist;
                        prev_col[jj] = j;
                        prev_row[jj] = matched_row;
                        pq.push({new_dist, jj});
                    }
                }
            }
        }

        if (end_col < 0) {
            LAP_THROW_INFEASIBLE("Could not find augmenting path for row " + std::to_string(start_row));
        }

        // Update potentials for visited columns
        // This maintains reduced cost non-negativity
        double delta = dist[end_col];
        u[start_row] += delta;

        for (int j = 0; j < m; ++j) {
            if (col_visited[j]) {
                v[j] -= (delta - dist[j]);
                if (col_to_row[j] >= 0) {
                    u[col_to_row[j]] += (delta - dist[j]);
                }
            }
        }

        // Augment along the path
        int cur_col = end_col;
        int cur_row = prev_row[cur_col];

        while (cur_col >= 0) {
            int old_col = (cur_row == start_row) ? -1 : prev_col[cur_col];

            // Make assignment
            row_to_col[cur_row] = cur_col;
            col_to_row[cur_col] = cur_row;

            if (old_col < 0) break;

            cur_col = old_col;
            cur_row = prev_row[cur_col];
        }
    }

    // Build output (0-based assignment)
    std::vector<int> assignment;
    double total = 0.0;

    if (!transposed) {
        // Original orientation: rows = n, cols = m
        assignment.resize(n);
        for (int i = 0; i < n; ++i) {
            int j = row_to_col[i];
            assignment[i] = (j >= 0) ? j : -1;
            if (j >= 0 && j < m0) {
                double val = cost.at(i, j);
                if (std::isfinite(val)) {
                    total += val;
                }
            }
        }
    } else {
        // Transposed: original had rows = m0, cols = n0
        // C is transposed, so C has rows = n = m0, cols = m = n0
        // row_to_col[i] gives column in C, which is row in original
        assignment.assign(n0, -1);
        for (int i = 0; i < n; ++i) {
            int j = row_to_col[i];
            if (j >= 0 && j < n0) {
                assignment[j] = i;  // 0-based
                double val = cost.at(j, i);  // cost is original, so cost[j][i]
                if (std::isfinite(val)) {
                    total += val;
                }
            }
        }
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
