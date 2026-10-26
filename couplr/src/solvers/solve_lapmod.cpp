// src/solvers/solve_lapmod.cpp
// Pure C++ LAPMOD solver - NO Rcpp dependencies
//
// LAPMOD: Sparse Linear Assignment Problem solver
// Based on: Jonker & Volgenant (1987) "A Shortest Augmenting Path Algorithm"
//
// This implementation uses CSR format to exploit sparsity.
// For matrices with >50% NA/Inf entries and n>100, this is faster than dense JV.

#include "solve_lapmod.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

namespace {

constexpr int UNASSIGNED = -1;
constexpr double INF = std::numeric_limits<double>::infinity();

// Sparse matrix in CSR (Compressed Sparse Row) format
struct SparseMatrix {
    int n, m;
    std::vector<int> row_ptr;    // row_ptr[i] to row_ptr[i+1] gives range for row i
    std::vector<int> col_idx;    // column indices of non-forbidden entries
    std::vector<double> values;  // cost values

    int row_size(int i) const { return row_ptr[i + 1] - row_ptr[i]; }
    int col(int i, int k) const { return col_idx[row_ptr[i] + k]; }
    double cost(int i, int k) const { return values[row_ptr[i] + k]; }
};

// Build sparse matrix from CostMatrix
SparseMatrix build_sparse(const CostMatrix& cost, bool maximize) {
    SparseMatrix sp;
    sp.n = static_cast<int>(cost.nrow);
    sp.m = static_cast<int>(cost.ncol);

    // nnz and the row_ptr prefix sums are int; a dense n*m exceeding 2^31 would
    // overflow them into wrong offsets. nnz can never exceed n*m, so reject when
    // n*m does not fit in an int (such a matrix is >16 GB and infeasible anyway).
    if (static_cast<long long>(sp.n) * sp.m >
        static_cast<long long>(std::numeric_limits<int>::max())) {
        LAP_THROW_DIMENSION("lapmod: problem too large (n*m exceeds 2^31); "
                            "use method = 'jv' or 'auction'.");
    }

    sp.row_ptr.resize(sp.n + 1);

    // Find max cost for negation if maximize
    double cmax = -INF;
    if (maximize) {
        for (int i = 0; i < sp.n; ++i) {
            for (int j = 0; j < sp.m; ++j) {
                if (cost.allowed(i, j)) {
                    double c = cost.at(i, j);
                    if (std::isfinite(c) && c > cmax) cmax = c;
                }
            }
        }
    }

    // Count non-forbidden entries per row
    sp.row_ptr[0] = 0;
    for (int i = 0; i < sp.n; ++i) {
        int count = 0;
        for (int j = 0; j < sp.m; ++j) {
            if (cost.allowed(i, j)) {
                double c = cost.at(i, j);
                if (std::isfinite(c)) count++;
            }
        }
        sp.row_ptr[i + 1] = sp.row_ptr[i] + count;
    }

    // Allocate storage for sparse entries
    int nnz = sp.row_ptr[sp.n];
    sp.col_idx.resize(nnz);
    sp.values.resize(nnz);

    // Fill sparse matrix
    std::vector<int> pos(sp.n, 0);
    for (int i = 0; i < sp.n; ++i) {
        for (int j = 0; j < sp.m; ++j) {
            if (cost.allowed(i, j)) {
                double c = cost.at(i, j);
                if (std::isfinite(c)) {
                    int idx = sp.row_ptr[i] + pos[i];
                    sp.col_idx[idx] = j;
                    sp.values[idx] = maximize ? (cmax - c) : c;
                    pos[i]++;
                }
            }
        }
    }

    return sp;
}

// Shortest augmenting path from free_row using Dijkstra
bool augment(const SparseMatrix& sp, int free_row,
             std::vector<int>& x, std::vector<int>& y,
             std::vector<double>& u, std::vector<double>& v) {
    const int m = sp.m;

    std::vector<double> d(m, INF);
    std::vector<int> pred(m, UNASSIGNED);
    std::vector<bool> scanned(m, false);

    // Initialize from free_row
    for (int k = 0; k < sp.row_size(free_row); ++k) {
        int j = sp.col(free_row, k);
        double red = sp.cost(free_row, k) - u[free_row] - v[j];
        if (red < d[j]) {
            d[j] = red;
            pred[j] = -2;  // Special marker for edges from free_row
        }
    }

    int sink = UNASSIGNED;
    double min_len = INF;

    // Dijkstra's algorithm
    while (true) {
        // Find minimum unscanned node
        int j_min = UNASSIGNED;
        double d_min = INF;
        for (int j = 0; j < m; ++j) {
            if (!scanned[j] && d[j] < d_min) {
                d_min = d[j];
                j_min = j;
            }
        }

        if (j_min == UNASSIGNED || d_min == INF) break;

        scanned[j_min] = true;

        // Found unmatched column - augmenting path complete
        if (y[j_min] == UNASSIGNED) {
            sink = j_min;
            min_len = d_min;
            break;
        }

        // Extend paths from j_min through matched row
        int i = y[j_min];
        for (int k = 0; k < sp.row_size(i); ++k) {
            int j = sp.col(i, k);
            if (scanned[j]) continue;
            double new_d = d_min + sp.cost(i, k) - u[i] - v[j];
            if (new_d < d[j]) {
                d[j] = new_d;
                pred[j] = j_min;
            }
        }
    }

    if (sink == UNASSIGNED) return false;

    // Update dual variables
    u[free_row] += min_len;
    for (int j = 0; j < m; ++j) {
        if (scanned[j] && d[j] < INF) {
            v[j] += d[j] - min_len;
            if (y[j] != UNASSIGNED) {
                u[y[j]] -= d[j] - min_len;
            }
        }
    }

    // Augment path from sink back to free_row
    int j = sink;
    while (pred[j] != -2) {
        int pj = pred[j];
        int i = y[pj];
        y[j] = i;
        std::swap(x[i], j);
        j = pj;
    }
    y[j] = free_row;
    x[free_row] = j;

    return true;
}

}  // anonymous namespace

LapResult solve_lapmod(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Build sparse representation
    SparseMatrix sp = build_sparse(cost, maximize);

    // Check feasibility: each row must have at least one finite cost
    for (int i = 0; i < n; ++i) {
        if (sp.row_size(i) == 0) {
            LAP_THROW_INFEASIBLE("Infeasible: row has no finite costs");
        }
    }

    // Initialize assignment and dual variables
    std::vector<int> x(n, UNASSIGNED);  // x[i] = column matched to row i
    std::vector<int> y(m, UNASSIGNED);  // y[j] = row matched to column j
    std::vector<double> u(n, 0.0);      // row dual variables
    std::vector<double> v(m, 0.0);      // column dual variables

    // Skip column reduction phase - let shortest augmenting paths handle everything
    // This is simpler and produces optimal results. Column reduction is an
    // optimization that can speed things up but the sequential augmentation
    // approach works correctly without it.

    // Augment for each row
    for (int i = 0; i < n; ++i) {
        if (!augment(sp, i, x, y, u, v)) {
            LAP_THROW_INFEASIBLE("Could not find augmenting path for row");
        }
    }

    // Build result (0-based assignment)
    std::vector<int> assignment(n);
    for (int i = 0; i < n; ++i) {
        assignment[i] = x[i];
    }

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
