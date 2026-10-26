// src/solvers/solve_sinkhorn.cpp
// Pure C++ Sinkhorn-Knopp Algorithm for Entropy-Regularized Optimal Transport
// Reference: Cuturi (2013) "Sinkhorn Distances: Lightspeed Computation of
//            Optimal Transport"
// Complexity: O(n² / ε²) iterations, O(n²) per iteration

#include "solve_sinkhorn.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>
#include <tuple>
#include <cstddef>

namespace lap {

SinkhornResult solve_sinkhorn(
    const CostMatrix& cost,
    double lambda,
    double tol,
    int max_iter,
    const std::vector<double>& r_weights,
    const std::vector<double>& c_weights
) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0 || m == 0) {
        SinkhornResult result;
        result.converged = true;
        result.iterations = 0;
        result.lambda = lambda;
        return result;
    }

    // Validate parameters
    if (lambda <= 0.0) {
        LAP_THROW("lambda must be positive");
    }
    if (tol <= 0.0) {
        LAP_THROW("tol must be positive");
    }
    if (max_iter <= 0) {
        LAP_THROW("max_iter must be positive");
    }

    // Set up marginals (default: uniform)
    std::vector<double> r(n), c(m);

    if (!r_weights.empty()) {
        if (static_cast<int>(r_weights.size()) != n) {
            LAP_THROW("r_weights must have length equal to number of rows");
        }
        double sum_r = 0.0;
        for (int i = 0; i < n; ++i) {
            if (r_weights[i] < 0.0) {
                LAP_THROW("r_weights must be non-negative");
            }
            r[i] = r_weights[i];
            sum_r += r[i];
        }
        if (sum_r <= 0.0) {
            LAP_THROW("r_weights must sum to a positive value");
        }
        // Normalize
        for (int i = 0; i < n; ++i) r[i] /= sum_r;
    } else {
        for (int i = 0; i < n; ++i) r[i] = 1.0 / n;
    }

    if (!c_weights.empty()) {
        if (static_cast<int>(c_weights.size()) != m) {
            LAP_THROW("c_weights must have length equal to number of columns");
        }
        double sum_c = 0.0;
        for (int j = 0; j < m; ++j) {
            if (c_weights[j] < 0.0) {
                LAP_THROW("c_weights must be non-negative");
            }
            c[j] = c_weights[j];
            sum_c += c[j];
        }
        if (sum_c <= 0.0) {
            LAP_THROW("c_weights must sum to a positive value");
        }
        // Normalize
        for (int j = 0; j < m; ++j) c[j] /= sum_c;
    } else {
        for (int j = 0; j < m; ++j) c[j] = 1.0 / m;
    }

    // Compute K = exp(-lambda * C)
    // Use numerical stabilization for large lambda
    std::vector<double> K(static_cast<std::size_t>(n) * m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double cij = cost.at(i, j);
            if (!std::isfinite(cij)) {
                // Treat NA/Inf as very high cost (very low probability)
                cij = 1e10;
            }
            K[static_cast<std::size_t>(i) * m + j] = std::exp(-lambda * cij);
        }
    }

    // Initialize scaling vectors
    std::vector<double> u(n, 1.0);
    std::vector<double> v(m, 1.0);

    // Sinkhorn iterations
    bool converged = false;
    int iter = 0;

    // Temporary vectors for computing Kv and K'u
    std::vector<double> Kv(n), Ktu(m);

    for (iter = 0; iter < max_iter; ++iter) {
        // Save old u for convergence check
        std::vector<double> u_old = u;

        // Update u: u = r / (K * v)
        for (int i = 0; i < n; ++i) {
            double sum = 0.0;
            for (int j = 0; j < m; ++j) {
                sum += K[i * m + j] * v[j];
            }
            Kv[i] = sum;
            u[i] = (sum > 1e-300) ? r[i] / sum : r[i] * 1e300;
        }

        // Update v: v = c / (K' * u)
        for (int j = 0; j < m; ++j) {
            double sum = 0.0;
            for (int i = 0; i < n; ++i) {
                sum += K[i * m + j] * u[i];
            }
            Ktu[j] = sum;
            v[j] = (sum > 1e-300) ? c[j] / sum : c[j] * 1e300;
        }

        // Check convergence: ||u - u_old|| / ||u||
        double diff = 0.0, norm = 0.0;
        for (int i = 0; i < n; ++i) {
            double d = u[i] - u_old[i];
            diff += d * d;
            norm += u[i] * u[i];
        }

        if (norm > 0 && std::sqrt(diff / norm) < tol) {
            converged = true;
            break;
        }
    }

    // Compute transport plan P = diag(u) * K * diag(v)
    std::vector<std::vector<double>> P(n, std::vector<double>(m, 0.0));
    double transport_cost = 0.0;

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double pij = u[i] * K[static_cast<std::size_t>(i) * m + j] * v[j];
            P[i][j] = pij;
            double cij = cost.at(i, j);
            if (std::isfinite(cij)) {
                transport_cost += pij * cij;
            }
        }
    }

    // Round to hard assignment
    std::vector<int> assignment = sinkhorn_round(P);

    // Build result
    SinkhornResult result;
    result.transport_matrix = std::move(P);
    result.assignment = std::move(assignment);
    result.total_cost = transport_cost;
    result.u = std::move(u);
    result.v = std::move(v);
    result.converged = converged;
    // On convergence `iter` is the 0-based iteration at break (count = iter+1);
    // on non-convergence the loop ran the full max_iter (iter == max_iter).
    result.iterations = converged ? (iter + 1) : iter;
    result.lambda = lambda;

    return result;
}

std::vector<int> sinkhorn_round(const std::vector<std::vector<double>>& transport_matrix) {
    if (transport_matrix.empty()) {
        return std::vector<int>();
    }

    const int n = static_cast<int>(transport_matrix.size());
    const int m = static_cast<int>(transport_matrix[0].size());

    // Greedy rounding: iteratively assign row to its most likely column
    std::vector<int> match(n, -1);  // 0-based, -1 = unmatched
    std::vector<bool> col_used(m, false);

    // Create list of (probability, row, col) and sort descending
    std::vector<std::tuple<double, int, int>> entries;
    entries.reserve(n * m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            entries.emplace_back(transport_matrix[i][j], i, j);
        }
    }

    std::sort(entries.begin(), entries.end(),
              [](const auto& a, const auto& b) {
                  return std::get<0>(a) > std::get<0>(b);
              });

    std::vector<bool> row_assigned(n, false);
    int assigned = 0;
    int max_assignments = std::min(n, m);

    for (const auto& e : entries) {
        int i = std::get<1>(e);
        int j = std::get<2>(e);

        if (!row_assigned[i] && !col_used[j]) {
            match[i] = j;  // 0-based
            row_assigned[i] = true;
            col_used[j] = true;
            ++assigned;
            if (assigned == max_assignments) break;
        }
    }

    return match;
}

}  // namespace lap
