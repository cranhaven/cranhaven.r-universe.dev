// src/solvers/solve_sinkhorn.h
// Pure C++ Sinkhorn-Knopp solver for Entropy-Regularized Optimal Transport - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include <vector>

namespace lap {

// Result structure for Sinkhorn algorithm
struct SinkhornResult {
    std::vector<std::vector<double>> transport_matrix;  // Soft assignment matrix P
    std::vector<int> assignment;                        // Rounded hard assignment (0-based)
    double total_cost = 0.0;                           // Transport cost <C, P>
    std::vector<double> u;                             // Row scaling factors
    std::vector<double> v;                             // Column scaling factors
    bool converged = false;                            // Convergence flag
    int iterations = 0;                                // Number of iterations used
    double lambda = 10.0;                              // Regularization parameter
};

// Sinkhorn-Knopp algorithm for entropy-regularized optimal transport
//
// Solves: P* = argmin_P <C, P> - (1/lambda) * H(P)
//         subject to P1 = r, P'1 = c (marginal constraints)
//
// The solution has form P = diag(u) * K * diag(v) where K = exp(-lambda * C)
// Algorithm alternates scaling rows and columns until convergence.
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   lambda: Regularization parameter (higher = more peaked, closer to discrete)
//   tol: Convergence tolerance (default: 1e-9)
//   max_iter: Maximum iterations (default: 1000)
//   r_weights: Row marginals (default: uniform 1/n)
//   c_weights: Column marginals (default: uniform 1/m)
//
// Returns:
//   SinkhornResult with transport matrix, rounded assignment, and metadata
//
// Reference:
//   Cuturi (2013) "Sinkhorn Distances: Lightspeed Computation of Optimal Transport"
//   Complexity: O(n² / ε²) iterations, O(n²) per iteration
SinkhornResult solve_sinkhorn(
    const CostMatrix& cost,
    double lambda = 10.0,
    double tol = 1e-9,
    int max_iter = 1000,
    const std::vector<double>& r_weights = {},
    const std::vector<double>& c_weights = {}
);

// Round soft assignment (transport plan) to hard assignment
// Uses greedy rounding: iteratively assign highest probability edges
//
// Parameters:
//   transport_matrix: n x m matrix of soft assignments (probabilities)
//
// Returns:
//   0-based assignment vector (length n), -1 for unmatched
std::vector<int> sinkhorn_round(const std::vector<std::vector<double>>& transport_matrix);

}  // namespace lap
