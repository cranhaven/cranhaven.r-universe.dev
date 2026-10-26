// src/solvers/solve_line_metric.h
// Pure C++ 1D assignment solver (line metric) - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include <vector>
#include <string>

namespace lap {

// Solve 1D assignment problem using line metric (optimal O(n log n) algorithm)
// This is a specialized solver for 1D point matching on the real line
//
// Parameters:
//   x: Vector of positions for left points (to be matched)
//   y: Vector of positions for right points (available positions)
//   cost_type: Distance metric - "L1" (Manhattan) or "L2" (squared Euclidean)
//   maximize: If true, find maximum weight matching (costs negated internally)
//
// Returns:
//   LapResult with 0-based assignment and total cost
//
// Algorithm:
//   - Square case (n == m): Sort both arrays, match in order - O(n log n)
//   - Rectangular (n < m): Dynamic programming - O(n * m)
//
// Throws:
//   DimensionException if n > m (more left points than right points)
//   DimensionException if x or y are empty
LapResult solve_line_metric(const std::vector<double>& x,
                            const std::vector<double>& y,
                            const std::string& cost_type = "L1",
                            bool maximize = false);

}  // namespace lap
