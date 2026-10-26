// src/solvers/greedy_matching.h
// Pure C++ Greedy Matching - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include <string>

namespace lap {

// Main dispatcher for greedy matching
// strategy: "sorted", "row_best", or "pq"
LapResult greedy_matching(const CostMatrix& cost, bool maximize = false,
                          const std::string& strategy = "row_best");

// Individual greedy strategies
LapResult greedy_matching_sorted(const CostMatrix& cost, bool maximize = false);
LapResult greedy_matching_row_best(const CostMatrix& cost, bool maximize = false);
LapResult greedy_matching_pq(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
