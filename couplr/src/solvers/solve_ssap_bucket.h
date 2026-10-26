// src/solvers/solve_ssap_bucket.h
// Pure C++ SSAP with Dial's bucket queue - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Successive shortest path with Dial's bucket queue for integer costs
// Uses min-cost flow formulation on bipartite graph
// Optimal for problems with integer or near-integer costs
LapResult solve_ssap_bucket(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
