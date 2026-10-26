// src/solvers/solve_csa.cpp
// Cost-scaling assignment entry point - NO Rcpp dependencies

#include "solve_csa.h"

namespace lap {

// Goldberg and Kennedy chose a scale factor of 10 and report running times
// within a factor of 2 for factors between 4 and 40.
static constexpr double CSA_SCALE_FACTOR = 10.0;

LapResult solve_csa(const CostMatrix& cost, bool maximize, EpsilonScalingStats* stats) {
    EpsilonScalingOptions options;
    options.alpha = CSA_SCALE_FACTOR;
    options.row_search = RowSearch::FourthBest;
    return solve_epsilon_scaling(cost, maximize, options, stats);
}

}  // namespace lap
