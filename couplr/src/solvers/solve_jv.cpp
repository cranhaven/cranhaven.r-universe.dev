// src/solvers/solve_jv.cpp
// Pure C++ Jonker-Volgenant LAP solver - NO Rcpp dependencies.
// The solve is solve_jv_duals(): detail::jv_core() produces the potentials
// whether or not anyone asks for them, so the two entry points are one body
// and this one drops the duals on the way out.

#include "solve_jv.h"
#include "solve_jv_duals.h"
#include <utility>

namespace lap {

LapResult solve_jv(const CostMatrix& cost, bool maximize) {
    return std::move(solve_jv_duals(cost, maximize).solution);
}

LapResult solve_jv(const LazyCostMatrix& cost) {
    return std::move(solve_jv_duals(cost).solution);
}

}  // namespace lap
