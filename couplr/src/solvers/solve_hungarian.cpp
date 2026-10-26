// src/solvers/solve_hungarian.cpp
// Classic Hungarian in shortest-augmenting-path form: the Jonker-Volgenant core
// with the LAPJV pre-stages disabled, which is what gets you the "textbook"
// O(n^3) Hungarian. solve_jv calls the same core with the pre-stages enabled.

#include "solve_hungarian.h"
#include "solve_jv_duals.h"
#include <utility>

namespace lap {

LapResult solve_hungarian(const CostMatrix& cost, bool maximize) {
    return std::move(solve_jv_duals(cost, maximize, /*warm_start=*/false).solution);
}

}  // namespace lap
