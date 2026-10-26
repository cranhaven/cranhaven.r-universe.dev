// src/solvers/jv_core.h
// Pure C++ shortest-augmenting-path Hungarian core.
// Shared by solve_jv, solve_jv_duals, and solve_hungarian.
// NO Rcpp dependencies.
#pragma once

#include "../core/lap_types.h"
#include "../core/lap_lazy_types.h"
#include <vector>

namespace lap {
namespace detail {

struct JvCoreOpts {
    // Run the LAPJV pre-stages (column reduction + augmenting row reduction)
    // before the shortest-path phase. solve_jv enables them; solve_hungarian
    // asks for the plain augmenting-path solve and leaves them off.
    bool use_warm_start = false;
};

struct JvCoreResult {
    std::vector<int> assignment;  // size nrow, 0-based, -1 = unmatched
    std::vector<double> u;        // size nrow, row potentials
    std::vector<double> v;        // size ncol, column potentials
};

// Solve LAP on a prepared cost source.
// Precondition: `work` came from prepare_for_solve() (CostMatrix) or was
// constructed already-negated/caliper-aware (LazyCostMatrix) -- forbidden
// cells read as BIG via at(), negated if the caller wants maximize, padded
// if needed. Caller is responsible for feasibility checks
// (ensure_each_row_has_option), dimension checks (n <= m), and computing
// total_cost from the original matrix.
//
// Templated on cost-source type so the dense (CostMatrix) and lazy
// (LazyCostMatrix) paths share one algorithm body; explicitly instantiated
// for both in jv_core.cpp. CostMatrix behavior/performance is unchanged --
// this is a source-level generalization, not a new dense implementation.
template <typename CostSourceT>
JvCoreResult jv_core(const CostSourceT& work, const JvCoreOpts& opts = {});

extern template JvCoreResult jv_core<CostMatrix>(const CostMatrix&, const JvCoreOpts&);
extern template JvCoreResult jv_core<LazyCostMatrix>(const LazyCostMatrix&, const JvCoreOpts&);

}  // namespace detail
}  // namespace lap
