// src/flow/flow_assign.cpp
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.

#include "flow_assign.h"

#include "flow_compile.h"
#include "flow_problem.h"

#include <cstddef>
#include <utility>
#include <vector>

namespace lap {

AssignmentFlow solve_assignment_flow(const CostOracle&  costs,
                                     const FlowOptions& opts) {
    AssignmentFlow out;
    out.match.assign(static_cast<std::size_t>(costs.nrow()), -1);

    CompiledDesign design = compile_one_to_one(costs, std::vector<CategoryConstraint>());
    FlowResult res = solve_min_cost_flow(design.problem, opts);

    // compile_one_to_one() emits exactly one block, and expand_blocks() records
    // the (i, j) behind every arc it emitted, so the matching is read off the
    // block's own arc range without knowing where in the arc array it landed.
    const BlockArcRange& block = design.problem.block_arcs[0];
    for (int64_t k = 0; k < block.n_arcs; ++k) {
        if (res.flow[static_cast<std::size_t>(block.first_arc + k)] <= 0) continue;
        const std::pair<int32_t, int32_t>& rc = block.rc[static_cast<std::size_t>(k)];
        out.match[static_cast<std::size_t>(rc.first)] = static_cast<int>(rc.second);
    }

    // Every unit of flow crosses exactly one pair arc, so the flow placed is
    // the number of rows matched.
    out.n_matched = res.flow_sent;
    out.status    = res.status;
    out.potential = std::move(res.potential);
    return out;
}

}  // namespace lap
