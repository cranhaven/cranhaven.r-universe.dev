// src/flow/flow_assign.h
// The assignment problem as a flow: compile it, solve it, read the matching
// back out. Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// Five solvers each carried their own residual-graph min-cost flow over the
// same three-layer network, with their own {to, rev, cap, cost} edge struct,
// their own add_edge pushing a zero-capacity reverse arc, and their own
// Dijkstra. What actually differs between them is upstream and downstream of
// that: how the cost matrix is prepared, which relaxation predicate breaks a
// tie between two equally-cheap paths, and what a shortfall is called. The
// network and the search are the same in all five.
//
// This is that shared part, written once. A caller passes its cost source and
// its FlowOptions and gets a matching; the predicate stays the caller's,
// because equal cost is not equal output and two callers with different
// predicates return different, equally optimal answers.
#pragma once

#include "flow_oracle.h"
#include "flow_solve.h"

#include <cstdint>
#include <string>
#include <vector>

namespace lap {

struct AssignmentFlow {
    // One column per row, -1 where the row went unmatched. Sized to the
    // source's row count, in the source's own row order.
    std::vector<int> match;

    // Rows the solve placed. Short of the row count exactly when no complete
    // matching exists over the admissible pairs, which is the condition each
    // caller reports in its own words.
    int64_t n_matched = 0;

    // As solve_min_cost_flow() derived it, from solver_status_values().
    std::string status = "infeasible";

    // Node potentials of the compiled problem, gauge potential[FLOW_SOURCE] = 0.
    // Empty when the caller asked for none.
    std::vector<double> potential;
};

// Solve the unit-capacity bipartite assignment over `costs`: every row matched
// to at most one column, every column to at most one row, at minimum total
// cost. Pairs the source forbids and pairs whose cost is not finite are not
// arcs, so a row with no admissible column goes unmatched rather than making
// the problem malformed.
AssignmentFlow solve_assignment_flow(const CostOracle&  costs,
                                     const FlowOptions& opts = FlowOptions());

}  // namespace lap
