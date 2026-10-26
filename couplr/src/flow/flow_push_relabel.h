// src/flow/flow_push_relabel.h
// Cost-scaling push-relabel over the same FlowProblem the SSP solver reads.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// Goldberg & Tarjan (1990), "Finding minimum-cost circulations by successive
// approximation". A sequence of eps-optimal flows, each phase dividing eps:
// the arcs the smaller eps no longer admits are saturated, which restores
// eps-optimality and breaks conservation, and conservation is restored by
// pushing excess along admissible arcs and relabelling a node that holds excess
// with no admissible arc out of it.
//
// This is a second solver for the same problem, not a second name for the
// first. solve_min_cost_flow() reaches the optimum through shortest augmenting
// paths, one path per augmentation, and holds primal feasibility throughout.
// This one holds dual near-feasibility throughout and reaches primal
// feasibility only at the end of each phase, and its two operations are local:
// a push reads one arc, a relabel reads one node's arcs.
#pragma once

#include "flow_problem.h"
#include "flow_solve.h"

namespace lap {

// One scaling phase, as the phase itself saw it: the eps it ran at, how much
// work it took, the potentials it settled on and the flow that went with them.
// This is the state an animation needs and a solver has no other reason to
// produce, so it is written only when a caller asks for it.
struct PRPhase {
    double  eps         = 0.0;
    int64_t n_saturated = 0;
    int64_t n_pushes    = 0;
    int64_t n_relabels  = 0;

    std::vector<double>  potential;  // gauged like FlowResult::potential
    std::vector<int64_t> flow;       // aligned to prob.arcs
};

// The per-phase record of one solve, and the eps the first phase started at.
struct PRTrace {
    double               eps_start = 0.0;
    std::vector<PRPhase> phases;
};

// Solve `prob`, expanding its blocks in place if that has not happened yet.
//
// The returned FlowResult is the same contract solve_min_cost_flow() returns,
// including the potential gauge and the status vocabulary, so a caller can
// swap one for the other. FlowOptions::relax_eps and FlowOptions::trace are
// not read: this search has no shortest path to break a tie in and no
// per-augmentation record to write.
//
// Throws lap::DimensionException on a malformed problem. A problem with no
// feasible flow comes back with status "partial" or "infeasible" and the
// maximum flow that could be placed.
//
// `trace` is where to write the per-phase record; null asks for none and costs
// the solve one null check per phase.
FlowResult solve_min_cost_flow_push_relabel(
    FlowProblem&       prob,
    const FlowOptions& opts  = FlowOptions(),
    PRTrace*           trace = nullptr);

}  // namespace lap
