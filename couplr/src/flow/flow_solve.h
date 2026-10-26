// src/flow/flow_solve.h
// The one min-cost flow solver every design routes through.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// Successive shortest paths with Johnson potentials over a residual graph.
// Capacities are integral, so every augmentation moves a whole number of units
// and the flow returned is exact rather than a rounded interior point.
//
// The dual variables come back with the flow because they are what a
// certificate and a warm start are made of, and because a design that is not a
// one-to-one assignment has no other way to produce them.
#pragma once

#include "flow_problem.h"

#include <cstdint>
#include <functional>
#include <string>
#include <vector>

namespace lap {

struct FlowResult {
    // Aligned to prob.arcs after expansion, so block_arcs[b].rc reads a matched
    // pair straight out of it.
    std::vector<int64_t> flow;

    // One per node, normalized so potential[0] == 0. Potentials are defined up
    // to a constant per connected component; the assignment lowering needs one
    // particular representative, so fixing the gauge is part of the contract
    // rather than an implementation detail. Empty when FlowOptions asked for no
    // potentials.
    std::vector<double> potential;

    double  total_cost = 0.0;

    // What the b-flow had to move, and what it managed to move. Equal exactly
    // when the problem has a feasible flow, because the reduction to a
    // max-flow makes feasibility and saturation the same question.
    int64_t flow_sent = 0;
    int64_t flow_required = 0;

    // From solver_status_values(): "optimal", "partial", "infeasible",
    // "iteration_limit" or "interrupted". Derived from the termination state,
    // never asserted.
    std::string status = "infeasible";

    int64_t n_augmentations = 0;
};

// One search of the augmentation loop, as the search itself saw it. This is
// the state an animation needs and a solver has no other reason to produce, so
// it is written only when a caller asks for it.
//
// `labelled` names the nodes the search gave a finite distance, and `dist` is
// their labels in the same order. `potential` is the whole potential vector
// after the shift that search made, which is what a frame has to show: the
// shift is what keeps the reduced cost of an arc the search stopped short of
// from going negative.
//
// `pred_arcs[i]` is the arc labelled node i was reached by, -1 for the node
// the search started from, which together with `labelled` is the shortest-path
// tree as it stood when the search stopped.
//
// `path_arcs` are indices into `prob.arcs`, ordered from `source` to `sink`,
// and `path_forward[i]` says whether the augmentation pushed along that arc or
// pushed flow back off it. A search that reached no deficit node reports
// `sink = -1`, `units = 0` and an empty path; it still carries its labels,
// because what it failed to reach is the interesting part.
struct FlowStep {
    int32_t source = -1;
    int32_t sink   = -1;
    double  reach  = 0.0;
    int64_t units  = 0;

    std::vector<int32_t> labelled;
    std::vector<double>  dist;
    std::vector<int64_t> pred_arcs;
    std::vector<char>    pred_forward;
    std::vector<double>  potential;
    std::vector<int64_t> path_arcs;
    std::vector<char>    path_forward;
};

// The per-search record of one solve. `potential_initial` is the potential the
// first search started from, so a renderer has a frame to open on.
struct FlowTrace {
    std::vector<double>   potential_initial;
    std::vector<FlowStep> steps;
};

struct FlowOptions {
    // Zero threshold for the complementary-slackness repair a warm start needs.
    double tol = 1e-12;

    // Dijkstra relaxation slack: an arc relaxes when
    // dist[tail] + cbar + relax_eps < dist[head]. The value decides which of
    // several equally-cheap shortest paths is taken and therefore which of
    // several optimal flows comes back, so it belongs to the caller rather than
    // to the solver: two callers with different predicates give different but
    // equally optimal answers, and neither is free to adopt the other's.
    double relax_eps = 1e-18;

    // 0 derives the bound from the problem: every augmentation moves at least
    // one unit, so flow_required augmentations suffice and no more are
    // possible.
    int64_t max_augmentations = 0;

    bool return_potentials = true;

    // Where to write the per-search record. Null asks for none, and costs the
    // solve nothing beyond one null check per augmentation.
    FlowTrace* trace = nullptr;

    // Asked once every `check_every` augmentations, and once per pass of the
    // starting potential relaxation. Returning true stops the loop where it
    // stands, and the solve comes back with status "interrupted" carrying the
    // flow as far as it got: feasible on every arc bound, short of the b-flow
    // the balances asked for. An empty function is no check at all.
    //
    // Stopping and failing are different answers, which is why this returns a
    // bool rather than throwing. A caller that does want to unwind -- an R
    // binding turning a user interrupt into an R condition -- throws from
    // inside the function instead, and the solver is transparent to it.
    std::function<bool()> should_stop;

    // How many augmentations pass between two calls of should_stop. One
    // augmentation is one Dijkstra over the residual graph, so the cadence
    // trades responsiveness against the cost of the call itself.
    int64_t check_every = 32;
};

// Solve `prob`, expanding its blocks in place if that has not happened yet.
//
// Throws lap::DimensionException on a malformed problem. A problem with no
// feasible flow is not malformed: it comes back with status "partial" or
// "infeasible" and with the maximum flow that could be placed.
FlowResult solve_min_cost_flow(FlowProblem& prob,
                               const FlowOptions& opts = FlowOptions());

}  // namespace lap
