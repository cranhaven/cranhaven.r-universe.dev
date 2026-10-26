// src/flow/flow_path.h
// A sequence of matchings that differ in one knob, solved as one loop.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// A design path answers "what does the matching look like as the caliper
// opens", and the honest way to get it is to solve the problem once per value.
// The values are not independent problems, though. Ascending, each one is the
// last one with pairs added: every arc the previous point placed is still an
// arc, every capacity is where it was, and every cost already reported is
// unchanged. So the flow the previous point ended on is a feasible flow for
// this one, and the only thing the widening can have broken is dual
// feasibility on the pairs it admitted.
//
// That is the pricing loop's own question. A path point is therefore
// flow_implicit.h's loop run again over the problem the last point left behind,
// with the pairs supplied by the widening instead of by a pricing round --
// continue_implicit_assignment(), and nothing else. What this file adds is the
// sweep: the order the values are visited in, the structure that is built once
// and put to every point, and the record of what each point cost.
//
// **The direction is a correctness requirement, not a preference.** Descending
// withdraws pairs, and a pair withdrawn may be one the incumbent flow is
// standing on, which breaks primal feasibility rather than dual feasibility and
// needs a repair phase this loop does not have. A caller who wrote a descending
// sweep meant something the mechanism does not do, so the sweep says so rather
// than sorting the values behind their back.
#pragma once

#include "../core/lap_error.h"
#include "flow_candidates.h"
#include "flow_implicit.h"
#include "flow_problem.h"
#include "flow_row_search.h"

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <sstream>
#include <string>
#include <vector>

namespace lap {

struct PathOptions {
    // Handed to every point. The knobs are the loop's, and one point is one
    // loop, so there is nothing here a point may set for itself.
    ImplicitOptions implicit;
};

// One point: the value the knob held, and the answer at it.
struct PathPoint {
    double value = 0.0;

    // From the loop: "optimal", "infeasible" with a witness, or a limit. A
    // point that comes back infeasible is an answer about the value rather
    // than a failure of the sweep, and the sweep carries on to the next one.
    std::string status = "infeasible";

    std::vector<int> match;
    double  total_cost = 0.0;
    int64_t n_matched  = 0;

    CertificateReport certificate;
    bool certified = false;

    std::vector<ImplicitRound> rounds;

    // The state the point left behind, and what reaching it cost. The two
    // counts are this point's own: the candidate set and the arc array carry
    // across points, so what a point is charged is what it added.
    int64_t candidate_edges = 0;   // pairs the set holds after this point
    int64_t block_arcs      = 0;   // of those, the ones that are arcs
    int64_t pairs_added     = 0;   // pairs this point put into the set
    int64_t edges_evaluated = 0;   // pairs this point computed a cost for
    double  seconds         = 0.0;

    DeficiencySet witness;
    bool witness_certified = false;
};

struct PathResult {
    std::vector<PathPoint> points;
    int64_t seed_width      = 0;   // columns the first point's seed gave a row
    int64_t possible_edges  = 0;   // nrow * ncol
    int64_t edges_evaluated = 0;   // over the whole path
    int64_t candidate_edges = 0;   // pairs the set held at the last point
};

namespace path_detail {

// std::to_string() gives a double six decimals whatever it holds, which turns
// a caliper of 1e-9 into "0.000000" in the one message a caller reads to find
// out which value was wrong.
inline std::string as_text(double x) {
    std::ostringstream ss;
    ss << x;
    return ss.str();
}

// The values, checked before any of them is solved, because a sweep that
// discovers its third value is smaller than its second has already spent the
// first two.
inline void require_ascending(const std::vector<double>& values) {
    if (values.empty()) {
        LAP_THROW_DIMENSION("solve_path: no values, so there is no path");
    }
    for (std::size_t k = 0; k < values.size(); ++k) {
        if (std::isnan(values[k])) {
            LAP_THROW_DIMENSION("solve_path: value " + std::to_string(k + 1) +
                                " is not a number");
        }
        if (k > 0 && values[k] <= values[k - 1]) {
            LAP_THROW_DIMENSION(
                "solve_path: value " + std::to_string(k + 1) + " is " +
                as_text(values[k]) + " and value " + std::to_string(k) +
                " is " + as_text(values[k - 1]) +
                ", and the path solves each point from the one before it, which "
                "needs every value to be larger than the last");
        }
    }
}

}  // namespace path_detail

// Sweep `values` through `relax` and solve the problem at each one, each point
// starting from the master the previous point ended on.
//
// `src`, `prob` and `cand` are the same three objects the loop takes, and they
// are the state that carries: `relax(src, prob, value)` moves the knob on them
// and everything else stays where it is. `prob` is unexpanded on entry -- the
// first point expands it over whatever seed `cand` holds -- and holds the last
// point's master on return.
//
// The row structure is built once, before the first point, and put to every
// one. That is not an optimization to note; it is the reason a knob that moves
// admissibility can be swept at all. A ball tree bounds subtrees from the
// columns' coordinates and reads the cut off the source at query time, so
// moving the cut leaves the tree correct and rebuilding it per point would be
// paying twenty times for one answer.
template <class Source, class Relax>
PathResult solve_path(Source& src, FlowProblem& prob, CandidateSet& cand,
                      const std::vector<double>& values, Relax relax,
                      const PathOptions& opts = PathOptions()) {
    path_detail::require_ascending(values);

    PathResult out;
    out.possible_edges = src.nrow * src.ncol;
    out.points.reserve(values.size());

    RowSearch<Source> search(src);

    int64_t evaluated_before = 0;
    int64_t pairs_before = 0;

    for (std::size_t k = 0; k < values.size(); ++k) {
        relax(src, prob, values[k]);

        const implicit_detail::Clock::time_point t0 =
            implicit_detail::Clock::now();
        const ImplicitResult res =
            (k == 0)
                ? start_implicit_assignment(src, prob, cand, search, opts.implicit)
                : continue_implicit_assignment(src, prob, cand, search, opts.implicit);
        const double seconds = implicit_detail::seconds_since(t0);

        PathPoint pt;
        pt.value       = values[k];
        pt.status      = res.status;
        pt.match       = res.match;
        pt.total_cost  = res.total_cost;
        pt.certificate = res.certificate;
        pt.certified   = res.certified;
        pt.rounds      = res.rounds;
        pt.seconds     = seconds;

        for (int j : pt.match) {
            if (j >= 0) ++pt.n_matched;
        }

        pt.candidate_edges = res.candidate_edges;
        pt.block_arcs      = prob.block_arcs.empty() ? 0 : prob.block_arcs[0].n_arcs;
        pt.pairs_added     = res.candidate_edges - pairs_before;
        pt.edges_evaluated = res.edges_evaluated - evaluated_before;
        pairs_before       = res.candidate_edges;
        evaluated_before   = res.edges_evaluated;

        pt.witness           = res.witness;
        pt.witness_certified = res.witness_certified;

        // Every point runs the loop over one candidate set, so the seed the
        // first point sized is the seed the whole path was built on.
        if (k == 0) out.seed_width = res.seed_width;

        out.points.push_back(std::move(pt));
    }

    out.candidate_edges = cand.n_arcs();
    out.edges_evaluated = cand.edges_evaluated();
    return out;
}

}  // namespace lap
