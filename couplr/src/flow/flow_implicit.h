// src/flow/flow_implicit.h
// The outer loop: an assignment over a complete implicit graph, solved by
// generating the pairs it turns out to need.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The problem is stated over every pair of a cost source and solved over a
// fraction of them. Seven steps:
//
//   1. Give every row some admissible columns to start from.
//   2. Solve that restricted master.
//   3. Read its potentials as assignment duals u, v, on the tight face the
//      assignment LP asks for rather than the one min-cost flow settles for.
//   4. Price the pairs the master does not hold: cbar_ij = c_ij - u_i - v_j.
//   5. Add the pairs that price below -tol, and only those.
//   6. Warm start from the incumbent flow and re-solve.
//   7. Repeat until nothing prices below -tol.
//
// At that point the duals are feasible for every pair of the complete implicit
// problem to within tol, and the restricted answer is optimal for it to within
// n * tol. That is what separates this from approximate k-nearest matching,
// which solves the same restricted master and stops at step 2.
//
// A loop that certifies closes the tolerance as it goes. Each round recovers
// exact potentials for the master over the pairs it holds, in exact arithmetic
// (lap_exact_potentials.h), which costs the candidate set and not the grid, and
// prices the omitted pairs against them at zero: the double pass decides the
// pairs clear of zero and the exact sign the few within rounding of it, in the
// same sweep. A pair whose reduced cost is exactly negative goes into the
// candidate set; when there is none, the potentials are exactly feasible for
// every admissible pair and the certificate is exact, with no n * tol left in
// it. Every round still adds a pair the master did not hold, so the loop still
// terminates. A master the recovery finds not exactly optimal over its own
// pairs -- the flow solver works in doubles too -- is priced at -tol that
// round, as a loop that does not certify prices every round.
//
// The loop lives in C++ because it owns the FlowProblem, the candidate set and
// the cost source, none of which have an R representation, and because it
// crosses to R once rather than once per round.
//
// **Feasibility is a different question and gets a different phase.** A master
// that comes back short of the required flow has no dual solution to price
// with, so a round that falls short hands over to flow_feasibility.h: Hall's
// witness names the rows that could not be matched and the re-seed aims at the
// columns that can repair them, or reports that no arc set over this source
// admits a complete matching and says why.
//
// The assignment -- one block, every row carrying one unit, every column at
// most one -- is the design Hall's condition answers feasibility for and the LP
// lap_certify.h certifies, and solve_implicit_assignment() runs it. Any other
// design compiled onto one block of pair arcs runs the same rounds through
// solve_implicit_design(), which prices against the flow's own potentials and
// answers a shortfall with a maximum-flow cut; see the section above it.
#pragma once

#include "../core/lap_certify.h"
#include "../core/lap_cost_source.h"
#include "../core/lap_exact_potentials.h"
#include "../core/lap_error.h"
#include "../core/lap_hall.h"
#include "flow_candidates.h"
#include "flow_certify.h"
#include "flow_feasibility.h"
#include "flow_pricing.h"
#include "flow_problem.h"
#include "flow_row_search.h"
#include "flow_solve.h"

#include <algorithm>
#include <cfloat>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace lap {

// Columns the seed gives a deficient row when the caller names no width.
//
// The loop's floor is two rounds: one to seed, one to sweep the omitted pairs,
// find nothing that prices in, and certify. It reaches that floor once the seed
// already holds every pair that can end up tight, and a seed short of it buys
// its missing pairs a round at a time, each round costing a further sweep and a
// further master solve.
//
// Where that floor is reached is not readable off the shape of the problem.
// Over ncol from 3.3e3 to 6.7e4 it ran from 20 columns to 230, and the data
// moved it further than the size did: eight-dimensional Gaussian coordinates
// reached it at 21, 20, 32 and 68 columns across those sizes, while the same
// sizes over six Gaussian coordinates and two binary ones needed 20, 40, 40 and
// 80 -- covariates that tie leave many columns at one distance from a row, and
// a row then has to hold more of them. Nothing the loop can read before it
// seeds separates those cases.
//
// So the seed is sized to clear the floor rather than to sit on it, which is
// the cheap side to miss on wherever a sweep is expensive. Six columns per
// doubling of ncol clears every measured case whose pairs the ball bound cannot
// prune, and costs 2% to 21% over the width that reached the floor exactly; the
// seeds below the floor ran 30% to 150% over. A source with fewer columns than
// the rule asks for takes all of them.
//
// The exception runs the other way and is left alone deliberately. Where the
// bound does prune -- two coordinates, a few percent of the grid evaluated --
// a further sweep is cheap and a wider seed is not, so the fastest run sits
// below the floor rather than on it: at ncol = 6.7e4 the floor wanted 230
// columns and took 5.33 s, where 45 columns took 2.66 s over five rounds. The
// rule asks for 102 there and lands at 3.53 s. It is deliberately short of that
// floor, and it is short in the regime where the whole solve costs seconds.
inline int64_t implicit_seed_width(int64_t ncol) {
    if (ncol < 1) return 1;
    int64_t doublings = 0;
    while ((int64_t{1} << doublings) < ncol) ++doublings;  // ceil(log2(ncol))
    const int64_t width = 6 * doublings;
    return width < ncol ? (width < 1 ? 1 : width) : ncol;
}

struct ImplicitOptions {
    // Violators a row contributes per pricing round. At least one, because a
    // round that adds nothing prices the same pairs again next round.
    int keep_per_row = 5;

    // Columns the first feasibility round gives each deficient row, doubled by
    // every round that has to run again. It is also the seed: a loop started
    // from an empty candidate set spends its first round with every row
    // deficient, which is the k-nearest seed taken over the whole source.
    //
    // Zero, the default, takes it from implicit_seed_width() instead, which is
    // the rule the measurements above put behind it.
    int width = 0;

    // Zero threshold for pricing and for the certificate. A pair prices out at
    // cbar < -tol, and the loop stops when none does.
    double tol = 1e-9;

    // Guard, not a convergence bound. Each round adds at least one pair the
    // master did not hold, so the loop terminates on its own after at most
    // nrow * ncol rounds; a run that reaches this limit has found something
    // worth looking at rather than a problem that needs more rounds.
    int64_t max_rounds = 60;

    // Whether to assemble the certificate on termination. The pricing round is
    // run either way -- it is what stops the loop -- so what this buys is the
    // other half of the scan, over the pairs the master holds, and the
    // conclusion drawn from the two together. Off, the answer carries the
    // status the master terminated on and no proof.
    bool certify = true;

    // Handed to every master solve. return_potentials is forced on: the duals
    // are what the next round prices with.
    FlowOptions flow;
};

// One round of the loop: the master it solved, and what it did about the
// answer. This is what edges_evaluated is computed from, and it is the per-step
// state a trace layer reads, so it is a record rather than a private counter.
struct ImplicitRound {
    enum class Kind {
        priced,    // the master's omitted pairs were priced
        reseeded   // the master came back short and the arc set grew toward it
    };

    int64_t round = 0;
    Kind    kind  = Kind::priced;

    // The master this round solved.
    std::string master_status;
    int64_t candidate_pairs = 0;   // pairs the candidate set held
    int64_t block_arcs      = 0;   // of those, the ones that became arcs
    int64_t flow_sent       = 0;
    int64_t flow_required   = 0;
    double  master_cost     = 0.0;
    double  master_seconds  = 0.0;

    // What the round then did. `min_reduced_cost` is over the omitted pairs and
    // stays infinite on a reseed round, which prices nothing.
    double  min_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t n_violators      = 0;
    // Whether the round priced at zero against exact potentials, or at -tol
    // against the master's doubles.
    bool    exact_pricing    = false;
    int64_t n_evaluated      = 0;   // pairs this round computed a cost for
    int64_t pairs_added      = 0;
    int64_t arcs_added       = 0;
    double  pricing_seconds  = 0.0;

    // How far the master's own potentials sat off the tight face before
    // tighten_matched_duals() put them back on it. A cold master leaves it at
    // rounding; a warm one is free to leave it anywhere, which is what the
    // projection is there for. Zero for a design, whose potentials are priced
    // as they come.
    double matched_slack = 0.0;
};

// What every run of the loop reports about the search, whatever the design.
struct ImplicitSearch {
    // From solver_status_values().
    std::string status = "infeasible";

    std::vector<ImplicitRound> rounds;

    // What the search cost, in the terms the public surface reports.
    int64_t seed_width      = 0;   // columns the first round gave a row
    int64_t candidate_edges = 0;   // pairs the candidate set ended up holding
    int64_t possible_edges  = 0;   // nrow * ncol
    int64_t edges_evaluated = 0;   // pairs a cost was computed for, all rounds
};

struct ImplicitResult : ImplicitSearch {
    // The loop either matches every row, reports "infeasible" with the witness
    // saying no arc set over this source could, or hits a limit: its own
    // max_rounds, or a master that ran out of augmentations, both reported as
    // "iteration_limit".

    // One column per row, -1 unmatched, and the duals the last master produced.
    // Empty when no master ever reached a matching.
    std::vector<int>    match;
    std::vector<double> u;
    std::vector<double> v;
    double total_cost = 0.0;

    // The certificate for the COMPLETE implicit problem, assembled from the
    // scan over the master's own pairs and the scan over the pairs it omits.
    // Meaningful when the status is "optimal".
    CertificateReport certificate;
    bool certified = false;

    // Why no arc set over this source admits a complete matching, and the
    // witness re-checked against the full source. Populated exactly when the
    // status is "infeasible".
    DeficiencySet witness;
    bool witness_certified = false;
};

namespace implicit_detail {

using Clock = std::chrono::steady_clock;

inline double seconds_since(const Clock::time_point& t0) {
    return std::chrono::duration<double>(Clock::now() - t0).count();
}

// The pricing round read as a scan of the omitted pairs, which is the half of
// the complete scan the certificate cannot take for itself.
inline ReducedCostScan omitted_scan(const BlockPricing& priced) {
    ReducedCostScan scan;
    scan.min_reduced_cost = priced.min_reduced_cost;
    scan.proven_floor = priced.proven_floor;
    scan.arg_i = priced.arg_i;
    scan.arg_j = priced.arg_j;
    scan.n_violations = priced.n_violators;
    scan.n_admissible = priced.n_evaluated;
    return scan;
}

// Whether any matched pair prices strictly below zero against `u` and `v`,
// which is the state tighten_matched_duals() exists to leave behind.
template <class Source>
double matched_slack(const Source& src, const std::vector<int>& match,
                     const std::vector<double>& u, const std::vector<double>& v) {
    double worst = 0.0;
    for (std::size_t i = 0; i < match.size(); ++i) {
        const int j = match[i];
        if (j < 0) continue;
        double c = 0.0;
        if (!cost_if_allowed(src, static_cast<int64_t>(i), static_cast<int64_t>(j), c)) {
            continue;
        }
        const double cbar = c - u[i] - v[static_cast<std::size_t>(j)];
        if (std::abs(cbar) > worst) worst = std::abs(cbar);
    }
    return worst;
}

// The width a reseed gives a row after `reseeds` earlier reseeds: the first
// width, doubled by each of them and held at the number of columns there are.
inline int64_t ladder_width(int64_t first, int64_t reseeds, int64_t cap) {
    int64_t w = first;
    for (int64_t k = 0; k < reseeds; ++k) w = std::min<int64_t>(w * 2, cap);
    return w;
}

// The checks every entry point runs on the block it is handed: one bipartite
// block backed by `src`, pair arcs carrying at most one unit, and knobs that
// let a round change the master it read.
template <class Source>
void require_block(const Source& src, const FlowProblem& prob,
                   const CandidateSet& cand, const ImplicitOptions& opts,
                   bool expect_expanded, const char* who) {
    const std::string name(who);
    if (src.nrow <= 0 || src.ncol <= 0) {
        LAP_THROW_DIMENSION(name + ": source is " + std::to_string(src.nrow) + " x " +
                            std::to_string(src.ncol) + ", which names no pair to generate");
    }
    if (prob.blocks.size() != 1u) {
        LAP_THROW_DIMENSION(name + ": " + std::to_string(prob.blocks.size()) +
                            " bipartite blocks, and the loop prices one");
    }
    const BipartiteBlock& blk = prob.blocks[0];
    if (blk.costs == nullptr) {
        LAP_THROW_DIMENSION(name + ": the block has no cost source");
    }
    if (blk.costs->nrow() != src.nrow || blk.costs->ncol() != src.ncol) {
        LAP_THROW_DIMENSION(name + ": the block is " + std::to_string(blk.costs->nrow()) +
                            " x " + std::to_string(blk.costs->ncol()) + ", the source is " +
                            std::to_string(src.nrow) + " x " + std::to_string(src.ncol));
    }
    if (blk.lower != 0 || blk.upper != 1) {
        LAP_THROW_DIMENSION(name + ": block arcs are bounded [" + std::to_string(blk.lower) +
                            ", " + std::to_string(blk.upper) +
                            "], and a pair the loop adds carries at most one unit");
    }
    // A problem that has never been expanded holds no arcs, so the candidate
    // set is what its arc set is about to be. One that has holds the last
    // master a path point left behind, and the candidate set is what that
    // master was solved over. Which of the two a caller has is the difference
    // between starting and continuing, and it is not something to infer.
    if (expect_expanded && !prob.expanded) {
        LAP_THROW_DIMENSION(name + ": the problem is not expanded, so there is no "
                            "master to continue from");
    }
    if (!expect_expanded && prob.expanded) {
        LAP_THROW_DIMENSION(name + ": the problem is already expanded, so its arc "
                            "set is not the candidate set's");
    }
    if (cand.nrow() != src.nrow || cand.ncol() != src.ncol) {
        LAP_THROW_DIMENSION(name + ": candidate set is " + std::to_string(cand.nrow()) +
                            " x " + std::to_string(cand.ncol()) + ", source is " +
                            std::to_string(src.nrow) + " x " + std::to_string(src.ncol));
    }
    if (opts.keep_per_row < 1) {
        LAP_THROW_DIMENSION(name + ": keep_per_row " + std::to_string(opts.keep_per_row) +
                            " adds no pair, so a pricing round cannot change the "
                            "master it priced");
    }
    if (opts.width < 0) {
        LAP_THROW_DIMENSION(name + ": width " + std::to_string(opts.width) +
                            " adds no column to a deficient row");
    }
    if (opts.max_rounds < 1) {
        LAP_THROW_DIMENSION(name + ": max_rounds " + std::to_string(opts.max_rounds) +
                            " runs no round");
    }
}

template <class Source>
void require_shape(const Source& src, const FlowProblem& prob,
                   const CandidateSet& cand, const ImplicitOptions& opts,
                   bool expect_expanded) {
    require_block(src, prob, cand, opts, expect_expanded, "solve_implicit_assignment");
    // Hall's condition answers feasibility for a row-perfect matching, so the
    // required flow has to be one unit per row. A design asking for fewer is a
    // different feasibility question and is not this loop's. The units are
    // counted rather than read off one node, because a compiler is free to
    // inject them at the auxiliary source or at the row nodes themselves.
    int64_t injected = 0;
    for (int64_t s : prob.supply) {
        if (s > 0) injected += s;
    }
    if (injected != src.nrow) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: the problem injects " +
                            std::to_string(injected) + " units for " +
                            std::to_string(src.nrow) + " rows, and the loop matches "
                            "every row");
    }
}

// What a policy did with a master that came back short of its flow.
enum class ShortfallAnswer {
    proceed,    // no arc over the source can place more: price what was placed
    reseeded,   // the arc set grew; solve the master again
    decided     // the answer is final and has been written
};

// The rounds themselves, over a problem whose arcs are already the ones the
// candidate set names. Every entry point runs this body; what separates them is
// whether those arcs were placed by this call or left behind by the last one,
// and the policy, which is what separates one design from another:
//
//   on_shortfall(master, cand, reseeds, rec)  a master short of its flow
//   duals(master, rec)                        the u, v it reads off the master
//   price(master, cand, rec)                  the omitted pairs, priced
//   finish(master, cand, priced, rec)         the answer, once nothing prices in
//
// `search` is whatever the source can be asked about a row without reading it,
// and it is a parameter rather than a local because it depends on the columns'
// geometry alone. A path over caliper values moves the cut and leaves the
// geometry where it is, so one structure serves every point.
// Make a master exactly optimal over the arcs it holds. The flow solver works
// in doubles, and on costs whose differences sit below their rounding it can
// stop on a flow that a residual cycle of exactly negative cost still
// improves, so no exact potentials exist for it and a certificate built on it
// can only be read at a tolerance. Each such cycle is cancelled, which keeps
// the flow integral and feasible and lowers its exact cost, until the residual
// graph carries none. The potentials are then the recovered ones, rounded and
// put back on the solver's gauge, and the total is read off the repaired flow.
// A master that needed no cancelling is left as the solver returned it.
inline void exact_master(const FlowProblem& prob, FlowResult& master) {
    std::vector<int64_t> flow = master.flow;
    int64_t n_cancelled = 0;
    exact::ShortestPaths paths;
    for (;;) {
        paths = recover_flow_potentials(prob, flow, master.potential, true);
        if (paths.ok || paths.cycle.empty()) break;
        cancel_residual_cycle(prob, flow, paths.cycle);
        ++n_cancelled;
    }
    if (n_cancelled == 0 || !paths.ok) return;

    master.flow = std::move(flow);
    const std::size_t n = paths.dist.size();
    const double gauge = n > 0 ? exact::approximate(paths.dist[0]).value : 0.0;
    master.potential.assign(n, 0.0);
    for (std::size_t v = 0; v < n; ++v) {
        master.potential[v] = exact::approximate(paths.dist[v]).value - gauge;
    }
    ::lap::detail::CompensatedSum total;
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        total.add(prob.arcs[a].cost * static_cast<double>(master.flow[a]));
    }
    master.total_cost = total.value();
}

template <class Source, class Policy>
void run_rounds(const Source& src, FlowProblem& prob, CandidateSet& cand,
                RowSearch<Source>& search, const ImplicitOptions& opts,
                Policy& policy, ImplicitSearch& out) {
    out.possible_edges = src.nrow * src.ncol;

    FlowOptions flow_opts = opts.flow;
    flow_opts.return_potentials = true;

    // Every exit leaves the problem holding the master it stopped on. That is
    // what makes the loop resumable: a caller with a further question about the
    // same arcs -- the next point of a path -- puts it warm rather than cold.
    const auto carry = [&prob](const FlowResult& master) {
        prob.warm_flow      = master.flow;
        prob.warm_potential = master.potential;
    };

    int64_t reseeds = 0;
    bool decided = false;
    for (int64_t round = 1; round <= opts.max_rounds; ++round) {
        ImplicitRound rec;
        rec.round = round;
        rec.candidate_pairs = cand.n_arcs();
        rec.block_arcs = prob.block_arcs[0].n_arcs;

        const Clock::time_point t_master = Clock::now();
        FlowResult master = solve_min_cost_flow(prob, flow_opts);
        if (opts.certify && master.status == "optimal") exact_master(prob, master);
        rec.master_seconds = seconds_since(t_master);
        rec.master_status  = master.status;
        rec.flow_sent      = master.flow_sent;
        rec.flow_required  = master.flow_required;
        rec.master_cost    = master.total_cost;
        carry(master);

        // A master that ran out of augmentations has not answered the question,
        // so nothing downstream can be read off it.
        if (master.status == "iteration_limit") {
            out.rounds.push_back(rec);
            out.status = master.status;
            decided = true;
            break;
        }

        if (master.flow_sent < master.flow_required) {
            rec.kind = ImplicitRound::Kind::reseeded;
            const ShortfallAnswer answer = policy.on_shortfall(master, cand, reseeds, rec);
            if (answer == ShortfallAnswer::reseeded) {
                out.rounds.push_back(rec);
                ++reseeds;
                continue;
            }
            if (answer == ShortfallAnswer::decided) {
                out.rounds.push_back(rec);
                decided = true;
                break;
            }
            rec.kind = ImplicitRound::Kind::priced;
        }

        policy.duals(master, rec);

        const Clock::time_point t_price = Clock::now();
        const BlockPricing priced = policy.price(master, cand, rec);
        rec.pricing_seconds  += seconds_since(t_price);
        rec.min_reduced_cost = priced.min_reduced_cost;
        rec.n_violators      = priced.n_violators;
        rec.n_evaluated     += priced.n_evaluated;

        if (priced.n_violators == 0) {
            // Nothing the master omits prices below the threshold, so the duals
            // are feasible for every pair of the complete problem, and the
            // policy writes the answer and whatever certifies it.
            policy.finish(master, cand, priced, rec);
            out.status = master.status;
            out.rounds.push_back(rec);
            decided = true;
            break;
        }

        const std::vector<CandidateSet::Pair> added =
            cand.add_pairs(violator_pairs(priced.violators));
        rec.pairs_added += static_cast<int64_t>(added.size());
        rec.arcs_added  += add_block_arcs(prob, 0, added);
        out.rounds.push_back(rec);
    }

    if (!decided) out.status = "iteration_limit";

    out.candidate_edges = cand.n_arcs();
    out.edges_evaluated = cand.edges_evaluated();
}

}  // namespace implicit_detail

// Put every matched pair back on the tight face: u_i := c_i,match(i) - v_match(i).
//
// The flow model gives a block arc the upper bound of one unit that the row's
// own supply already implies, and the assignment LP has no such bound -- its
// dual asks u_i + v_j <= c_ij on every admissible pair, matched pairs included.
// A min-cost flow is optimal with an arc at its upper bound priced strictly
// below zero, so the two agree on the primal and not on the duals, and a warm
// start reaches that state on purpose: the slackness repair pushes a newly
// added arc to its upper bound, and an augmentation that never touches it again
// leaves it there. Solved cold, every matched arc entered the flow on a
// shortest path at a reduced cost of zero and the question does not arise.
//
// A matched arc prices at or below zero, so this only ever lowers u_i, and
// lowering u_i raises every reduced cost in row i: a dual point that was
// feasible stays feasible, the matched pairs become tight, and the dual
// objective meets the primal rather than overshooting it. That is a projection
// onto the duals the LP asks for, not a repair of a wrong answer -- an
// unmatched row and a pair the source forbids are left alone, and what they
// leave behind is what certify_assignment() then refuses.
template <class Source>
void tighten_matched_duals(const Source& src, const std::vector<int>& match,
                           std::vector<double>& u, const std::vector<double>& v) {
    for (std::size_t i = 0; i < match.size(); ++i) {
        const int j = match[i];
        if (j < 0) continue;
        double c = 0.0;
        if (!cost_if_allowed(src, static_cast<int64_t>(i), static_cast<int64_t>(j), c)) {
            continue;
        }
        u[i] = c - v[static_cast<std::size_t>(j)];
    }
}

namespace implicit_detail {

// Exact potentials for the pair arcs, U for the rows and V for the columns, set
// up to price the pairs a master omits at zero: the violators are the pairs
// whose reduced cost c - U_i - V_j is exactly negative.
//
// The pricer runs in doubles, against potentials rounded up: u_hi >= U and
// v_hi >= V, so c - u_hi - v_hi is at most the exact reduced cost. Two
// subtractions in doubles can put a negative value at most 4 eps (|u| + |v|)
// above itself, so a pair whose exact reduced cost is negative prices below a
// margin of twice that, and every pair pricing above the margin is exactly
// non-negative. The pricer therefore takes -margin as its threshold, which
// admits the pairs within rounding of zero, and the exact sign decides each of
// those as the pricer reaches it. A pruning pricer's bound is already a lower
// bound on the exact reduced cost, so a subtree it skips at the margin holds no
// negative pair either.
//
// `offset` serves a network whose pair arcs carry the source's cost less a
// constant, rounded: an arc costs fl(c - offset), and the exact sign is taken of
// fl(c - offset) - U_i - V_j. The pricer still reads c, so its row potentials
// are rounded up from U_i + offset, and the margin grows by the rounding of the
// one subtraction, which near zero is bounded by the same potentials. An offset
// of zero is the unshifted cost and changes nothing above.
struct ExactPrices {
    bool ok = false;
    std::vector<exact::Expansion> U;
    std::vector<exact::Expansion> V;
    std::vector<exact::Approximation> ua;
    std::vector<exact::Approximation> va;
    std::vector<double> u_hi;
    std::vector<double> v_hi;
    double offset = 0.0;
    double margin = 0.0;

    void set(std::vector<exact::Expansion> rows, std::vector<exact::Expansion> cols,
             double cost_offset = 0.0) {
        U = std::move(rows);
        V = std::move(cols);
        offset = cost_offset;
        ua.resize(U.size());
        va.resize(V.size());
        u_hi.resize(U.size());
        v_hi.resize(V.size());
        double largest_u = 0.0;
        double largest_v = 0.0;
        exact::Expansion shifted;
        for (std::size_t i = 0; i < U.size(); ++i) {
            ua[i] = exact::approximate(U[i]);
            exact::Approximation sa = ua[i];
            if (offset != 0.0) {
                exact::grow_expansion(U[i], offset, shifted);
                sa = exact::approximate(shifted);
            }
            u_hi[i] = detail::next_up(sa.value + sa.error);
            largest_u = std::max(largest_u, std::abs(u_hi[i]));
        }
        for (std::size_t j = 0; j < V.size(); ++j) {
            va[j] = exact::approximate(V[j]);
            v_hi[j] = detail::next_up(va[j].value + va[j].error);
            largest_v = std::max(largest_v, std::abs(v_hi[j]));
        }
        margin = detail::next_up(8.0 * DBL_EPSILON *
                                 (largest_u + largest_v + std::abs(offset)));
        ok = true;
    }

    template <class Source>
    BlockPricing price(const Source& src, RowSearch<Source>& search, CandidateSet& cand,
                       int keep_per_row) const {
        const auto negative = [this](int64_t i, int64_t j, double c) {
            const std::size_t si = static_cast<std::size_t>(i);
            const std::size_t sj = static_cast<std::size_t>(j);
            const double arc_cost = c - offset;
            return exact::sign_reduced_cost(arc_cost, U[si], ua[si], V[sj], va[sj]) < 0;
        };
        return search.price(src, u_hi, v_hi, cand, keep_per_row, -margin, negative);
    }
};

// The unit-capacity assignment. A short master is Hall's question, answered by
// a witness or by the columns that can repair it; the duals are the assignment
// LP's, read off the potentials and put on its tight face; and the answer is
// certified against that LP.
template <class Source>
struct AssignmentPolicy {
    const Source& src;
    FlowProblem& prob;
    RowSearch<Source>& search;
    const ImplicitOptions& opts;
    ImplicitResult& out;
    int64_t first_width;
    AssignmentDuals duals_;
    ExactPrices exact_;

    AssignmentPolicy(const Source& s, FlowProblem& p, RowSearch<Source>& rs,
                     const ImplicitOptions& o, ImplicitResult& r)
        : src(s), prob(p), search(rs), opts(o), out(r),
          first_width(o.width > 0 ? o.width : implicit_seed_width(s.ncol)) {
        out.seed_width = first_width;
    }

    ShortfallAnswer on_shortfall(const FlowResult&, CandidateSet& cand, int64_t reseeds,
                                 ImplicitRound& rec) {
        const int width = static_cast<int>(ladder_width(first_width, reseeds, src.ncol));
        const Clock::time_point t_seed = Clock::now();
        FeasibilityRound seeded = feasibility_round(src, cand, width, search);
        rec.pricing_seconds = seconds_since(t_seed);
        rec.n_evaluated = seeded.n_evaluated;

        if (seeded.status == FeasibilityRound::Status::infeasible) {
            out.status = "infeasible";
            out.witness = std::move(seeded.witness);
            out.witness_certified = seeded.certified;
            return ShortfallAnswer::decided;
        }
        if (seeded.status == FeasibilityRound::Status::feasible) {
            // Hall says the restricted arcs admit a row-perfect matching and the
            // master placed fewer units than it has rows. Both read the same arc
            // set, so one of them is wrong about it, and that is not a statement
            // about the candidate set to carry on from.
            LAP_THROW("solve_implicit_assignment: the master placed " +
                      std::to_string(rec.flow_sent) + " of " +
                      std::to_string(rec.flow_required) +
                      " units over an arc set Hall's condition calls feasible");
        }
        rec.pairs_added = static_cast<int64_t>(seeded.added.size());
        rec.arcs_added  = add_block_arcs(prob, 0, seeded.added);
        return ShortfallAnswer::reseeded;
    }

    // The master is feasible, so its potentials are duals to price with, once
    // they are on the face the assignment LP asks for.
    void duals(const FlowResult& master, ImplicitRound& rec) {
        duals_ = map_assignment_duals(prob, layout_of(prob, 0), master.flow,
                                      master.potential, AssignmentEquality::Rows);
        if (!duals_.ok()) {
            LAP_THROW("solve_implicit_assignment: the master placed its required "
                      "flow over unit-capacity arcs and the flow does not read "
                      "back as an assignment");
        }
        rec.matched_slack = matched_slack(src, duals_.match, duals_.u, duals_.v);
        tighten_matched_duals(src, duals_.match, duals_.u, duals_.v);
    }

    // A loop that certifies prices against the master's exact potentials,
    // recovered over the pairs it holds; one that does not, or a master the
    // recovery finds not exactly optimal over its own pairs, prices against its
    // doubles at -tol.
    BlockPricing price(const FlowResult&, CandidateSet& cand, ImplicitRound& rec) {
        exact_.ok = false;
        if (opts.certify) {
            const exact::AssignmentPotentials pots = exact::recover_assignment_potentials(
                CandidateGraph<Source>(src, cand), duals_.match, duals_.v);
            cand.note_evaluated(pots.n_evaluated);
            rec.n_evaluated += pots.n_evaluated;
            if (pots.ok) {
                exact_.set(pots.u, pots.v);
                rec.exact_pricing = true;
                return exact_.price(src, search, cand, opts.keep_per_row);
            }
        }
        return search.price(src, duals_.u, duals_.v, cand, opts.keep_per_row, opts.tol);
    }

    // The certificate is the omitted scan and a scan over the master's own
    // pairs, which costs the candidates rather than the grid. When the last
    // round priced at zero, the same two halves decide it exactly: the pairs
    // the master holds checked against the exact potentials, and the omitted
    // ones proven non-negative by the pricing that ended the loop.
    void finish(const FlowResult& master, CandidateSet& cand, const BlockPricing& priced,
                ImplicitRound& rec) {
        if (opts.certify) {
            const CandidateGraph<Source> held_graph(src, cand);
            const ReducedCostScan held = scan_reduced_costs(
                held_graph, duals_.u, duals_.v, opts.tol);
            cand.note_evaluated(held.n_admissible);
            rec.n_evaluated += held.n_admissible;

            ExactDuals extra;
            exact::ExpansionCheck held_exact;
            if (exact_.ok) {
                held_exact = exact::check_expansion_duals(held_graph, duals_.match,
                                                          exact_.U, exact_.V);
                cand.note_evaluated(held_exact.n_evaluated);
                rec.n_evaluated += held_exact.n_evaluated;
                extra.u = &exact_.U;
                extra.v = &exact_.V;
                extra.check = &held_exact;
                extra.source = ExactDualsSource::recovered;
            }

            out.certificate = certify_assignment(
                src, duals_.match, duals_.u, duals_.v, opts.tol,
                merge_scans(held, omitted_scan(priced)), Arithmetic::Auto, extra);
            out.certified = out.certificate.certified_optimal;
        }
        out.match = duals_.match;
        out.u = duals_.u;
        out.v = duals_.v;
        out.total_cost = master.total_cost;
    }
};

}  // namespace implicit_detail

// Solve the assignment over the complete implicit problem `src` describes, by
// growing `cand` until the pairs it holds carry an optimal solution for all of
// them.
//
// `prob` is a compiled, unexpanded problem holding one unit-capacity bipartite
// block backed by `src`, with one unit of supply per row. It is expanded over
// the candidate set here, and it is left holding the last master solved -- its
// arcs, its flow and its potentials -- so a caller can read the solution back
// through the block's own (i, j) metadata, and a caller with a further question
// about the same arcs can put it warm.
//
// `cand` carries whatever seed the caller has. An empty set is a valid start:
// the first master places nothing, and the feasibility phase seeds every row
// with its `width` cheapest admissible columns. That seed and the pricing both
// go through one RowSearch, so a source carrying geometry answers them from a
// bound over its columns rather than by reading all of them.
template <class Source>
ImplicitResult start_implicit_assignment(const Source& src, FlowProblem& prob,
                                         CandidateSet& cand,
                                         RowSearch<Source>& search,
                                         const ImplicitOptions& opts = ImplicitOptions()) {
    implicit_detail::require_shape(src, prob, cand, opts, /*expect_expanded=*/false);
    expand_block_subset(prob, 0, cand);
    ImplicitResult out;
    implicit_detail::AssignmentPolicy<Source> policy(src, prob, search, opts, out);
    implicit_detail::run_rounds(src, prob, cand, search, opts, policy, out);
    return out;
}

// Put the same question to a problem a previous call already answered: its
// arcs, its flow and its potentials, against a source that has since admitted
// pairs it did not admit before.
//
// This is a path's step. A wider caliper leaves every arc in place, every
// capacity where it was and every cost already reported unchanged, so the
// incumbent flow stays feasible and the only thing the widening can have broken
// is dual feasibility on the pairs it admitted. Those are exactly what a pricing
// round looks at, which is why a path point is the loop again rather than a
// second mechanism.
template <class Source>
ImplicitResult continue_implicit_assignment(const Source& src, FlowProblem& prob,
                                            CandidateSet& cand,
                                            RowSearch<Source>& search,
                                            const ImplicitOptions& opts = ImplicitOptions()) {
    implicit_detail::require_shape(src, prob, cand, opts, /*expect_expanded=*/true);
    ImplicitResult out;
    implicit_detail::AssignmentPolicy<Source> policy(src, prob, search, opts, out);
    implicit_detail::run_rounds(src, prob, cand, search, opts, policy, out);
    return out;
}

// One problem, one answer, and the row structure built here because nothing
// outside the call has a use for it.
template <class Source>
ImplicitResult solve_implicit_assignment(const Source& src,
                                         FlowProblem& prob,
                                         CandidateSet& cand,
                                         const ImplicitOptions& opts = ImplicitOptions()) {
    RowSearch<Source> search(src);
    return start_implicit_assignment(src, prob, cand, search, opts);
}

// ---------------------------------------------------------------------------
// Any design with one block of pair arcs
// ---------------------------------------------------------------------------
//
// A compiled design other than the assignment -- a full matching, a ratio, a
// variable ratio -- is the same three-layer network with other bounds on the
// source and sink arcs, and its pair arcs are still [0, 1] over one cost source.
// Two things change, and neither is the pricing.
//
// The duals are the potentials themselves. An arc from row node r to column node
// c reduces to cost + pi(r) - pi(c), which is the pricer's c - u_i - v_j with
// u_i = -pi(row_i) and v_j = pi(col_j). A pair the master omits sits at its
// lower bound of zero, so the complete problem is optimal exactly when no
// omitted pair prices below zero against the master's potentials: every arc
// the master holds already satisfies its own slackness, and the omitted ones
// are the only arcs left to ask.
//
// Feasibility is a maximum flow rather than a matching of every row. A master
// short of its flow leaves some nodes with excess, and the nodes it can still
// reach in the residual graph form a set R holding no deficit node, since a
// deficit inside R would be an augmenting path. An omitted pair could raise the
// flow only by leaving R: a row inside it and a column outside. So a round asks
// for exactly those pairs, the cheapest `width` per row inside R, or per column
// outside it when that side is the smaller, and when there are none the flow the
// master placed is the maximum over every arc the source admits, not only over
// the ones it holds. The master's potentials leave every residual arc priced at
// or above zero, and the solver settles a shortfall as the cheapest flow of its
// value, so pricing the omitted pairs then decides the cost of that maximum
// flow the way it decides the cost of a complete one.

// The source with its rows and columns exchanged, which is what a question about
// a column's cheapest rows is put to. The primary template wraps any source; a
// LazyCostMatrix is transposed into another one, so the question reaches the
// tree its specialisation builds over the rows.
template <class Source>
struct TransposedSource {
    const Source* src = nullptr;
    int64_t nrow = 0;
    int64_t ncol = 0;

    explicit TransposedSource(const Source& s) : src(&s), nrow(s.ncol), ncol(s.nrow) {}

    double at(int64_t i, int64_t j) const { return src->at(j, i); }
    bool allowed(int64_t i, int64_t j) const { return src->allowed(j, i); }
    bool admissible(int64_t i, int64_t j, double& cost) const {
        return cost_if_allowed(*src, j, i, cost);
    }
};

template <class Source>
struct Transposition {
    using type = TransposedSource<Source>;
    static type of(const Source& s) { return type(s); }
};

template <>
struct Transposition<LazyCostMatrix> {
    using type = LazyCostMatrix;
    static LazyCostMatrix of(const LazyCostMatrix& s) { return s.transposed(); }
};

struct DesignResult : ImplicitSearch {
    // Aligned to the problem's arcs, which `prob` holds after the call, and one
    // potential per node in the solver's gauge. Empty when no master was solved.
    std::vector<int64_t> flow;
    std::vector<double>  potential;
    double  total_cost    = 0.0;
    int64_t flow_sent     = 0;
    int64_t flow_required = 0;

    // The master's own certificate, over the arcs it holds.
    FlowCertificate flow_certificate;

    // The omitted pairs, priced against the same potentials at `price_tol`. The
    // floor bounds every omitted admissible pair, the ones a pruning pricer
    // never evaluated included.
    double omitted_min_reduced_cost = std::numeric_limits<double>::infinity();
    double omitted_proven_floor     = std::numeric_limits<double>::infinity();
    double price_tol = 0.0;

    // Whether the flow placed is the maximum over every pair the source admits:
    // true when it met the required flow, and when a short master's residual
    // cut had no admissible pair crossing it.
    bool max_flow_certified = false;

    // The master's certificate holds and no omitted pair prices below
    // -price_tol, so the flow is optimal for the complete problem.
    bool certified = false;
};

namespace implicit_detail {

// Which nodes a flow can still reach from the excess it left unplaced, over the
// residual graph of the arcs the problem holds.
inline std::vector<char> residual_reach(const FlowProblem& prob,
                                        const std::vector<int64_t>& flow) {
    const int32_t n = prob.n_nodes;
    const std::size_t n_arcs = prob.arcs.size();
    std::vector<int64_t> left(prob.supply.begin(), prob.supply.end());
    std::vector<int32_t> deg(static_cast<std::size_t>(n) + 1, 0);
    for (std::size_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[a];
        left[static_cast<std::size_t>(arc.tail)] -= flow[a];
        left[static_cast<std::size_t>(arc.head)] += flow[a];
        ++deg[static_cast<std::size_t>(arc.tail) + 1];
        ++deg[static_cast<std::size_t>(arc.head) + 1];
    }
    for (int32_t v = 0; v < n; ++v) {
        deg[static_cast<std::size_t>(v) + 1] += deg[static_cast<std::size_t>(v)];
    }
    std::vector<std::size_t> adj(2 * n_arcs);
    std::vector<int32_t> cursor(deg.begin(), deg.end() - 1);
    for (std::size_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[a];
        adj[static_cast<std::size_t>(cursor[static_cast<std::size_t>(arc.tail)]++)] = a;
        adj[static_cast<std::size_t>(cursor[static_cast<std::size_t>(arc.head)]++)] = a;
    }

    std::vector<char> reached(static_cast<std::size_t>(n), 0);
    std::vector<int32_t> queue;
    for (int32_t v = 0; v < n; ++v) {
        if (left[static_cast<std::size_t>(v)] > 0) {
            reached[static_cast<std::size_t>(v)] = 1;
            queue.push_back(v);
        }
    }
    for (std::size_t q = 0; q < queue.size(); ++q) {
        const int32_t v = queue[q];
        for (int32_t k = deg[static_cast<std::size_t>(v)];
             k < deg[static_cast<std::size_t>(v) + 1]; ++k) {
            const std::size_t a = adj[static_cast<std::size_t>(k)];
            const FlowArc& arc = prob.arcs[a];
            int32_t to = -1;
            if (arc.tail == v && flow[a] < arc.upper) to = arc.head;
            else if (arc.head == v && flow[a] > arc.lower) to = arc.tail;
            if (to < 0 || reached[static_cast<std::size_t>(to)]) continue;
            reached[static_cast<std::size_t>(to)] = 1;
            queue.push_back(to);
        }
    }
    return reached;
}

// The flow a problem's lower bounds require and the part of it `flow` places,
// counted from the problem rather than from the start a solve was given. A solve
// reports both relative to its starting flow, which a warm start moves.
inline void placed_value(const FlowProblem& prob, const std::vector<int64_t>& flow,
                         int64_t& sent, int64_t& required) {
    std::vector<int64_t> d0(prob.supply.begin(), prob.supply.end());
    std::vector<int64_t> left(prob.supply.begin(), prob.supply.end());
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        d0[static_cast<std::size_t>(arc.tail)] -= arc.lower;
        d0[static_cast<std::size_t>(arc.head)] += arc.lower;
        left[static_cast<std::size_t>(arc.tail)] -= flow[a];
        left[static_cast<std::size_t>(arc.head)] += flow[a];
    }
    required = 0;
    int64_t unplaced = 0;
    for (std::size_t v = 0; v < d0.size(); ++v) {
        if (d0[v] > 0) required += d0[v];
        if (left[v] > 0) unplaced += left[v];
    }
    sent = required - unplaced;
}

template <class Source>
struct DesignPolicy {
    using TSource = typename Transposition<Source>::type;

    const Source& src;
    FlowProblem& prob;
    RowSearch<Source>& search;
    const ImplicitOptions& opts;
    DesignResult& out;

    // Built the first time a round asks a column for its cheapest rows.
    std::unique_ptr<TSource> t_src;
    std::unique_ptr<RowSearch<TSource>> t_search;

    std::vector<double> u_, v_;
    exact::ShortestPaths recovered_;
    ExactPrices exact_;

    DesignPolicy(const Source& s, FlowProblem& p, RowSearch<Source>& rs,
                 const ImplicitOptions& o, DesignResult& r)
        : src(s), prob(p), search(rs), opts(o), out(r) {
        out.seed_width = o.width > 0 ? o.width : implicit_seed_width(s.ncol);
    }

    int64_t width_for(int64_t dim, int64_t reseeds) const {
        const int64_t first = opts.width > 0 ? opts.width : implicit_seed_width(dim);
        return ladder_width(first, reseeds, dim);
    }

    ShortfallAnswer on_shortfall(const FlowResult& master, CandidateSet& cand,
                                 int64_t reseeds, ImplicitRound& rec) {
        const Clock::time_point t_cut = Clock::now();
        const BipartiteBlock& blk = prob.blocks[0];
        const std::vector<char> reached = residual_reach(prob, master.flow);

        std::vector<int32_t> rows_in;
        for (int64_t i = 0; i < src.nrow; ++i) {
            if (reached[static_cast<std::size_t>(blk.row_base + i)]) {
                rows_in.push_back(static_cast<int32_t>(i));
            }
        }
        std::vector<int32_t> cols_out;
        for (int64_t j = 0; j < src.ncol; ++j) {
            if (!reached[static_cast<std::size_t>(blk.col_base + j)]) {
                cols_out.push_back(static_cast<int32_t>(j));
            }
        }

        std::vector<CandidateSet::Pair> want;
        RowScanWork work;
        if (!rows_in.empty() && !cols_out.empty()) {
            if (rows_in.size() <= cols_out.size()) {
                // Each row inside R, over the columns outside it that the row
                // does not already hold.
                std::vector<char> skip(static_cast<std::size_t>(src.ncol), 0);
                for (int64_t j = 0; j < src.ncol; ++j) {
                    skip[static_cast<std::size_t>(j)] =
                        reached[static_cast<std::size_t>(blk.col_base + j)];
                }
                const int width = static_cast<int>(width_for(src.ncol, reseeds));
                detail::RowTopK keep(static_cast<int64_t>(rows_in.size()), width);
                for (std::size_t t = 0; t < rows_in.size(); ++t) {
                    const int64_t i = rows_in[t];
                    for (const int32_t* p = cand.row_begin(i); p != cand.row_end(i); ++p) {
                        skip[static_cast<std::size_t>(*p)] = 1;
                    }
                    search.cheapest_outside(src, i, skip, keep, static_cast<int64_t>(t), work);
                    for (const int32_t* p = cand.row_begin(i); p != cand.row_end(i); ++p) {
                        skip[static_cast<std::size_t>(*p)] =
                            reached[static_cast<std::size_t>(blk.col_base + *p)];
                    }
                }
                keep.emit([&](int32_t t, int32_t j, double) {
                    want.emplace_back(rows_in[static_cast<std::size_t>(t)], j);
                });
            } else {
                // Each column outside R, over the rows inside it that do not
                // already hold the column.
                if (!t_search) {
                    t_src.reset(new TSource(Transposition<Source>::of(src)));
                    t_search.reset(new RowSearch<TSource>(*t_src));
                }
                std::vector<std::vector<int32_t>> holders(static_cast<std::size_t>(src.ncol));
                for (int64_t i = 0; i < src.nrow; ++i) {
                    for (const int32_t* p = cand.row_begin(i); p != cand.row_end(i); ++p) {
                        holders[static_cast<std::size_t>(*p)].push_back(static_cast<int32_t>(i));
                    }
                }
                std::vector<char> skip(static_cast<std::size_t>(src.nrow), 0);
                for (int64_t i = 0; i < src.nrow; ++i) {
                    skip[static_cast<std::size_t>(i)] =
                        !reached[static_cast<std::size_t>(blk.row_base + i)];
                }
                const int width = static_cast<int>(width_for(src.nrow, reseeds));
                detail::RowTopK keep(static_cast<int64_t>(cols_out.size()), width);
                for (std::size_t t = 0; t < cols_out.size(); ++t) {
                    const int32_t j = cols_out[t];
                    for (int32_t i : holders[static_cast<std::size_t>(j)]) {
                        skip[static_cast<std::size_t>(i)] = 1;
                    }
                    t_search->cheapest_outside(*t_src, j, skip, keep,
                                               static_cast<int64_t>(t), work);
                    for (int32_t i : holders[static_cast<std::size_t>(j)]) {
                        skip[static_cast<std::size_t>(i)] =
                            !reached[static_cast<std::size_t>(blk.row_base + i)];
                    }
                }
                keep.emit([&](int32_t t, int32_t i, double) {
                    want.emplace_back(i, cols_out[static_cast<std::size_t>(t)]);
                });
            }
        }
        cand.note_evaluated(work.n_evaluated);
        rec.n_evaluated = work.n_evaluated;
        rec.pricing_seconds = seconds_since(t_cut);

        if (want.empty()) {
            out.max_flow_certified = true;
            return ShortfallAnswer::proceed;
        }
        const std::vector<CandidateSet::Pair> added = cand.add_pairs(want);
        rec.pairs_added = static_cast<int64_t>(added.size());
        rec.arcs_added  = add_block_arcs(prob, 0, added);
        out.max_flow_certified = false;
        return ShortfallAnswer::reseeded;
    }

    void duals(const FlowResult& master, ImplicitRound&) {
        const BipartiteBlock& blk = prob.blocks[0];
        u_.resize(static_cast<std::size_t>(src.nrow));
        v_.resize(static_cast<std::size_t>(src.ncol));
        for (int64_t i = 0; i < src.nrow; ++i) {
            u_[static_cast<std::size_t>(i)] =
                -master.potential[static_cast<std::size_t>(blk.row_base + i)];
        }
        for (int64_t j = 0; j < src.ncol; ++j) {
            v_[static_cast<std::size_t>(j)] =
                master.potential[static_cast<std::size_t>(blk.col_base + j)];
        }
        if (master.flow_sent == master.flow_required) out.max_flow_certified = true;
    }

    // A reduced cost is a cost and two potentials, so the resolution it has is
    // set by the largest of them, which is the scale certify_flow() reads every
    // arc against.
    double price_tol(const FlowResult& master) const {
        double scale = 1.0;
        for (double p : master.potential) scale = std::max(scale, std::abs(p));
        return opts.tol * scale;
    }

    // The omitted pairs, priced against the master's exact potentials when the
    // loop certifies: the shortest-path distances of its residual graph, which
    // put the row node at -U_i and the column node at V_j in the pricer's
    // terms, the reading duals() gives the solver's own. A flow the recovery
    // finds not exactly optimal over its own arcs is priced against the doubles
    // at the threshold, as a loop that does not certify is every round.
    BlockPricing price(const FlowResult& master, CandidateSet& cand, ImplicitRound& rec) {
        exact_.ok = false;
        recovered_.ok = false;
        if (opts.certify) {
            recovered_ = recover_flow_potentials(prob, master.flow, master.potential);
            if (recovered_.ok) {
                const BipartiteBlock& blk = prob.blocks[0];
                std::vector<exact::Expansion> U(static_cast<std::size_t>(src.nrow));
                std::vector<exact::Expansion> V(static_cast<std::size_t>(src.ncol));
                for (int64_t i = 0; i < src.nrow; ++i) {
                    U[static_cast<std::size_t>(i)] = exact::negated(
                        recovered_.dist[static_cast<std::size_t>(blk.row_base + i)]);
                }
                for (int64_t j = 0; j < src.ncol; ++j) {
                    V[static_cast<std::size_t>(j)] =
                        recovered_.dist[static_cast<std::size_t>(blk.col_base + j)];
                }
                exact_.set(std::move(U), std::move(V));
                rec.exact_pricing = true;
                return exact_.price(src, search, cand, opts.keep_per_row);
            }
        }
        return search.price(src, u_, v_, cand, opts.keep_per_row, price_tol(master));
    }

    // The master is certified over the arcs it holds and the omitted pairs by
    // their floor. When the round priced at zero, it is certified exactly too:
    // the master's flow against the potentials that priced it, and the omitted
    // pairs by the pricing that found none of them below zero.
    void finish(const FlowResult& master, CandidateSet&, const BlockPricing& priced,
                ImplicitRound&) {
        out.flow = master.flow;
        out.potential = master.potential;
        out.total_cost = master.total_cost;
        placed_value(prob, master.flow, out.flow_sent, out.flow_required);
        out.omitted_min_reduced_cost = priced.min_reduced_cost;
        out.omitted_proven_floor = priced.proven_floor;
        out.price_tol = price_tol(master);
        if (opts.certify) {
            out.flow_certificate = certify_flow(
                prob, master.flow, master.potential, opts.tol, Arithmetic::Auto,
                exact_.ok ? &recovered_.dist : nullptr, false,
                ExactDualsSource::recovered);
            const bool exact_all = exact_.ok && out.flow_certificate.exact_certificate;
            out.flow_certificate.conclusion_is_exact = exact_all;
            out.certified = exact_all ||
                            (out.flow_certificate.certified_optimal &&
                             !(priced.proven_floor < -out.price_tol));
        }
    }
};

}  // namespace implicit_detail

// Solve a compiled design with one block of [0, 1] pair arcs backed by `src`,
// over the complete implicit problem, by growing `cand` until the pairs it
// holds carry an optimal flow for all of them. `prob` is unexpanded, and is left
// holding the last master solved.
template <class Source>
DesignResult solve_implicit_design(const Source& src, FlowProblem& prob,
                                   CandidateSet& cand, RowSearch<Source>& search,
                                   const ImplicitOptions& opts = ImplicitOptions()) {
    implicit_detail::require_block(src, prob, cand, opts, /*expect_expanded=*/false,
                                   "solve_implicit_design");
    expand_block_subset(prob, 0, cand);
    DesignResult out;
    implicit_detail::DesignPolicy<Source> policy(src, prob, search, opts, out);
    implicit_detail::run_rounds(src, prob, cand, search, opts, policy, out);
    if (out.flow.empty() && !prob.warm_flow.empty()) {
        out.flow = prob.warm_flow;
        out.potential = prob.warm_potential;
    }
    return out;
}

template <class Source>
DesignResult solve_implicit_design(const Source& src, FlowProblem& prob,
                                   CandidateSet& cand,
                                   const ImplicitOptions& opts = ImplicitOptions()) {
    RowSearch<Source> search(src);
    return solve_implicit_design(src, prob, cand, search, opts);
}

}  // namespace lap
