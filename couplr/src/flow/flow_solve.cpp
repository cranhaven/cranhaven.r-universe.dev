// src/flow/flow_solve.cpp
// Successive shortest paths with Johnson potentials, over a residual graph.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// ---------------------------------------------------------------------------
// Lower bounds are the solver's job
// ---------------------------------------------------------------------------
//
// The problem as posed is a bounded-variable transshipment:
//
//     min  sum_a cost(a) * f(a)
//     s.t. sum_{a out of v} f(a) - sum_{a into v} f(a) = supply(v)
//          lower(a) <= f(a) <= upper(a)
//
// Substituting f(a) = lower(a) + g(a) moves the lower bounds off the variables
// and into the node balances. The capacity constraint becomes
// 0 <= g(a) <= upper(a) - lower(a), and conservation at v becomes
//
//     sum_out g - sum_in g = supply(v) - sum_out lower + sum_in lower =: d(v)
//
// so the mandatory flow every lower bound forces is charged to the two nodes it
// runs between: the tail loses that much of its ability to supply, the head
// gains that much of an obligation to dispose. The objective shifts by the
// constant sum_a cost(a) * lower(a), which is why the total cost is summed from
// f = lower + g at the end rather than accumulated per augmentation.
//
// The residual d(v) is a b-flow: some nodes carry an excess to place and others
// a deficit to fill. Because sum_v supply(v) = 0 and every lower bound
// contributes -lower at its tail and +lower at its head, sum_v d(v) = 0, so the
// two sides balance and the excess side sums to flow_required.
//
// Successive shortest paths moves that b-flow one path at a time: take a node s
// with d(s) > 0, find a shortest residual path from it to any node t with
// d(t) < 0, and push what the two balances and the path allow. A feasible g
// exists exactly when every excess can be delivered, so "a lower bound cannot be
// met" is not a separate condition to test; it is the shortfall that leaves
// flow_sent below flow_required, and the status distinguishes the two by how
// much was placed.
//
// The search starts at one excess node rather than at a super-source over all of
// them. A super-source settles every excess node at distance zero and therefore
// relaxes the whole arc set on every augmentation, which costs one pass over the
// arcs per unit of flow whatever the arc set holds. A single source reaches only
// the alternating tree that one node can grow.
//
// A source that reaches no deficit node is blocked for good, which is what lets
// the loop skip it and still place a maximum flow. Every arc leaving its
// reachable set R(s) is saturated, and a later path from another source that
// entered R(s) would have to leave it again to finish, so nothing that happens
// afterwards changes what s can reach.
//
// ---------------------------------------------------------------------------
// Reduced costs and the search
// ---------------------------------------------------------------------------
//
//     cbar(a) = cost(a) + pi[tail(a)] - pi[head(a)]
//
// Successive shortest paths keeps the invariant cbar(a) >= 0 on every residual
// arc. A search that stops at the first deficit node t restores it by adding
// min(dist[v], dist[t]) to every potential. An arc between two nodes at or past
// dist[t] is left alone; an arc out of a node settled before t is covered by the
// relaxation that gave its head a label; an arc into a node settled before t
// gains dist[t] - dist[v] > 0. The arcs on the augmenting path come out at
// cbar == 0.
//
// That clamp is also why the invariant has to hold before the first search
// rather than from the first update onwards: an arc between two nodes the search
// never reached keeps whatever cbar it had. So a cold start computes potentials
// that give it. Relaxing pi[head] against pi[tail] + cost over the arc set until
// nothing moves is the shortest distance from a virtual root joined to every
// node at zero, which is the invariant written out; with every cost non-negative
// the first pass changes nothing and leaves pi == 0, and no arc set can keep
// moving for more than n_nodes passes unless it prices some cycle below zero.
//
// The search is then plain Dijkstra: with cbar >= 0 a settled node is final, so
// the first deficit node to reach the top of the queue is the nearest one and
// the search stops there rather than draining the queue. The two directions of
// one arc are priced by expressions that are negatives of each other in exact
// arithmetic and not in floating point, so both can round a few ulps below zero
// at once. Where the invariant says the price cannot be negative, rounding is
// the only thing that could have made it so, and it is read as rounding.
//
// ---------------------------------------------------------------------------
// A shortfall is settled over a super source
// ---------------------------------------------------------------------------
//
// Searching from one excess node at a time places a maximum flow, and the
// potentials keep every residual arc non-negative, so the flow is the cheapest
// one for the balances it met. It is not yet the cheapest flow of its value:
// which excess nodes were served was decided by the order they were searched
// from. Two excess nodes competing for one deficit node over arcs costing 10 and
// 1 are served in node order, and the arc costing 10 is the one placed when its
// tail comes first.
//
// Minimum cost among flows of the value placed is the problem with a super
// source joined to every excess node by an arc as wide as its excess and a super
// sink joined from every deficit node, asked for exactly that value. When a
// solve falls short it is re-solved in that form, warm from the flow it
// reached: the super arcs start at what each node sent or received, and the two
// super nodes take the potential that leaves every arc to an unserved node, or
// from an unfilled one, priced at or above zero. The re-solve prices the
// exchanges the search order left out and places them, and it cannot fall short
// itself, since the flow it starts from already has the value it is asked for.
//
// That flow has to be a flow of the problem, short only at the nodes the lower
// bounds left an excess or a deficit at. A cold start has that shape, since its
// balances are the lower bounds' own and a search only moves balance between an
// excess and a deficit node. A warm start does not: the slackness repair
// saturates or empties arcs wherever the potentials say, which leaves balance
// at nodes that conserve flow in the problem, and a solve that cannot place it
// all stops with those nodes still out of balance. So a warm solve that falls
// short is solved again cold, and only the cold answer is settled.
#include "flow_solve.h"

#include "../core/lap_certify.h"
#include "../core/lap_error.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <limits>
#include <queue>
#include <string>
#include <utility>
#include <vector>

namespace lap {

namespace {

// One direction of a residual arc. `rev` is the index of the opposite direction
// in the same array, so pushing flow is two array writes.
struct ResArc {
    int32_t to   = -1;
    int32_t rev  = -1;
    int64_t cap  = 0;
    double  cost = 0.0;
};

// The residual graph in compressed rows: every arc in one array, and the offset
// each node's arcs begin at. A problem arc contributes one forward direction at
// its tail and one reverse at its head, so both degrees are known before
// anything is written and the arcs of a node land contiguously, which is how
// the search reads them.
struct Residual {
    std::vector<int32_t> start;
    std::vector<ResArc>  arcs;

    int32_t begin_of(int32_t v) const {
        return start[static_cast<std::size_t>(v)];
    }
    int32_t end_of(int32_t v) const {
        return start[static_cast<std::size_t>(v) + 1];
    }
    ResArc& at(int32_t e) { return arcs[static_cast<std::size_t>(e)]; }
    const ResArc& at(int32_t e) const {
        return arcs[static_cast<std::size_t>(e)];
    }
};

// int64 addition that reports overflow instead of wrapping, because the node
// balances are sums of caller-supplied capacities and a wrapped balance turns
// an impossible demand into a plausible small one.
bool add_checked(int64_t a, int64_t b, int64_t& out) {
    if (b > 0 && a > std::numeric_limits<int64_t>::max() - b) return false;
    if (b < 0 && a < std::numeric_limits<int64_t>::min() - b) return false;
    out = a + b;
    return true;
}

int64_t add_or_throw(int64_t a, int64_t b, const char* what) {
    int64_t out = 0;
    if (!add_checked(a, b, out)) {
        LAP_THROW_DIMENSION(std::string("FlowProblem: ") + what + " overflows int64");
    }
    return out;
}

// What a starting flow leaves the augmentation loop to place: the node balances
// it does not meet, counted on the excess side. Every augmentation moves at
// least one unit, so this is what a starting point costs.
//
// A null `f` asks the question of the arc lower bounds, which is the cold
// start.
int64_t b_flow_required(const FlowProblem& prob, const std::vector<int64_t>* f) {
    std::vector<int64_t> d(prob.supply.begin(), prob.supply.end());
    const int64_t n_arcs = static_cast<int64_t>(prob.arcs.size());
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t fa = (f == nullptr) ? arc.lower
                                          : (*f)[static_cast<std::size_t>(a)];
        int64_t& dt = d[static_cast<std::size_t>(arc.tail)];
        int64_t& dh = d[static_cast<std::size_t>(arc.head)];
        dt = add_or_throw(dt, -fa, "a node balance");
        dh = add_or_throw(dh, fa, "a node balance");
    }
    int64_t need = 0;
    for (const int64_t dv : d) {
        if (dv > 0) need = add_or_throw(need, dv, "the required flow");
    }
    return need;
}

// "optimal" when the b-flow was met, and every weaker outcome names what stopped
// the solver rather than what the caller hoped for. A shortfall means no
// feasible flow exists at all; the split between "partial" and "infeasible" is
// whether a maximum-cardinality-then-minimum-cost answer was produced or
// nothing moved.
//
// An interrupted solve stopped before it had searched the paths that decide
// between those three, so it is named for what it is rather than folded into
// any of them: "partial" and "infeasible" both say a feasible flow does not
// exist, and the interrupted solve knows nothing of the kind.
std::string derive_status(int64_t flow_sent, int64_t flow_required, bool no_path,
                          bool interrupted) {
    if (interrupted)                return "interrupted";
    if (flow_sent == flow_required) return "optimal";
    if (!no_path)                   return "iteration_limit";
    if (flow_sent == 0)             return "infeasible";
    return "partial";
}

// The cheapest flow of the value `short_flow` placed, over the problem with a
// super source and sink attached; see the header comment. `short_flow` and
// `short_pi` are replaced by the re-solved flow and potentials, and false is
// returned, leaving both untouched, when the re-solve does not place the value
// it was asked for.
bool settle_shortfall(const FlowProblem& prob, const FlowOptions& opts,
                      std::vector<int64_t>& short_flow, std::vector<double>& short_pi,
                      double& total_cost) {
    const int32_t N = prob.n_nodes;
    const int64_t n_arcs = static_cast<int64_t>(prob.arcs.size());

    // Balances left by the lower bounds alone, which is what the super arcs are
    // as wide as, and the balances the reached flow leaves unmet.
    std::vector<int64_t> d0(prob.supply.begin(), prob.supply.end());
    std::vector<int64_t> left(prob.supply.begin(), prob.supply.end());
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        d0[static_cast<std::size_t>(arc.tail)] -= arc.lower;
        d0[static_cast<std::size_t>(arc.head)] += arc.lower;
        const int64_t fa = short_flow[static_cast<std::size_t>(a)];
        left[static_cast<std::size_t>(arc.tail)] -= fa;
        left[static_cast<std::size_t>(arc.head)] += fa;
    }

    const int32_t s_star = N;
    const int32_t t_star = N + 1;

    FlowProblem aug;
    aug.n_nodes = N + 2;
    aug.supply.assign(static_cast<std::size_t>(N) + 2, 0);
    aug.arcs = prob.arcs;
    aug.expanded = true;
    aug.warm_flow = short_flow;

    double pi_s = -std::numeric_limits<double>::infinity();
    double pi_t = std::numeric_limits<double>::infinity();
    int64_t value = 0;
    for (int32_t v = 0; v < N; ++v) {
        const std::size_t sv = static_cast<std::size_t>(v);
        aug.supply[sv] = prob.supply[sv] - d0[sv];
        if (d0[sv] > 0) {
            const int64_t sent = d0[sv] - left[sv];
            value += sent;
            aug.arcs.emplace_back(s_star, v, 0, d0[sv], 0.0);
            aug.warm_flow.push_back(sent);
            if (left[sv] > 0 && short_pi[sv] > pi_s) pi_s = short_pi[sv];
        } else if (d0[sv] < 0) {
            const int64_t received = -d0[sv] + left[sv];
            aug.arcs.emplace_back(v, t_star, 0, -d0[sv], 0.0);
            aug.warm_flow.push_back(received);
            if (left[sv] < 0 && short_pi[sv] < pi_t) pi_t = short_pi[sv];
        }
    }
    aug.supply[static_cast<std::size_t>(s_star)] = value;
    aug.supply[static_cast<std::size_t>(t_star)] = -value;

    aug.warm_potential.assign(short_pi.begin(), short_pi.end());
    aug.warm_potential.push_back(std::isfinite(pi_s) ? pi_s : 0.0);
    aug.warm_potential.push_back(std::isfinite(pi_t) ? pi_t : 0.0);

    FlowOptions aug_opts = opts;
    aug_opts.trace = nullptr;
    aug_opts.max_augmentations = 0;
    aug_opts.return_potentials = true;
    const FlowResult settled = solve_min_cost_flow(aug, aug_opts);
    if (settled.status != "optimal") return false;

    short_flow.assign(settled.flow.begin(), settled.flow.begin() + n_arcs);
    short_pi.assign(settled.potential.begin(), settled.potential.begin() + N);
    detail::CompensatedSum total;
    for (int64_t a = 0; a < n_arcs; ++a) {
        total.add(prob.arcs[static_cast<std::size_t>(a)].cost *
                  static_cast<double>(short_flow[static_cast<std::size_t>(a)]));
    }
    total_cost = total.value();
    return true;
}

}  // namespace

FlowResult solve_min_cost_flow(FlowProblem& prob, const FlowOptions& opts) {
    validate(prob);
    expand_blocks(prob);

    const int64_t n_arcs  = static_cast<int64_t>(prob.arcs.size());
    const int32_t n_nodes = prob.n_nodes;

    // The residual graph holds two directions per arc and names each of them by
    // a 32-bit index, which is what keeps a residual arc down to one cache
    // line's worth. Beyond this the index is what would fail rather than the
    // memory, so the bound is checked rather than trusted.
    if (n_arcs > static_cast<int64_t>(std::numeric_limits<int32_t>::max()) / 2) {
        LAP_THROW_DIMENSION("FlowProblem: " + std::to_string(n_arcs) +
                            " expanded arcs is more than the residual graph "
                            "can index");
    }

    if (!prob.warm_flow.empty() &&
        static_cast<int64_t>(prob.warm_flow.size()) != n_arcs) {
        LAP_THROW_DIMENSION("FlowProblem: warm_flow has " +
                            std::to_string(prob.warm_flow.size()) +
                            " entries for " + std::to_string(n_arcs) +
                            " expanded arcs");
    }

    // Set where the stop was noticed and read where the answer is assembled, so
    // an interrupted solve leaves the same shape of result behind as any other:
    // a flow inside every arc bound, potentials of the right length, and a
    // status that says the b-flow was not finished.
    bool interrupted = false;

    FlowResult out;
    out.flow.assign(static_cast<std::size_t>(n_arcs), 0);
    if (n_nodes <= 0) {
        out.status = derive_status(0, 0, true, false);
        return out;
    }

    const int32_t N = n_nodes;

    // ---- starting point: potentials, then a flow consistent with them ----

    std::vector<double> pi(static_cast<std::size_t>(N), 0.0);

    // Whether cbar >= 0 already holds on every residual arc. A warm start that
    // passed the slackness repair has it, which is what lets the search read a
    // reduced cost a few ulps below zero as the rounding it is. Anything else is
    // given it below, before the first search, because the potential update a
    // search makes cannot repair an arc that search never reached.
    bool residual_nonneg = false;

    bool warm = !prob.warm_potential.empty() || !prob.warm_flow.empty();
    if (!prob.warm_potential.empty()) {
        for (int32_t v = 0; v < n_nodes; ++v) {
            pi[static_cast<std::size_t>(v)] =
                prob.warm_potential[static_cast<std::size_t>(v)];
        }
    }

    std::vector<int64_t> f(static_cast<std::size_t>(n_arcs), 0);
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t v = prob.warm_flow.empty()
                              ? arc.lower
                              : prob.warm_flow[static_cast<std::size_t>(a)];
        if (v < arc.lower || v > arc.upper) {
            LAP_THROW_DIMENSION("FlowProblem: warm_flow on arc " +
                                std::to_string(a) + " is outside its bounds");
        }
        f[static_cast<std::size_t>(a)] = v;
    }

    if (warm) {
        // A warm flow that was optimal for other costs, or optimal potentials
        // paired with any flow at all, need not satisfy complementary
        // slackness, and a residual arc with cbar < 0 is a negative cycle
        // waiting to happen. Restoring slackness costs one pass: an arc the
        // potentials price above zero belongs at its lower bound, one priced
        // below zero belongs at its upper bound, and one priced at zero may sit
        // anywhere. What that breaks is conservation, and repairing
        // conservation is exactly what the augmentation loop below does.
        //
        // An arc with cbar < 0 and no finite upper bound has no bound to be
        // pushed to, so the supplied potentials cannot be made slack-consistent
        // with any flow and the whole warm start is discarded rather than half
        // applied.
        bool repairable = true;
        for (int64_t a = 0; a < n_arcs && repairable; ++a) {
            const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
            const double cbar = arc.cost +
                                pi[static_cast<std::size_t>(arc.tail)] -
                                pi[static_cast<std::size_t>(arc.head)];
            if (cbar < -opts.tol && arc.upper >= FLOW_INF_CAP) repairable = false;
        }
        if (repairable) {
            for (int64_t a = 0; a < n_arcs; ++a) {
                const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
                const double cbar = arc.cost +
                                    pi[static_cast<std::size_t>(arc.tail)] -
                                    pi[static_cast<std::size_t>(arc.head)];
                if (cbar > opts.tol) {
                    f[static_cast<std::size_t>(a)] = arc.lower;
                } else if (cbar < -opts.tol) {
                    f[static_cast<std::size_t>(a)] = arc.upper;
                }
            }
            residual_nonneg = true;
        }

        // Which of the two starting points is the cheaper one, asked of the
        // number that decides it. Successive shortest paths moves the b-flow
        // one path at a time, so what a start costs is the b-flow it leaves,
        // and that is a sum over the arcs either way.
        //
        // The repair is what makes the question worth asking. An arc the
        // potentials price at zero may sit anywhere, and a cost that moves by
        // any amount at all decides every one of those arcs one way or the
        // other; where a design ties many pairs -- rounded distances, a
        // penalty tier shared by a whole category -- a repricing turns a large
        // part of the arc array strictly attractive at once and the repair
        // saturates all of it. The flow that comes out is slack-consistent and
        // a long way from conserving, and every unit of that is an
        // augmentation the cold start would not have paid for.
        //
        // Costing both and taking the smaller is one more pass over the arcs.
        // It leaves the warm start free to win by as much as it can and bounds
        // what it can lose by that one pass.
        if (residual_nonneg &&
            b_flow_required(prob, &f) > b_flow_required(prob, nullptr)) {
            residual_nonneg = false;
        }
        if (!residual_nonneg) {
            // Both halves go back, not just the flow. Relaxation reaches
            // cbar >= 0 from any starting labels, so the potentials could be
            // kept as its starting point; what that costs is the accuracy of
            // the duals it settles on. A potential of the size these designs
            // reach -- the penalty tier puts them in the millions -- carries
            // its own rounding into every reduced cost derived from it, and
            // the certificate reads those reduced costs against a tolerance
            // the shortest-path sums from zero stay inside and the retained
            // ones do not.
            std::fill(pi.begin(), pi.end(), 0.0);
            for (int64_t a = 0; a < n_arcs; ++a) {
                f[static_cast<std::size_t>(a)] =
                    prob.arcs[static_cast<std::size_t>(a)].lower;
            }
        }
    }

    const bool started_warm = warm && residual_nonneg;

    if (!residual_nonneg) {
        // f sits at the lower bound on every arc here, so the residual graph
        // carries forward arcs only, and only where the bounds leave room.
        // Relaxing until nothing moves is a shortest distance from a virtual
        // root joined to every node at zero, which is cbar >= 0 written out.
        bool moved = true;
        int32_t passes = 0;
        while (moved) {
            // One pass reads every arc, so a check here is a fixed cost per
            // O(n_arcs) of work and the relaxation is as responsive as the
            // augmentation loop below.
            if (opts.should_stop && opts.should_stop()) {
                interrupted = true;
                break;
            }
            if (++passes > n_nodes) {
                LAP_THROW("FlowProblem: the residual graph carries a "
                          "negative-cost cycle, so no shortest path exists");
            }
            moved = false;
            for (int64_t a = 0; a < n_arcs; ++a) {
                const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
                if (arc.upper <= arc.lower) continue;
                const double cand =
                    pi[static_cast<std::size_t>(arc.tail)] + arc.cost;
                if (cand + opts.tol < pi[static_cast<std::size_t>(arc.head)]) {
                    pi[static_cast<std::size_t>(arc.head)] = cand;
                    moved = true;
                }
            }
        }
    }

    // ---- node balances the starting flow leaves unmet ----
    //
    // Written against f rather than against lower alone, so the cold case is
    // the warm case with f = lower and there is one formula instead of two.

    std::vector<int64_t> d(static_cast<std::size_t>(n_nodes), 0);
    for (int32_t v = 0; v < n_nodes; ++v) {
        d[static_cast<std::size_t>(v)] = prob.supply[static_cast<std::size_t>(v)];
    }
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t fa = f[static_cast<std::size_t>(a)];
        int64_t& dt = d[static_cast<std::size_t>(arc.tail)];
        int64_t& dh = d[static_cast<std::size_t>(arc.head)];
        dt = add_or_throw(dt, -fa, "a node balance");
        dh = add_or_throw(dh, fa, "a node balance");
    }

    for (int32_t v = 0; v < n_nodes; ++v) {
        const int64_t dv = d[static_cast<std::size_t>(v)];
        if (dv > 0) {
            out.flow_required = add_or_throw(out.flow_required, dv,
                                             "the required flow");
        }
    }

    // ---- residual graph ----

    Residual g;
    g.start.assign(static_cast<std::size_t>(N) + 1, 0);
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        ++g.start[static_cast<std::size_t>(arc.tail) + 1];
        ++g.start[static_cast<std::size_t>(arc.head) + 1];
    }
    for (int32_t v = 0; v < N; ++v) {
        g.start[static_cast<std::size_t>(v) + 1] +=
            g.start[static_cast<std::size_t>(v)];
    }
    g.arcs.assign(static_cast<std::size_t>(2 * n_arcs), ResArc{});

    // The problem's own arcs, in the order the compiler emitted them. A node's
    // arcs are scanned in the order they were written, so this ordering is what
    // decides which of several equally-cheap shortest paths the search finds.
    std::vector<int32_t> cursor(g.start.begin(), g.start.end() - 1);
    std::vector<int32_t> arc_slot(static_cast<std::size_t>(n_arcs), -1);

    // The inverse of arc_slot, for a trace that has to report a residual arc as
    // the problem arc it came from and the direction it was taken in. Built
    // only when a trace was asked for, because it is two more arrays the size
    // of the residual graph.
    std::vector<int64_t> slot_arc;
    std::vector<char>    slot_forward;
    if (opts.trace != nullptr) {
        slot_arc.assign(static_cast<std::size_t>(2 * n_arcs), -1);
        slot_forward.assign(static_cast<std::size_t>(2 * n_arcs), 0);
    }

    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t placed = f[static_cast<std::size_t>(a)] - arc.lower;
        const int32_t iu = cursor[static_cast<std::size_t>(arc.tail)]++;
        const int32_t iv = cursor[static_cast<std::size_t>(arc.head)]++;
        g.at(iu) = ResArc{arc.head, iv,
                          (arc.upper - arc.lower) - placed, arc.cost};
        g.at(iv) = ResArc{arc.tail, iu, placed, -arc.cost};
        arc_slot[static_cast<std::size_t>(a)] = iu;
        if (opts.trace != nullptr) {
            slot_arc[static_cast<std::size_t>(iu)] = a;
            slot_arc[static_cast<std::size_t>(iv)] = a;
            slot_forward[static_cast<std::size_t>(iu)] = 1;
            slot_forward[static_cast<std::size_t>(iv)] = 0;
        }
    }

    if (opts.trace != nullptr) {
        opts.trace->potential_initial.assign(pi.begin(), pi.end());
        opts.trace->steps.clear();
    }

    // The labels and the tree a search ended on, which both kinds of step
    // record identically.
    auto record_search = [&](FlowStep& step, const std::vector<int32_t>& seen,
                             const std::vector<double>& label,
                             const std::vector<int32_t>& reached_by,
                             const std::vector<double>& pot) {
        step.labelled = seen;
        step.dist.reserve(seen.size());
        step.pred_arcs.reserve(seen.size());
        step.pred_forward.reserve(seen.size());
        for (const int32_t v : seen) {
            step.dist.push_back(label[static_cast<std::size_t>(v)]);
            const int32_t ei = reached_by[static_cast<std::size_t>(v)];
            if (ei < 0) {
                step.pred_arcs.push_back(-1);
                step.pred_forward.push_back(0);
            } else {
                step.pred_arcs.push_back(slot_arc[static_cast<std::size_t>(ei)]);
                step.pred_forward.push_back(
                    slot_forward[static_cast<std::size_t>(ei)]);
            }
        }
        step.potential.assign(pot.begin(), pot.end());
    };

    // ---- successive shortest paths ----

    int64_t max_augmentations = opts.max_augmentations;
    if (max_augmentations <= 0) max_augmentations = out.flow_required;

    const double INF = std::numeric_limits<double>::infinity();
    // Both arrays are held at their cleared value between augmentations, so a
    // search starts from a clean one without either having been walked.
    std::vector<double>  dist(static_cast<std::size_t>(N), INF);

    // The arc a node was reached by. Its reverse names the node it was reached
    // from, so the predecessor needs no array of its own.
    std::vector<int32_t> pe(static_cast<std::size_t>(N), -1);

    // The nodes a search gave a finite label to, which are the only ones it
    // wrote and therefore the only ones the next search has to clear. A search
    // from one node reaches the alternating tree that node can grow, which on a
    // restricted master is a small part of the graph: 7 to 646 nodes of 10,002
    // to 22,002 on the four shapes counted by dev_notes/phase3/c1_probe.cpp.
    std::vector<int32_t> touched;

    // A source that reached no deficit node, and that nothing done afterwards
    // can bring back into play.
    std::vector<char> blocked(static_cast<std::size_t>(N), 0);
    using Entry = std::pair<double, int32_t>;

    // cbar >= 0 holds on every residual arc, so a settled node is final and a
    // node is queued at most once per residual arc into it. Past that bound the
    // invariant has been lost, and the search would circle whatever cycle the
    // residual graph prices below zero rather than ever emptying the queue.
    const int64_t max_pops =
        static_cast<int64_t>(g.arcs.size()) + static_cast<int64_t>(N) + 1;

    bool no_path = false;
    int64_t since_check = 0;
    const int64_t check_every = opts.check_every > 0 ? opts.check_every : 1;
    int32_t src = 0;
    while (!interrupted && out.flow_sent < out.flow_required &&
           out.n_augmentations < max_augmentations) {
        if (opts.should_stop && ++since_check >= check_every) {
            since_check = 0;
            if (opts.should_stop()) {
                interrupted = true;
                break;
            }
        }
        // An excess only falls and a deficit only rises, so a node this scan has
        // passed cannot come back into play and the cursor never moves back.
        while (src < N && (d[static_cast<std::size_t>(src)] <= 0 ||
                           blocked[static_cast<std::size_t>(src)])) {
            ++src;
        }
        if (src >= N) {
            no_path = true;
            break;
        }

        for (const int32_t v : touched) {
            dist[static_cast<std::size_t>(v)] = INF;
            pe[static_cast<std::size_t>(v)] = -1;
        }
        touched.clear();

        std::priority_queue<Entry, std::vector<Entry>, std::greater<Entry>> pq;
        dist[static_cast<std::size_t>(src)] = 0.0;
        touched.push_back(src);
        pq.emplace(0.0, src);

        int32_t dst = -1;
        int64_t pops = 0;
        while (!pq.empty()) {
            if (++pops > max_pops) {
                LAP_THROW("FlowProblem: the residual graph carries a "
                          "negative-cost cycle, so no shortest path exists");
            }
            const Entry cur = pq.top();
            pq.pop();
            const double dcur = cur.first;
            const int32_t u = cur.second;
            if (dcur != dist[static_cast<std::size_t>(u)]) continue;
            if (d[static_cast<std::size_t>(u)] < 0) { dst = u; break; }

            const int32_t lo = g.begin_of(u);
            const int32_t hi = g.end_of(u);
            for (int32_t ei = lo; ei < hi; ++ei) {
                const ResArc& e = g.at(ei);
                if (e.cap <= 0) continue;
                double rc = e.cost + pi[static_cast<std::size_t>(u)] -
                            pi[static_cast<std::size_t>(e.to)];
                // The two directions of one arc are priced by expressions that
                // are negatives of each other in exact arithmetic and not in
                // floating point, so both can round a few ulps below zero at
                // once. That pair is a cycle of negative reduced cost, and the
                // search would keep going round it lowering labels. Where the
                // invariant says the price cannot be negative, rounding is the
                // only thing that could have made it so.
                if (rc < 0.0) rc = 0.0;
                const double nd = dcur + rc;
                if (nd + opts.relax_eps < dist[static_cast<std::size_t>(e.to)]) {
                    if (dist[static_cast<std::size_t>(e.to)] == INF) {
                        touched.push_back(e.to);
                    }
                    dist[static_cast<std::size_t>(e.to)] = nd;
                    pe[static_cast<std::size_t>(e.to)] = ei;
                    pq.emplace(nd, e.to);
                }
            }
        }

        if (dst < 0) {
            if (opts.trace != nullptr) {
                FlowStep step;
                step.source = src;
                record_search(step, touched, dist, pe, pi);
                opts.trace->steps.push_back(std::move(step));
            }
            blocked[static_cast<std::size_t>(src)] = 1;
            continue;
        }

        // Nodes settled before the deficit node take their own label, which is
        // final because a settled node's label is; every other node takes the
        // deficit node's, which is what keeps cbar >= 0 on the arcs the search
        // stopped short of.
        //
        // Written as the difference from that common label rather than as the
        // label itself, because adding dist[t] to every node is a constant
        // shift of the whole potential and a potential is only ever read as a
        // difference: cbar(a) subtracts one from another, and the potentials
        // returned are gauged against pi[0]. The shift cancels in both, so the
        // only nodes with anything left to write are the ones the search
        // labelled below dist[t], and they are a subset of the ones it labelled
        // at all.
        const double reach = dist[static_cast<std::size_t>(dst)];
        for (const int32_t v : touched) {
            const double dv = dist[static_cast<std::size_t>(v)];
            if (dv < reach) {
                pi[static_cast<std::size_t>(v)] += dv - reach;
            }
        }

        int64_t aug = d[static_cast<std::size_t>(src)];
        if (-d[static_cast<std::size_t>(dst)] < aug) {
            aug = -d[static_cast<std::size_t>(dst)];
        }
        int32_t steps = 0;
        for (int32_t v = dst; v != src; ) {
            const int32_t ei = pe[static_cast<std::size_t>(v)];
            if (ei < 0) {
                LAP_THROW("FlowProblem: the augmenting path from the deficit "
                          "node has no predecessor at node " + std::to_string(v));
            }
            // A predecessor chain longer than the node count has closed a
            // cycle, which a shortest-path tree cannot contain unless the
            // residual graph prices some cycle below zero.
            if (++steps > N) {
                LAP_THROW("FlowProblem: the residual graph carries a "
                          "negative-cost cycle, so no shortest path exists");
            }
            const ResArc& e = g.at(ei);
            aug = std::min(aug, e.cap);
            v = g.at(e.rev).to;
        }

        for (int32_t v = dst; v != src; ) {
            ResArc& e = g.at(pe[static_cast<std::size_t>(v)]);
            e.cap -= aug;
            g.at(e.rev).cap += aug;
            v = g.at(e.rev).to;
        }

        if (opts.trace != nullptr) {
            FlowStep step;
            step.source = src;
            step.sink   = dst;
            step.reach  = reach;
            step.units  = aug;
            record_search(step, touched, dist, pe, pi);

            // Walked from the sink, so the path comes out reversed and is
            // turned round to read from source to sink.
            for (int32_t v = dst; v != src; ) {
                const int32_t ei = pe[static_cast<std::size_t>(v)];
                step.path_arcs.push_back(slot_arc[static_cast<std::size_t>(ei)]);
                step.path_forward.push_back(
                    slot_forward[static_cast<std::size_t>(ei)]);
                v = g.at(g.at(ei).rev).to;
            }
            std::reverse(step.path_arcs.begin(), step.path_arcs.end());
            std::reverse(step.path_forward.begin(), step.path_forward.end());

            opts.trace->steps.push_back(std::move(step));
        }

        d[static_cast<std::size_t>(src)] -= aug;
        d[static_cast<std::size_t>(dst)] += aug;
        out.flow_sent += aug;
        ++out.n_augmentations;
    }

    // ---- read the answer back ----

    // The reverse direction of an arc carries exactly what has been pushed
    // through the forward one, which is g(a); undoing the substitution gives
    // f(a) = lower(a) + g(a).
    detail::CompensatedSum total;
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const ResArc& fwd = g.at(arc_slot[static_cast<std::size_t>(a)]);
        const int64_t placed = g.at(fwd.rev).cap;
        const int64_t fa = arc.lower + placed;
        out.flow[static_cast<std::size_t>(a)] = fa;
        total.add(arc.cost * static_cast<double>(fa));
    }
    out.total_cost = total.value();

    if (no_path && !interrupted && out.flow_sent < out.flow_required) {
        if (started_warm) {
            FlowProblem cold = prob;
            cold.warm_flow.clear();
            cold.warm_potential.clear();
            return solve_min_cost_flow(cold, opts);
        }
        if (out.flow_sent > 0) settle_shortfall(prob, opts, out.flow, pi, out.total_cost);
    }

    if (opts.return_potentials) {
        out.potential.assign(static_cast<std::size_t>(n_nodes), 0.0);
        const double gauge = pi[0];
        for (int32_t v = 0; v < n_nodes; ++v) {
            out.potential[static_cast<std::size_t>(v)] =
                pi[static_cast<std::size_t>(v)] - gauge;
        }
    }

    out.status = derive_status(out.flow_sent, out.flow_required, no_path,
                               interrupted);
    return out;
}

}  // namespace lap
