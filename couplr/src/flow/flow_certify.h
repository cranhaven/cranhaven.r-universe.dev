// src/flow/flow_certify.h
// Optimality certificate for the minimum-cost flow problem.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h, so the checks run
// from the C++ test harness as well as from the Rcpp wrappers.
//
// The LP is the one FlowProblem represents:
//
//     min  sum_a cost(a) f(a)
//     s.t. sum_{a out of v} f(a) - sum_{a into v} f(a) = supply(v)   for every node v
//          lower(a) <= f(a) <= upper(a)                              for every arc a
//
// ---------------------------------------------------------------------------
// The dual, derived rather than quoted
// ---------------------------------------------------------------------------
//
// The sign of the bound terms is the entire content of the objective check, and
// a formula transcribed with one sign wrong certifies nothing while looking
// exactly like a certificate, so it is derived here.
//
// Attach pi(v) to the conservation equality, alpha(a) >= 0 to f(a) >= lower(a)
// and beta(a) >= 0 to f(a) <= upper(a), with the signs that make the Lagrangian
// a lower bound on the primal at every feasible f:
//
//   L = sum_a cost(a) f(a)
//       + sum_v pi(v) [ sum_{a out of v} f(a) - sum_{a into v} f(a) - supply(v) ]
//       + sum_a alpha(a) [ lower(a) - f(a) ]
//       + sum_a beta(a)  [ f(a) - upper(a) ]
//
// Each bracket is zero or negative at a feasible f and each multiplier outside
// the first is nonnegative, so L <= sum_a cost(a) f(a) there. Collecting the
// coefficient of f(a) uses
//
//     sum_v pi(v) * (net flow out of v) = sum_a f(a) [ pi(tail(a)) - pi(head(a)) ]
//
// which is where the reduced cost
//
//     cbar(a) = cost(a) + pi(tail(a)) - pi(head(a))
//
// comes from - the convention the solver uses, and the one in which pi is a
// distance from the source, since cbar(a) >= 0 is the shortest-path inequality
// pi(head) <= pi(tail) + cost. In that convention it is -pi, not pi, that plays
// the usual dual variable of the equality system, which is where the sign of the
// supply term below comes from. The Lagrangian becomes
//
//   L = sum_a f(a) [ cbar(a) - alpha(a) + beta(a) ]
//       - sum_v supply(v) pi(v)
//       + sum_a [ lower(a) alpha(a) - upper(a) beta(a) ]
//
// and its infimum over unrestricted f is finite only when
//
//     alpha(a) - beta(a) = cbar(a)
//
// Given pi, the best (alpha, beta) is available in closed form. Substituting
// alpha = beta + cbar into lower*alpha - upper*beta gives
//
//     lower(a) cbar(a) + beta(a) [ lower(a) - upper(a) ]
//
// whose beta coefficient is nonpositive, so beta is pushed as low as alpha,
// beta >= 0 permits: beta(a) = max(-cbar(a), 0), alpha(a) = max(cbar(a), 0).
// The dual objective is therefore
//
//     D(pi) = - sum_v supply(v) pi(v)
//             + sum_a [ lower(a) * max(cbar(a), 0) + upper(a) * min(cbar(a), 0) ]
//
// The LOWER bound pairs with the POSITIVE part of cbar and the UPPER bound with
// the negative part, and the supply term is NEGATED. The mechanism: an arc with
// cbar(a) > 0 is one the dual would rather leave empty, so the only flow it can
// charge for is the lower bound the arc cannot avoid carrying; an arc with
// cbar(a) < 0 is one the dual would rather run unbounded, and the upper bound is
// what stops it, so it pays upper(a) |cbar(a)|. Pairing them the other way round,
// or dropping the minus on the supply term, returns -primal on the two-node,
// one-arc instance.
//
// ---------------------------------------------------------------------------
// Why the four conditions are one statement
// ---------------------------------------------------------------------------
//
// Weak duality here is exact and arc-local. For any f satisfying conservation,
// sum_v supply(v) pi(v) = sum_a f(a) [ cbar(a) - cost(a) ], and substituting that
// into primal - D(pi) collapses the cost terms:
//
//     primal - D(pi) = sum_a [ f(a) cbar(a)
//                              - lower(a) max(cbar(a), 0)
//                              - upper(a) min(cbar(a), 0) ]
//                    = sum_{cbar(a) > 0} cbar(a) [ f(a) - lower(a) ]
//                      + sum_{cbar(a) < 0} cbar(a) [ f(a) - upper(a) ]
//
// Every term is nonnegative on a primal-feasible f. So the duality gap IS the
// sum of the complementary-slackness violations, each weighted by |cbar|. The
// objective check is not a fourth independent condition bolted on for luck: it
// is what the first three failing looks like in the objective, and it is the
// place a violation too small to trip a per-arc tolerance still accumulates.
//
// One consequence is worth stating plainly. An arc left at a near-infinite upper
// bound whose reduced cost is negative even by rounding contributes
// upper(a) cbar(a) to D(pi), which at FLOW_INF_CAP is a term of order 1e18 |cbar|.
// That is not an artefact to widen a tolerance around. It is the LP answering
// that a negative-reduced-cost arc of unbounded capacity supports no useful
// bound, and the repair is a dual-feasible potential.
//
// ---------------------------------------------------------------------------
// The assignment problem as a special case
// ---------------------------------------------------------------------------
//
// lap_certify.h certifies a different LP over the same data,
//
//     min sum_ij c_ij x_ij   s.t. sum_j x_ij = 1, sum_i x_ij <= 1, x >= 0
//
// whose dual has u free, v <= 0, u_i + v_j = c_ij on matched pairs and v_j = 0 on
// every column no row matched. Reading (u, v) off pi is not a relabelling: the
// two LPs have different constraint sets, and the map has to say which flow
// multiplier is standing in for which assignment dual.
//
// A compiled bipartite design is three arc sets over the node layout of
// flow_problem.h, and the assignment LP appears in it twice, once per side:
//
//   equality on the rows      supply +1 on each row node, supply -F on the sink,
//   (F = n_rows <= n_cols)    block arcs [0,1] costing c_ij, sink arcs [0,1]
//                             costing 0. Every row must place its unit, so the
//                             rows carry sum_j x_ij = 1 and the columns the
//                             inequality.
//
//   equality on the columns   supply +F at the auxiliary source and -F at the
//   (F = n_cols <  n_rows)    sink, source arcs [0,1] costing 0, block arcs
//                             [0,1], sink arcs [0,1]. F units against n_cols
//                             unit-capacity sink arcs saturates every one of
//                             them, so the columns carry the equality and the
//                             rows the inequality.
//
// Write the reduced costs of the two auxiliary arc families,
//
//     a_i = cbar(source -> row_i) = pi(source) - pi(row_i)
//     b_j = cbar(col_j -> sink)   = pi(col_j)  - pi(sink)
//
// both against zero-cost arcs. The bound f <= 1 on col_j -> sink IS the
// assignment's column constraint sum_i x_ij <= 1 when the columns are the free
// side. Its bounded-variable multiplier is beta_j = max(-b_j, 0) from the
// derivation above, and the assignment dual v_j, nonpositive by the same LP
// convention, is its negative:
//
//     v_j = -beta_j = min(b_j, 0)
//
// Taking the multiplier rather than the raw potential difference is what makes
// the second half of complementary slackness hold by construction. A column no
// row matched has f = 0 < 1 on its sink arc, so b_j >= 0 and v_j = 0 exactly.
// Taking v_j = b_j instead fails precisely there and only there: a shortest-path
// solver prices an unused column at its own distance, which sits at or above the
// sink's, so a raw b_j is nonnegative on exactly the columns the assignment dual
// requires to be zero - and a positive v_j is what condition 3's second half
// exists to reject.
//
// The row side carries the equality, so u is free in sign and absorbs the whole
// potential drop across the network, one unit of pi(sink) per unit of flow:
//
//     u_i = pi(sink) - pi(row_i)
//
// That shift is forced. Row node i contributes -supply(i) pi(i) = -pi(row_i) to
// D(pi) and the sink contributes +F pi(sink); F is also the number of rows the
// equality constrains, so spreading pi(sink) across them is the only way
// sum_i u_i can reproduce those two terms together. Without it the two objectives
// differ by F pi(sink) on every instance whose sink potential is not zero.
//
// With the matched block arcs tight - cbar = 0, which a shortest-path solver
// delivers because it augments along tight paths and its potential update leaves
// them tight - all four assignment conditions follow:
//
//     u_i + v_j = min(pi(col_j), pi(sink)) - pi(row_i) <= pi(col_j) - pi(row_i)
//               <= c_ij                                      dual feasibility
//     u_i + v_j = c_ij on a matched pair, since a used column has b_j <= 0
//     v_j = 0 on an unmatched column, since an unused column has b_j >= 0
//     v_j <= 0 everywhere, by construction
//     sum_i u_i + sum_j v_j
//         = F pi(sink) - sum_i pi(row_i) + sum_{j used} [ pi(col_j) - pi(sink) ]
//         = sum_{(i,j) matched} [ pi(col_j) - pi(row_i) ]
//         = sum_{(i,j) matched} c_ij = primal
//
// the last step because F rows are matched and F columns are used, so the two
// F pi(sink) terms cancel, and tightness turns each potential difference into a
// cost.
//
// Orientation. The assignment LP puts the equality on the rows, so it needs
// n_rows <= n_cols, and R/lap_certify.R normalizes a tall problem by transposing
// it. The second compilation above is the same statement on the flow side, and
// under it a_i and b_j simply exchange roles: the free rows' source arcs carry
// the slack the free columns' sink arcs carried, and the anchor moves from the
// sink to the source. Its objective identity runs the same way,
//
//     sum_j u_j + sum_i v_i
//         = sum_j [ pi(col_j) - pi(source) ] + sum_{i used} [ pi(source) - pi(row_i) ]
//         = sum_{(i,j) matched} [ pi(col_j) - pi(row_i) ] = primal
//
// because F = n_cols rows are used and there are n_cols columns, so the two
// pi(source) terms cancel. Both cases are
//
//     anchor  = potential of the terminal the FREE side touches
//     u       = signed potential drop between the anchor and the tight-side node
//     v       = min(that drop on the free side, 0)
//
// which is one body, not two derivations. AssignmentEquality names which side
// carries the equality constraint.
#pragma once

#include "flow_problem.h"
#include "../core/lap_certify.h"
#include "../core/lap_exact.h"
#include "../core/lap_exact_potentials.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace lap {

// Full certificate for a candidate flow and a candidate potential vector. The
// potentials are an input to a check, never an answer: garbage potentials fail,
// so accepting potentials from the solver that produced the flow is sound.
struct FlowCertificate {
    // primal
    bool    primal_feasible = false;
    int64_t n_capacity_violations = 0;      // arcs outside [lower, upper]
    int64_t n_conservation_violations = 0;  // nodes whose net flow misses supply
    double  max_conservation_error = 0.0;   // largest |net out - net in - supply|
    double  primal_objective = std::numeric_limits<double>::quiet_NaN();

    // dual: every arc that can still take flow must not price below its own
    // -tol(a), scaled as dual_tolerance describes
    bool    dual_feasible = false;
    double  min_residual_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t worst_arc = -1;

    // The widest per-arc tolerance any comparison was made against. A reduced
    // cost is a sum of a cost and two potentials, so the resolution available
    // to it is set by the largest of those three rather than by 1. Reported
    // because a verdict reached at a tolerance the caller cannot see is not one
    // the caller can weigh: a design whose potentials reach 1e15 is being
    // certified to a quarter of a unit, and that is worth knowing.
    double  dual_tolerance = 0.0;

    // complementary slackness: every arc carrying more than its lower bound must
    // not price above the same per-arc tolerance
    bool    complementary_slackness = false;
    int64_t n_cs_violations = 0;

    // exact arithmetic
    //
    // The same conditions with no tolerance: the sign of cost(a) + pi(tail) -
    // pi(head) on every residual arc, decided exactly. The flow is integral and
    // its conservation already exact, so these are the whole of it, and the
    // duality gap, being the sum of the slackness violations weighted by
    // |cbar|, is then exactly zero. The potentials they are asked of are the
    // ones supplied when those hold exactly, and otherwise the ones the flow
    // itself determines: shortest-path distances over its residual graph, held
    // as expansions. A flow that is not optimal has none, since its residual
    // graph carries a negative cycle.
    bool    exact_available = false;
    bool    exact_certificate = false;
    int64_t n_exact_violations = 0;   // residual arcs pricing exactly below zero
    ExactDualsSource exact_potentials_source = ExactDualsSource::none;
    std::vector<exact::Expansion> exact_potential;

    // conclusion
    double  dual_objective = std::numeric_limits<double>::quiet_NaN();
    double  duality_gap = std::numeric_limits<double>::quiet_NaN();
    bool    certified_optimal = false;
    bool    conclusion_is_exact = false;
    double  tolerance = 0.0;
};

namespace flow_exact_detail {

// Every residual arc of `flow`, as the arcs of the shortest-path problem whose
// distances are the flow's optimal potentials: the forward arc of a where it can
// still take flow, at weight cost(a), and the reverse arc where it carries more
// than its lower bound, at weight -cost(a). cbar(a) >= 0 on the first and <= 0
// on the second are the shortest-path inequalities of the two.
struct ResidualAdjacency {
    std::vector<int64_t> start;   // n_nodes + 1 offsets
    std::vector<int32_t> head;
    std::vector<double>  weight;  // the arc cost, negated on a reverse arc
    std::vector<int64_t> arc;     // the arc it is residual to
    std::vector<char>    reverse; // 1 on a reverse arc
};

inline ResidualAdjacency residual_adjacency(const FlowProblem& prob,
                                            const std::vector<int64_t>& flow) {
    const std::size_t n = static_cast<std::size_t>(prob.n_nodes);
    ResidualAdjacency adj;
    adj.start.assign(n + 1, 0);
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        if (flow[a] < arc.upper) ++adj.start[static_cast<std::size_t>(arc.tail) + 1];
        if (flow[a] > arc.lower) ++adj.start[static_cast<std::size_t>(arc.head) + 1];
    }
    for (std::size_t v = 0; v < n; ++v) adj.start[v + 1] += adj.start[v];
    const std::size_t m = static_cast<std::size_t>(adj.start[n]);
    adj.head.resize(m);
    adj.weight.resize(m);
    adj.arc.resize(m);
    adj.reverse.resize(m);
    std::vector<int64_t> cursor(adj.start.begin(), adj.start.end() - 1);
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        if (flow[a] < arc.upper) {
            const std::size_t k = static_cast<std::size_t>(cursor[static_cast<std::size_t>(arc.tail)]++);
            adj.head[k] = arc.head;
            adj.weight[k] = arc.cost;
            adj.arc[k] = static_cast<int64_t>(a);
            adj.reverse[k] = 0;
        }
        if (flow[a] > arc.lower) {
            const std::size_t k = static_cast<std::size_t>(cursor[static_cast<std::size_t>(arc.head)]++);
            adj.head[k] = arc.tail;
            adj.weight[k] = -arc.cost;
            adj.arc[k] = static_cast<int64_t>(a);
            adj.reverse[k] = 1;
        }
    }
    return adj;
}

// The number of residual arcs whose reduced cost against the expansions `pi`
// is exactly negative.
inline int64_t count_exact_violations(const FlowProblem& prob,
                                      const std::vector<int64_t>& flow,
                                      const std::vector<exact::Expansion>& pi) {
    std::vector<exact::Expansion> neg(pi.size());
    std::vector<exact::Approximation> pa(pi.size());
    std::vector<exact::Approximation> na(pi.size());
    for (std::size_t v = 0; v < pi.size(); ++v) {
        neg[v] = exact::negated(pi[v]);
        pa[v] = exact::approximate(pi[v]);
        na[v] = exact::approximate(neg[v]);
    }
    int64_t bad = 0;
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        const std::size_t t = static_cast<std::size_t>(arc.tail);
        const std::size_t h = static_cast<std::size_t>(arc.head);
        // cost + pi(tail) - pi(head) is c - U - V with U = -pi(tail), V = pi(head).
        const int s = exact::sign_reduced_cost(arc.cost, neg[t], na[t], pi[h], pa[h]);
        if (flow[a] < arc.upper && s < 0) ++bad;
        if (flow[a] > arc.lower && s > 0) ++bad;
    }
    return bad;
}

}  // namespace flow_exact_detail

// Exact potentials for `flow`: the shortest-path distances of its residual
// graph from a root joined to every node at weight zero, or ok = false when the
// graph carries a negative cycle and the flow is not optimal. `hint` is the
// solver's potentials when there are any, and orders the search only.
//
// With `want_cycle`, a flow that is not optimal comes back with one negative
// cycle of its residual graph in `cycle`, each residual arc tagged 2a for the
// forward arc of a and 2a + 1 for its reverse; cancel_residual_cycle() pushes
// flow around it.
inline exact::ShortestPaths recover_flow_potentials(const FlowProblem& prob,
                                                    const std::vector<int64_t>& flow,
                                                    const std::vector<double>& hint,
                                                    bool want_cycle = false) {
    const flow_exact_detail::ResidualAdjacency adj =
        flow_exact_detail::residual_adjacency(prob, flow);
    return exact::shortest_paths(
        prob.n_nodes, hint, [&](int64_t k, auto&& emit) {
            const std::size_t sk = static_cast<std::size_t>(k);
            for (int64_t e = adj.start[sk]; e < adj.start[sk + 1]; ++e) {
                const std::size_t se = static_cast<std::size_t>(e);
                emit(adj.head[se],
                     exact::DoubleDifference{adj.weight[se], 0.0,
                                             2 * adj.arc[se] + adj.reverse[se]});
            }
        }, want_cycle);
}

// recover_flow_potentials() against costs that replace the doubles in `prob`:
// the costs of a network whose arcs carry terms no double holds, such as moment
// multipliers folded into a distance. The flow is then proved optimal against
// those costs rather than their roundings.
//
// `costs` answers two questions per arc: `approximation(a)`, the cost rounded
// with a bound on its error, which is all the filter reads, and `exact(a)`,
// the cost as an expansion, asked only of the relaxations the filter cannot
// settle. A cost that is a sum of parts can then be rounded from its parts and
// never built for the arcs the doubles decide.
template <class Costs>
exact::ShortestPaths recover_flow_potentials(const FlowProblem& prob,
                                             const std::vector<int64_t>& flow,
                                             const std::vector<double>& hint,
                                             const Costs& costs, bool want_cycle = false) {
    const flow_exact_detail::ResidualAdjacency adj =
        flow_exact_detail::residual_adjacency(prob, flow);
    return exact::shortest_paths(
        prob.n_nodes, hint, [&](int64_t k, auto&& emit) {
            const std::size_t sk = static_cast<std::size_t>(k);
            for (int64_t e = adj.start[sk]; e < adj.start[sk + 1]; ++e) {
                const std::size_t se = static_cast<std::size_t>(e);
                const std::size_t a = static_cast<std::size_t>(adj.arc[se]);
                const auto exact_cost = [&costs, a]() -> exact::Expansion {
                    return costs.exact(a);
                };
                emit(adj.head[se],
                     exact::LazyWeight<decltype(exact_cost)>{
                         costs.approximation(a), exact_cost,
                         2 * adj.arc[se] + adj.reverse[se], adj.reverse[se] != 0});
            }
        }, want_cycle);
}

// Push as much flow around a residual cycle, tagged as recover_flow_potentials()
// tags it, as its tightest arc allows. The flow stays integral, inside every
// bound and conserved, and on a negative cycle its cost falls.
inline void cancel_residual_cycle(const FlowProblem& prob, std::vector<int64_t>& flow,
                                  const std::vector<int64_t>& cycle) {
    int64_t room = std::numeric_limits<int64_t>::max();
    for (int64_t t : cycle) {
        const std::size_t a = static_cast<std::size_t>(t / 2);
        const FlowArc& arc = prob.arcs[a];
        const int64_t r = (t % 2 == 1) ? flow[a] - arc.lower : arc.upper - flow[a];
        room = std::min(room, r);
    }
    for (int64_t t : cycle) {
        const std::size_t a = static_cast<std::size_t>(t / 2);
        flow[a] += (t % 2 == 1) ? -room : room;
    }
}

// D(pi) of the header, exactly, for potentials held as expansions and arc costs
// read through a costs type as recover_flow_potentials() reads them. Weak
// duality makes it a lower bound on the LP's optimum at every pi, so it is a
// bound whether or not the flow those potentials came from is optimal, and at
// optimal potentials it is the optimum.
//
// Most arcs contribute nothing: an arc with lower bound zero whose reduced cost
// is clearly positive, or with upper bound zero whose reduced cost is clearly
// negative. The same filter as the reduced-cost sign decides those in doubles,
// and only the rest are summed exactly.
template <class Costs>
exact::Expansion exact_dual_objective(const FlowProblem& prob,
                                      const std::vector<exact::Expansion>& pi,
                                      const Costs& costs) {
    const std::size_t n = static_cast<std::size_t>(prob.n_nodes);
    std::vector<exact::Approximation> pa(n);
    for (std::size_t v = 0; v < n; ++v) pa[v] = exact::approximate(pi[v]);

    exact::Expansion total;
    for (std::size_t v = 0; v < n; ++v) {
        const int64_t s = prob.supply[v];
        if (s == 0) continue;
        total = exact::expansion_sum(total,
                                     exact::scale_expansion(pi[v], -static_cast<double>(s)));
    }
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        const std::size_t t = static_cast<std::size_t>(arc.tail);
        const std::size_t h = static_cast<std::size_t>(arc.head);
        const exact::Approximation ca = costs.approximation(a);
        const double approx = (ca.value + pa[t].value) - pa[h].value;
        const double bound = 4.0 * DBL_EPSILON *
                                 (std::fabs(ca.value) + std::fabs(pa[t].value) +
                                  std::fabs(pa[h].value)) +
                             ca.error + pa[t].error + pa[h].error;
        if (approx > bound && arc.lower == 0) continue;
        if (approx < -bound && arc.upper == 0) continue;

        const exact::Expansion cbar = exact::expansion_sum(
            exact::expansion_sum(costs.exact(a), pi[t]), exact::negated(pi[h]));
        const int s = exact::sign(cbar);
        const int64_t mult = s > 0 ? arc.lower : (s < 0 ? arc.upper : 0);
        if (mult == 0) continue;
        // A count above 2^53 does not survive the conversion to a double,
        // FLOW_INF_CAP among them, so it is split into the double nearest it and
        // the integer remainder, both exact.
        const double high = static_cast<double>(mult);
        const double low = static_cast<double>(mult - static_cast<int64_t>(high));
        total = exact::expansion_sum(total, exact::scale_expansion(cbar, high));
        if (low != 0.0) {
            total = exact::expansion_sum(total, exact::scale_expansion(cbar, low));
        }
    }
    return total;
}

// Certify `flow` (one entry per explicit arc) against `potential` (one entry per
// node).
//
// Four groups of conditions:
//   1. primal feasibility  - lower(a) <= f(a) <= upper(a) on every arc, and net
//      flow out of every node equal to its supply;
//   2. dual feasibility    - f(a) < upper(a) implies cbar(a) >= -tol(a);
//   3. complementary slackness - f(a) > lower(a) implies cbar(a) <= tol(a), so
//      an arc strictly inside its bounds needs |cbar(a)| <= tol(a);
//   4. objective equality  - primal against D(pi), within a magnitude-scaled
//      tolerance.
//
// `tol` is relative, and every comparison in 2 and 3 is made against
//
//     tol(a) = tol * max(1, |cost(a)|, |pi(tail(a))|, |pi(head(a))|)
//
// for the reason condition 4 already scales its own. cbar(a) is computed as
// cost(a) + pi(tail(a)) - pi(head(a)), so its last bits are worth the largest
// of those three times eps, and a threshold below that asks a correct solution
// to be more exact than the arithmetic that produced it. The lexicographic tier
// weights a balance design compiles to put the potentials in the millions,
// where one ulp is around 1e-9, so an exactly optimal flow failed an absolute
// 1e-9 on rounding alone.
//
// The scale never falls below 1, so a problem whose costs and potentials are
// all of order 1 is checked against tol itself.
//
// Conditions 2 and 3 are the two directions of the residual graph: 2 prices the
// forward residual arc of a, 3 prices its reverse, whose reduced cost is
// -cbar(a). min_residual_reduced_cost is the minimum over both, so the pair holds
// exactly when no residual arc prices below -tol, and worst_arc names the arc
// that attains it.
//
// The problem must be expanded: `flow` is aligned to the explicit arc array, and
// a block that has not been turned into arcs is a claim about a flow that has no
// entries. That, a length mismatch, and an arc endpoint outside the node range
// are all reported as primal_feasible = false rather than read past the end.
//
// `mode` says which reading the conclusion is taken from, as it does for
// certify_assignment(). The exact reading asks `exact_pi` first when the caller
// holds expansions, since those are what the caller is asking about, then the
// doubles in `potential`, and then, with `recover` set, the potentials the flow
// itself determines. `exact_pi_source` is what the certificate reports
// expansions the caller passed as; a caller that recovered them says so.
inline FlowCertificate certify_flow(const FlowProblem& prob,
                                    const std::vector<int64_t>& flow,
                                    const std::vector<double>& potential,
                                    double tol,
                                    Arithmetic mode = Arithmetic::Double,
                                    const std::vector<exact::Expansion>* exact_pi = nullptr,
                                    bool recover = false,
                                    ExactDualsSource exact_pi_source =
                                        ExactDualsSource::supplied) {
    FlowCertificate rep;
    rep.tolerance = tol;

    const int64_t n_nodes = static_cast<int64_t>(prob.n_nodes);
    const int64_t n_arcs  = static_cast<int64_t>(prob.arcs.size());

    if (!prob.expanded) return rep;
    if (n_nodes <= 0) return rep;
    if (static_cast<int64_t>(prob.supply.size()) != n_nodes) return rep;
    if (static_cast<int64_t>(potential.size()) != n_nodes) return rep;
    if (static_cast<int64_t>(flow.size()) != n_arcs) return rep;
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        if (arc.tail < 0 || static_cast<int64_t>(arc.tail) >= n_nodes) return rep;
        if (arc.head < 0 || static_cast<int64_t>(arc.head) >= n_nodes) return rep;
    }

    // ---- primal feasibility ----
    //
    // The node balance accumulates in double rather than int64_t. A flow handed
    // to a checker is not trusted to respect any bound, so its sum is not
    // trusted either, and a double sum that runs away is a large reported error
    // where an int64_t sum that runs away is undefined behaviour. Every flow
    // value that could belong to a feasible solution is below 2^53 and is
    // therefore summed exactly, which is what lets the conservation test be
    // equality against zero rather than a tolerance: capacities are integral, so
    // conservation either holds or does not.
    std::vector<detail::CompensatedSum> balance(static_cast<std::size_t>(n_nodes));
    for (int64_t v = 0; v < n_nodes; ++v) {
        balance[static_cast<std::size_t>(v)]
            .add(-static_cast<double>(prob.supply[static_cast<std::size_t>(v)]));
    }
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t f = flow[static_cast<std::size_t>(a)];
        if (f < arc.lower || f > arc.upper) ++rep.n_capacity_violations;
        balance[static_cast<std::size_t>(arc.tail)].add(static_cast<double>(f));
        balance[static_cast<std::size_t>(arc.head)].add(-static_cast<double>(f));
    }
    for (int64_t v = 0; v < n_nodes; ++v) {
        const double err = balance[static_cast<std::size_t>(v)].value();
        if (err != 0.0) ++rep.n_conservation_violations;
        const double mag = std::abs(err);
        if (!(mag <= rep.max_conservation_error)) rep.max_conservation_error = mag;
    }
    rep.primal_feasible = (rep.n_capacity_violations == 0) &&
                          (rep.n_conservation_violations == 0);

    // The primal objective is only meaningful once the flow is a flow. Summing
    // costs over a vector that violates conservation produces a number that
    // invites comparison with the dual bound while corresponding to no feasible
    // solution.
    if (rep.primal_feasible) {
        detail::CompensatedSum primal;
        for (int64_t a = 0; a < n_arcs; ++a) {
            const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
            primal.add(arc.cost * static_cast<double>(flow[static_cast<std::size_t>(a)]));
        }
        rep.primal_objective = primal.value();
    }

    // ---- dual feasibility, complementary slackness, and D(pi) ----
    detail::CompensatedSum dual;
    for (int64_t v = 0; v < n_nodes; ++v) {
        dual.add(-static_cast<double>(prob.supply[static_cast<std::size_t>(v)]) *
                 potential[static_cast<std::size_t>(v)]);
    }

    int64_t n_dual_violations = 0;
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t f = flow[static_cast<std::size_t>(a)];
        const double pi_tail = potential[static_cast<std::size_t>(arc.tail)];
        const double pi_head = potential[static_cast<std::size_t>(arc.head)];
        const double cbar = arc.cost + pi_tail - pi_head;

        // The resolution the three terms leave this difference. A non-finite
        // term carries through to a non-finite tolerance, against which every
        // comparison below fails, which is the answer a non-finite potential
        // deserves.
        const double tol_a = tol * std::max(std::max(1.0, std::abs(arc.cost)),
                                            std::max(std::abs(pi_tail),
                                                     std::abs(pi_head)));

        if (cbar > 0.0) {
            dual.add(static_cast<double>(arc.lower) * cbar);
        } else if (cbar < 0.0) {
            dual.add(static_cast<double>(arc.upper) * cbar);
        }

        // Forward residual: the arc can still take flow.
        if (f < arc.upper) {
            if (cbar < rep.min_residual_reduced_cost) {
                rep.min_residual_reduced_cost = cbar;
                rep.worst_arc = a;
            }
            if (tol_a > rep.dual_tolerance) rep.dual_tolerance = tol_a;
            // Negated so a non-finite potential fails rather than slipping
            // through a comparison that is false on NaN either way.
            if (!(cbar >= -tol_a)) ++n_dual_violations;
        }
        // Reverse residual: the arc carries more than it must, and the reverse
        // arc of the residual graph prices at -cbar.
        if (f > arc.lower) {
            if (-cbar < rep.min_residual_reduced_cost) {
                rep.min_residual_reduced_cost = -cbar;
                rep.worst_arc = a;
            }
            if (tol_a > rep.dual_tolerance) rep.dual_tolerance = tol_a;
            if (!(cbar <= tol_a)) ++rep.n_cs_violations;
        }
    }
    rep.dual_objective = dual.value();
    rep.dual_feasible = (n_dual_violations == 0);
    rep.complementary_slackness = (rep.n_cs_violations == 0);

    // ---- conclusion ----
    rep.duality_gap = rep.primal_objective - rep.dual_objective;

    // The gap tolerance scales with the magnitude of the objective. Both sums
    // carry a relative rounding error of order eps per term, so an absolute tol
    // on a sum of many terms sits below the representable resolution of the sum
    // itself and no correct solution could ever meet it. Compensated summation
    // buys back the accumulation error, not the fact that the objective's own
    // last bits are worth |objective| * eps.
    const double tol_gap = tol * std::max(1.0, std::abs(rep.primal_objective));
    const bool numerical = rep.primal_feasible &&
                           rep.dual_feasible &&
                           rep.complementary_slackness &&
                           (std::abs(rep.duality_gap) <= tol_gap);

    // ---- exact reading ----
    //
    // The same order and the same rule as certify_assignment(): expansions the
    // caller holds, then the doubles given, and, when the doubles pass the
    // numerical reading, the potentials the flow itself determines. Those
    // prove the flow and not the potentials given, so n_exact_violations keeps
    // counting against the ones given.
    if (mode != Arithmetic::Double && rep.primal_feasible) {
        const auto try_expansions = [&](const std::vector<exact::Expansion>& pi,
                                        ExactDualsSource source, bool keep,
                                        bool counts) {
            if (static_cast<int64_t>(pi.size()) != n_nodes) return;
            rep.exact_available = true;
            const int64_t bad = flow_exact_detail::count_exact_violations(prob, flow, pi);
            if (counts) rep.n_exact_violations = bad;
            if (bad == 0) {
                rep.exact_certificate = true;
                rep.exact_potentials_source = source;
                if (keep) rep.exact_potential = pi;
            }
        };
        if (exact_pi != nullptr) {
            try_expansions(*exact_pi, exact_pi_source, true, true);
        }
        if (!rep.exact_certificate) {
            std::vector<exact::Expansion> as_given(static_cast<std::size_t>(n_nodes));
            bool finite = true;
            for (int64_t v = 0; v < n_nodes && finite; ++v) {
                const double x = potential[static_cast<std::size_t>(v)];
                finite = std::isfinite(x);
                if (finite && x != 0.0) as_given[static_cast<std::size_t>(v)].push_back(x);
            }
            if (finite) try_expansions(as_given, ExactDualsSource::solver, true,
                                       exact_pi == nullptr);
        }
        if (!rep.exact_certificate && recover && numerical) {
            const exact::ShortestPaths paths = recover_flow_potentials(prob, flow, potential);
            if (paths.ok) try_expansions(paths.dist, ExactDualsSource::recovered, true, false);
        }
    }

    switch (mode) {
        case Arithmetic::Exact:
            rep.certified_optimal = rep.exact_certificate;
            rep.conclusion_is_exact = true;
            break;
        case Arithmetic::Double:
            rep.certified_optimal = numerical;
            rep.conclusion_is_exact = false;
            break;
        case Arithmetic::Auto:
        default:
            rep.certified_optimal = rep.exact_certificate || numerical;
            rep.conclusion_is_exact = rep.exact_certificate;
            break;
    }

    return rep;
}

// Which side of a compiled bipartite assignment carries the equality constraint
// sum x = 1, and therefore which side is free to go unmatched under sum x <= 1.
// The equality belongs to the short side, because the flow value is
// min(n_rows, n_cols) and that side is matched in full.
enum class AssignmentEquality {
    // Every row node carries supply 1 and must place it: the flow the rows
    // inject is the equality. Duals come out in the compiled orientation, and a
    // tall instance reaches this case by being compiled from a transposed cost
    // source.
    Rows,
    // The auxiliary source injects n_cols units through unit-capacity row arcs,
    // saturating every column's sink arc: the columns are matched in full and
    // the rows carry the slack. Duals come out indexed by column, which is the
    // transposed orientation.
    Columns
};

// Where a compiled unit-capacity bipartite assignment put its nodes and its
// block. The auxiliary arcs source -> row_i and col_j -> sink are assumed to
// carry zero cost and bounds [0, 1], which is what makes their reduced costs
// readable off the potentials alone.
struct AssignmentLayout {
    int32_t source_node = FLOW_SOURCE;
    int32_t sink_node   = FLOW_SINK;
    int32_t row_base    = FLOW_FIRST_ROW;
    int32_t col_base    = FLOW_FIRST_ROW;
    int64_t n_rows      = 0;
    int64_t n_cols      = 0;
    int64_t block       = 0;   // index into FlowProblem::block_arcs
};

// The layout one of a problem's blocks sits in. The auxiliary nodes are fixed
// by flow_problem.h, and the block already records where its rows and columns
// start and, through its cost source, how many of each it spans -- so a caller
// that assembles the fields by hand is re-deriving what the problem carries.
//
// An out-of-range block, or one with no cost source, gives a layout with no
// rows, which map_assignment_duals() rejects.
inline AssignmentLayout layout_of(const FlowProblem& prob, int64_t block = 0) {
    AssignmentLayout layout;
    layout.block = block;
    if (block < 0 || block >= static_cast<int64_t>(prob.blocks.size())) return layout;

    const BipartiteBlock& blk = prob.blocks[static_cast<std::size_t>(block)];
    if (blk.costs == nullptr) return layout;

    layout.row_base = blk.row_base;
    layout.col_base = blk.col_base;
    layout.n_rows   = blk.costs->nrow();
    layout.n_cols   = blk.costs->ncol();
    return layout;
}

// How far map_assignment_duals() got. The two failures differ in what they
// leave behind, so a caller that only asks "did it work" and carries on cannot
// tell an empty result from a populated one.
enum class AssignmentMapStatus {
    // `match`, `u` and `v` are populated and the block flow is a unit
    // assignment.
    Ok,
    // The problem, the layout, the flow and the potentials do not describe each
    // other: unexpanded problem, wrong vector length, a block or node index out
    // of range, or a block arc naming a row or column outside the layout.
    // Nothing was read, so `match`, `u` and `v` are empty.
    StructuralMismatch,
    // Everything lined up and the block flow was read, but it is not a unit
    // assignment: an arc carries more than one unit, or a row or column is
    // claimed twice. `match`, `u` and `v` are populated, `match` holding the
    // pairs that were unambiguous and -1 elsewhere.
    NotAnAssignment
};

// A solved flow read back as an assignment plus a pair of assignment duals, in
// the orientation lap::certify_assignment() expects: `match` and `u` are indexed
// by the side carrying the equality, `v` by the side carrying the inequality.
// The caller supplies the cost source in the same orientation, transposed when
// the equality is on the columns.
struct AssignmentDuals {
    AssignmentMapStatus status = AssignmentMapStatus::StructuralMismatch;
    std::vector<int>    match;    // per equality-side unit, -1 unmatched
    std::vector<double> u;        // per equality-side unit
    std::vector<double> v;        // per inequality-side unit

    // The three vectors are safe to index exactly when this is true.
    bool ok() const { return status == AssignmentMapStatus::Ok; }
};

// Map a solved unit-capacity bipartite flow back to (match, u, v).
//
// A flow that is not an assignment - a block arc carrying more than one unit,
// or a row or column claimed twice - is refused, because the map's arithmetic
// then describes something the assignment LP has no primal for. So is a
// structural mismatch, on the same terms as certify_flow(). The two are
// different values of `status` rather than one `ok == false`, because only the
// second leaves the vectors empty.
//
// The duals are exact when the matched block arcs are tight, which is the state
// a shortest-path solver terminates in. When they are not, the map still returns
// them and certify_assignment() rejects them; that is the division of labour
// this file exists for.
inline AssignmentDuals map_assignment_duals(const FlowProblem& prob,
                                            const AssignmentLayout& layout,
                                            const std::vector<int64_t>& flow,
                                            const std::vector<double>& potential,
                                            AssignmentEquality equality) {
    AssignmentDuals out;

    const int64_t n_nodes = static_cast<int64_t>(prob.n_nodes);
    const int64_t n_arcs  = static_cast<int64_t>(prob.arcs.size());
    const int64_t nr = layout.n_rows;
    const int64_t nc = layout.n_cols;

    if (!prob.expanded) return out;
    if (nr <= 0 || nc <= 0) return out;
    if (static_cast<int64_t>(potential.size()) != n_nodes) return out;
    if (static_cast<int64_t>(flow.size()) != n_arcs) return out;
    if (layout.block < 0 ||
        layout.block >= static_cast<int64_t>(prob.block_arcs.size())) return out;
    if (layout.source_node < 0 || static_cast<int64_t>(layout.source_node) >= n_nodes) return out;
    if (layout.sink_node < 0 || static_cast<int64_t>(layout.sink_node) >= n_nodes) return out;
    if (layout.row_base < 0 || static_cast<int64_t>(layout.row_base) + nr > n_nodes) return out;
    if (layout.col_base < 0 || static_cast<int64_t>(layout.col_base) + nc > n_nodes) return out;

    const BlockArcRange& blk = prob.block_arcs[static_cast<std::size_t>(layout.block)];
    if (blk.first_arc < 0 || blk.n_arcs < 0) return out;
    if (blk.first_arc + blk.n_arcs > n_arcs) return out;
    if (static_cast<int64_t>(blk.rc.size()) != blk.n_arcs) return out;

    std::vector<int> row_to_col(static_cast<std::size_t>(nr), -1);
    std::vector<int> col_to_row(static_cast<std::size_t>(nc), -1);

    bool is_assignment = true;
    for (int64_t k = 0; k < blk.n_arcs; ++k) {
        const int64_t f = flow[static_cast<std::size_t>(blk.first_arc + k)];
        if (f == 0) continue;
        if (f < 0 || f > 1) { is_assignment = false; continue; }
        const int64_t i = static_cast<int64_t>(blk.rc[static_cast<std::size_t>(k)].first);
        const int64_t j = static_cast<int64_t>(blk.rc[static_cast<std::size_t>(k)].second);
        if (i < 0 || i >= nr || j < 0 || j >= nc) return out;
        if (row_to_col[static_cast<std::size_t>(i)] >= 0 ||
            col_to_row[static_cast<std::size_t>(j)] >= 0) {
            is_assignment = false;
            continue;
        }
        row_to_col[static_cast<std::size_t>(i)] = static_cast<int>(j);
        col_to_row[static_cast<std::size_t>(j)] = static_cast<int>(i);
    }

    // The anchor is the potential of the terminal the free side touches: the
    // sink when the columns are free, the source when the rows are. Both duals
    // are potential drops measured against it, oriented so that they increase
    // with cost in the direction the flow runs.
    const double anchor = (equality == AssignmentEquality::Rows)
        ? potential[static_cast<std::size_t>(layout.sink_node)]
        : potential[static_cast<std::size_t>(layout.source_node)];

    std::vector<double> row_drop(static_cast<std::size_t>(nr));
    for (int64_t i = 0; i < nr; ++i) {
        row_drop[static_cast<std::size_t>(i)] =
            anchor - potential[static_cast<std::size_t>(layout.row_base + i)];
    }
    std::vector<double> col_drop(static_cast<std::size_t>(nc));
    for (int64_t j = 0; j < nc; ++j) {
        col_drop[static_cast<std::size_t>(j)] =
            potential[static_cast<std::size_t>(layout.col_base + j)] - anchor;
    }

    if (equality == AssignmentEquality::Rows) {
        out.match = row_to_col;
        out.u = row_drop;
        out.v = col_drop;
        for (double& vj : out.v) vj = std::min(vj, 0.0);
    } else {
        out.match = col_to_row;
        out.u = col_drop;
        out.v = row_drop;
        for (double& vi : out.v) vi = std::min(vi, 0.0);
    }
    out.status = is_assignment ? AssignmentMapStatus::Ok
                               : AssignmentMapStatus::NotAnAssignment;
    return out;
}

}  // namespace lap
