// src/flow/flow_lagrangian.h
// The balance network of cardinality_match() under moment multipliers, bounded
// exactly.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The search in R/matching_cardinality_exact.R bounds a node by the Lagrangian
//
//     L(lambda) = min { c(x) + lambda' A x : x a flow of the network }
//
// with one moment row a^r_ij = u^r_i - w^r_j - b^r per one-sided bound. The
// multipliers touch only the pair arcs, and a pair arc then costs
//
//     base_ij + sum_r lambda_r (u^r_i - b^r) - sum_r lambda_r w^r_j,
//
// base_ij being the double the network holds for it. That sum is a real number
// no double generally holds, and the solver is handed its rounding, so neither
// the solver's optimum nor its potentials speak for L(lambda) itself. What does
// is weak duality: for any potentials pi, D(pi) of flow_certify.h, taken over
// the exact costs, is at most L(lambda), which is at most the optimum of the
// moment-constrained problem. It needs no optimality from anything, only exact
// arithmetic, and the costs are held here as expansions to supply it.
//
// The solver stops at reduced costs within its own tolerance of zero and prices
// the rounded costs, so its flow can miss the exact optimum by a margin of that
// order. The step closes the margin before reading anything: it recovers the
// flow's exact potentials from its residual graph, as the certificate does,
// and while the recovery finds a negative cycle instead, it cancels that cycle
// and recovers again. Each cancellation lowers the exact cost strictly and the
// flows are finitely many, so the loop ends at a flow that is exactly optimal
// for the exact costs over the arcs held, and D(pi) at its potentials is its
// exact Lagrangian value. The flow returned is that one, which may differ from
// the solver's.
//
// A generating search holds some of the pair arcs. The pairs it omits sit at
// their lower bound of zero, so each adds min(cbar, 0) to D(pi); pricing them
// against the same potentials and finding none exactly negative makes that sum
// exactly zero, and D over the arcs held is D over every pair. The potentials
// the pricer needs are the pair potentials returned here: the reduced cost of
// pair (i, j) is its arc cost less U_i and V_j.
#pragma once

#include "../core/lap_exact.h"
#include "../core/lap_exact_potentials.h"
#include "flow_certify.h"
#include "flow_problem.h"

#include <cfloat>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace lap {

// The moment rows in the shape the multiplier terms read them: u and w column
// major, one column per row, and one b per row.
struct MomentRows {
    int64_t n_rows = 0;
    int64_t n_left = 0;
    int64_t n_right = 0;
    std::vector<double> u;   // n_left * n_rows
    std::vector<double> w;   // n_right * n_rows
    std::vector<double> b;   // n_rows
};

// The network's pair arcs: which arc each is, its two units, and the nodes
// those units occupy. Units and nodes are 0-based.
struct PairArcs {
    std::vector<int64_t> arc;
    std::vector<int32_t> left;
    std::vector<int32_t> right;
    std::vector<int32_t> left_node;    // per left unit
    std::vector<int32_t> right_node;   // per right unit
};

// sum_r lambda_r (u^r_i - b^r) for every left unit and -sum_r lambda_r w^r_j
// for every right unit, exactly.
inline void multiplier_parts(const MomentRows& rows, const std::vector<double>& lambda,
                             std::vector<exact::Expansion>& left_part,
                             std::vector<exact::Expansion>& right_part) {
    left_part.assign(static_cast<std::size_t>(rows.n_left), exact::Expansion());
    right_part.assign(static_cast<std::size_t>(rows.n_right), exact::Expansion());
    for (int64_t r = 0; r < rows.n_rows; ++r) {
        const double lam = lambda[static_cast<std::size_t>(r)];
        if (lam == 0.0) continue;
        const exact::Expansion lam_b =
            exact::negated(exact::product(lam, rows.b[static_cast<std::size_t>(r)]));
        for (int64_t i = 0; i < rows.n_left; ++i) {
            const double u = rows.u[static_cast<std::size_t>(r * rows.n_left + i)];
            exact::Expansion& acc = left_part[static_cast<std::size_t>(i)];
            acc = exact::expansion_sum(exact::expansion_sum(acc, exact::product(lam, u)),
                                       lam_b);
        }
        for (int64_t j = 0; j < rows.n_right; ++j) {
            const double w = rows.w[static_cast<std::size_t>(r * rows.n_right + j)];
            exact::Expansion& acc = right_part[static_cast<std::size_t>(j)];
            acc = exact::expansion_sum(acc, exact::negated(exact::product(lam, w)));
        }
    }
}

// Every arc's cost exactly: the double the network holds, and on a pair arc
// the two multiplier parts beside it. The recovery and the dual objective read
// most arcs through their rounding alone, so a pair arc's cost is rounded from
// its three parts, and built as an expansion only when one of them asks.
//
// The rounded value is base + L_i + R_j summed in doubles from the parts'
// roundings. Its error is the parts' own error bounds plus the two additions,
// each within half a unit in the last place of a partial sum no larger than
// |base| + |L_i| + |R_j|, which 2 eps times that magnitude covers.
class LagrangianArcCosts {
public:
    LagrangianArcCosts(const FlowProblem& prob, const PairArcs& pairs,
                       const std::vector<exact::Expansion>& left_part,
                       const std::vector<exact::Expansion>& right_part)
        : prob_(prob), pairs_(pairs), left_(left_part), right_(right_part),
          pair_of_arc_(prob.arcs.size(), -1),
          left_approx_(left_part.size()), right_approx_(right_part.size()) {
        for (std::size_t k = 0; k < pairs.arc.size(); ++k) {
            pair_of_arc_[static_cast<std::size_t>(pairs.arc[k])] = static_cast<int64_t>(k);
        }
        for (std::size_t i = 0; i < left_part.size(); ++i) {
            left_approx_[i] = exact::approximate(left_part[i]);
        }
        for (std::size_t j = 0; j < right_part.size(); ++j) {
            right_approx_[j] = exact::approximate(right_part[j]);
        }
    }

    exact::Approximation approximation(std::size_t a) const {
        const double base = prob_.arcs[a].cost;
        const int64_t k = pair_of_arc_[a];
        if (k < 0) return exact::Approximation{base, 0.0};
        const exact::Approximation& l =
            left_approx_[static_cast<std::size_t>(pairs_.left[static_cast<std::size_t>(k)])];
        const exact::Approximation& r =
            right_approx_[static_cast<std::size_t>(pairs_.right[static_cast<std::size_t>(k)])];
        exact::Approximation out;
        out.value = (base + l.value) + r.value;
        out.error = l.error + r.error +
                    2.0 * DBL_EPSILON * (std::fabs(base) + std::fabs(l.value) + std::fabs(r.value));
        return out;
    }

    exact::Expansion exact(std::size_t a) const {
        exact::Expansion c;
        const double base = prob_.arcs[a].cost;
        if (base != 0.0) c.push_back(base);
        const int64_t k = pair_of_arc_[a];
        if (k < 0) return c;
        const std::size_t sk = static_cast<std::size_t>(k);
        return exact::expansion_sum(
            exact::expansion_sum(c, left_[static_cast<std::size_t>(pairs_.left[sk])]),
            right_[static_cast<std::size_t>(pairs_.right[sk])]);
    }

private:
    const FlowProblem& prob_;
    const PairArcs& pairs_;
    const std::vector<exact::Expansion>& left_;
    const std::vector<exact::Expansion>& right_;
    std::vector<int64_t> pair_of_arc_;
    std::vector<exact::Approximation> left_approx_;
    std::vector<exact::Approximation> right_approx_;
};

struct LagrangianStep {
    // The flow, after any negative cycle the exact costs showed was cancelled,
    // and how many were.
    std::vector<int64_t> flow;
    int64_t n_cancelled = 0;
    // The flow is optimal for the exact costs over the arcs held, and `pi` is
    // its recovered potentials; otherwise `pi` is the solver's.
    bool recovered = false;
    std::vector<exact::Expansion> pi;
    // D(pi) over the arcs held, exactly.
    exact::Expansion bound;
    // Per unit, so that a pair's reduced cost is its arc cost less U_i and V_j.
    std::vector<exact::Expansion> pair_u;
    std::vector<exact::Expansion> pair_v;
};

// One solve of the network at `lambda`, read exactly. `prob` holds the base
// costs, before any multiplier; `flow` and `potential` are the solver's answer
// to the rounded costs.
inline LagrangianStep lagrangian_step(const FlowProblem& prob,
                                      const std::vector<int64_t>& flow,
                                      const std::vector<double>& potential,
                                      const PairArcs& pairs, const MomentRows& rows,
                                      const std::vector<double>& lambda) {
    LagrangianStep out;
    std::vector<exact::Expansion> left_part;
    std::vector<exact::Expansion> right_part;
    multiplier_parts(rows, lambda, left_part, right_part);
    const LagrangianArcCosts cost(prob, pairs, left_part, right_part);

    // With every multiplier at zero the costs are the doubles the network
    // holds, and the recovery reads them as such.
    bool priced = false;
    for (double lam : lambda) priced = priced || lam != 0.0;

    out.flow = flow;
    exact::ShortestPaths paths;
    for (;;) {
        paths = priced
            ? recover_flow_potentials(prob, out.flow, potential, cost, true)
            : recover_flow_potentials(prob, out.flow, potential, true);
        if (paths.ok || paths.cycle.empty()) break;
        cancel_residual_cycle(prob, out.flow, paths.cycle);
        ++out.n_cancelled;
    }
    out.recovered = paths.ok;
    if (paths.ok) {
        out.pi = paths.dist;
    } else {
        out.pi.assign(potential.size(), exact::Expansion());
        for (std::size_t v = 0; v < potential.size(); ++v) {
            if (potential[v] != 0.0) out.pi[v].push_back(potential[v]);
        }
    }
    out.bound = exact_dual_objective(prob, out.pi, cost);

    // A pair arc left_node -> right_node reduces to cost + pi(left) - pi(right),
    // which is base - U_i - V_j for U_i = -pi(left) - left_part_i and
    // V_j = pi(right) - right_part_j.
    out.pair_u.resize(left_part.size());
    for (std::size_t i = 0; i < left_part.size(); ++i) {
        out.pair_u[i] = exact::negated(exact::expansion_sum(
            out.pi[static_cast<std::size_t>(pairs.left_node[i])], left_part[i]));
    }
    out.pair_v.resize(right_part.size());
    for (std::size_t j = 0; j < right_part.size(); ++j) {
        out.pair_v[j] = exact::expansion_sum(
            out.pi[static_cast<std::size_t>(pairs.right_node[j])],
            exact::negated(right_part[j]));
    }
    return out;
}

}  // namespace lap
