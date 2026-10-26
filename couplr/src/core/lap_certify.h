// src/core/lap_certify.h
// Optimality certificate for the rectangular linear assignment problem.
// Pure C++ types and templates - NO Rcpp dependencies (same rule as
// lap_types.h), so the checks run from the C++ test harness as well as from
// the Rcpp wrappers.
//
// Everything here is a minimization. The LP checked is
//
//     min  sum_ij c_ij x_ij
//     s.t. sum_j x_ij  = 1   for every row i
//          sum_i x_ij <= 1   for every column j
//          x >= 0
//
// whose dual is
//
//     max  sum_i u_i + sum_j v_j
//     s.t. u_i + v_j <= c_ij   for every admissible pair (i, j)
//          v_j <= 0            when ncol > nrow
//
// The sign condition is exactly as conditional as that line says. For any
// feasible assignment M', cost(M') >= sum_i u_i + sum_{j used by M'} v_j, and
// the dual objective sums v over ALL columns, so the bound holds only if the
// columns M' leaves out contribute nothing positive. When ncol > nrow every
// column is one M' could leave out, which forces v_j <= 0 throughout. When
// ncol == nrow every feasible assignment uses every column, the two sums are
// the same sum, and v is unrestricted in sign -- the column constraint is an
// equality there, not an inequality. Jonker-Volgenant returns duals of exactly
// that shape (see the rectangular warm-start gate in jv_core.cpp:55-71), so
// imposing v_j <= 0 on a square problem rejects correct duals.
//
// A maximization instance is certified by negating the costs and both dual
// vectors and running exactly these conditions; that sign change belongs to
// the caller, which is why no template here takes a `maximize` flag.
//
// The templates take any cost source exposing `at(int64_t, int64_t) const`,
// `allowed(int64_t, int64_t) const`, and the public data members `nrow` and
// `ncol`: lap::CostMatrix, lap::LazyCostMatrix, lap::PaddedCostView<Base>.
// Forbidden cells read as lap::BIG (1e100), not as Inf or NA, so every scan
// gates on allowed(i, j) before touching at(i, j) -- a single unguarded BIG
// poisons a minimum or a sum beyond recovery.
#pragma once

#include "lap_types.h"
#include "lap_cost_source.h"
#include "lap_exact.h"
#include "lap_exact_potentials.h"
#include "lap_neighbours.h"

#include <vector>
#include <limits>
#include <cmath>
#include <cstdint>
#include <algorithm>
#include <cstddef>

namespace lap {

namespace detail {

// Neumaier compensated summation: `comp` accumulates the low-order bits lost
// at each addition, and is folded back in once at the end. The certificate's
// whole purpose is that a discrepancy of a fraction of a percent between the
// primal and dual objectives is visible, and naive summation over 10^5 terms
// loses digits at exactly that scale. Neumaier's variant, unlike plain Kahan,
// is also correct when the running sum is smaller in magnitude than the
// incoming term, which happens whenever costs straddle zero.
inline void compensated_add(double& sum, double& comp, double x) {
    const double t = sum + x;
    if (std::abs(sum) >= std::abs(x)) {
        comp += (sum - t) + x;
    } else {
        comp += (x - t) + sum;
    }
    sum = t;
}

struct CompensatedSum {
    double  sum = 0.0;
    double  comp = 0.0;
    // What the sum's own rounding is charged against. Neumaier's error is
    // bounded by (2u + O(n^2 u^2)) times the sum of the magnitudes, so both
    // the magnitudes and the term count have to be carried to state it.
    double  abs_sum = 0.0;
    int64_t n_terms = 0;

    void add(double x) {
        compensated_add(sum, comp, x);
        abs_sum += std::fabs(x);
        ++n_terms;
    }
    double value() const { return sum + comp; }

    // An upper bound on |value() - exact|, rounded away from zero. The
    // O(n^2 u^2) term is charged as gamma_n^2, which dominates it and is
    // negligible beside 2u at any term count a solve reaches.
    double envelope() const {
        const double u = 0.5 * std::numeric_limits<double>::epsilon();
        const double g = gamma_of(n_terms);
        const double rel = 2.0 * u + g * g;
        if (!(abs_sum > 0.0)) return 0.0;
        return next_up(rel * abs_sum);
    }
};

}  // namespace detail

// Result of a full scan of the reduced costs cbar_ij = c_ij - u_i - v_j over
// admissible pairs. Certification asks whether the minimum is >= -tol; a
// pricing loop asks for the argmin and the violation count. One scan, both
// questions.
struct ReducedCostScan {
    double  min_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t arg_i = -1;             // 0-based argmin, -1 when no admissible pair exists
    int64_t arg_j = -1;
    int64_t n_violations = 0;       // admissible pairs with cbar < -tol
    int64_t n_admissible = 0;       // admissible pairs scanned

    // A lower bound on the reduced cost of every admissible pair the scan
    // covers, the ones it never evaluated included. A scan that visits every
    // pair proves min_reduced_cost itself; one that prunes proves only the
    // bounds it pruned against. The suboptimality bound rests on this, since
    // min_reduced_cost says nothing about a pair nobody looked at.
    double  proven_floor = std::numeric_limits<double>::infinity();

    // Set by a scan asked for the exact sign of every pair it visited, which
    // is what an exact certificate needs and what a pricing loop does not pay
    // for. A scan that was not asked leaves `exact_checked` false, and a
    // certificate built on it reports its conclusion in double arithmetic.
    bool    exact_checked = false;
    int64_t n_exact_violations = 0; // admissible pairs with c - u - v < 0 exactly
};

enum class ExactDualsSource { none, solver, supplied, recovered };

// Full certificate for a candidate matching and a candidate pair of dual
// vectors. The duals are an input to a check, never an answer: garbage duals
// fail, so accepting duals from the solver that produced the matching is
// sound.
struct CertificateReport {
    // primal
    //
    // `structurally_valid_matching` reads the match vector as a matching:
    // every entry in range, no column claimed twice, no forbidden pair
    // matched. It permits unmatched rows, which is what separates it from
    // primal feasibility. The primal constrains every row of the short side to
    // hold exactly one pair, so a matching that leaves a row free is a valid
    // partial matching and not a feasible solution, and neither conclusion may
    // rest on it. The objective is summed under the weaker of the two, since
    // the cost of a partial matching is a meaningful number while the cost of
    // a vector claiming one column twice is not.
    bool    structurally_valid_matching = false;
    bool    all_rows_matched = false;
    bool    primal_feasible = false;
    int64_t n_rows = 0;
    int64_t n_cols = 0;
    int64_t n_matched = 0;
    int64_t n_duplicate_cols = 0;    // columns claimed by more than one row
    int64_t n_forbidden_matched = 0; // matched pairs where allowed(i,j) is false
    int64_t n_out_of_range = 0;      // match entries outside [-1, ncol-1]
    double  primal_objective = std::numeric_limits<double>::quiet_NaN();

    // dual
    bool    dual_feasible = false;
    double  min_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t worst_i = -1;
    int64_t worst_j = -1;
    double  max_v = std::numeric_limits<double>::quiet_NaN();
    double  dual_objective = std::numeric_limits<double>::quiet_NaN();

    // complementary slackness
    bool    cs_matched_tight = false;
    double  max_matched_slack = std::numeric_limits<double>::quiet_NaN();
    bool    cs_unmatched_free = false;
    double  max_v_unmatched = std::numeric_limits<double>::quiet_NaN();
    bool    complementary_slackness = false;

    // exact arithmetic
    //
    // The same conditions decided with no tolerance at all. Where the
    // numerical conclusion closes on the objective gap, the exact one closes
    // on the row count already required for primal feasibility: given dual
    // feasibility, tight matched arcs and free unmatched columns, the two
    // objectives are equal exactly when every row is matched, so no tolerance
    // decides the exact conclusion.
    bool    exact_dual_feasible = false;
    bool    exact_cs_matched_tight = false;
    bool    exact_cs_unmatched_free = false;
    bool    exact_certificate = false;
    bool    exact_available = false;  // every condition was asked exactly
    int64_t n_exact_violations = 0;   // admissible pairs with c - u - v < 0
    int64_t n_exact_untight = 0;      // matched pairs with c - u - v != 0

    // Which potentials the exact conditions were decided on. The solver's
    // doubles are asked first; when they miss exactness and the matching is
    // optimal, exact potentials recovered from the matching itself decide it
    // instead, and a caller can also hand in expansions of its own. Whichever
    // decided an exact certificate is returned with it, so that the conclusion
    // can be checked again by anyone holding the cost matrix.
    ExactDualsSource exact_duals_source = ExactDualsSource::none;
    std::vector<exact::Expansion> exact_u;
    std::vector<exact::Expansion> exact_v;

    // conclusion
    double  duality_gap = std::numeric_limits<double>::quiet_NaN();
    bool    certified_optimal = false;
    bool    conclusion_is_exact = false;  // which arithmetic the conclusion is in
    double  tolerance = 0.0;

    // The lower bound the scan proves for the reduced cost of every admissible
    // pair, the ones it never evaluated included. Equal to min_reduced_cost
    // when the scan visited every pair; below it when a pricer pruned, since a
    // skipped subtree is known only by the bound it was skipped against.
    double  certified_reduced_cost_floor = -std::numeric_limits<double>::infinity();

    // The most any feasible solution of the complete problem can beat this one
    // by, in the cost unit.
    //
    // Write eps = max(0, -certified_reduced_cost_floor) and, where the sign
    // condition applies, s = max(0, max_v). Then u_i - eps and v_j - s satisfy
    // both dual conditions exactly: every reduced cost gains eps + s and was at
    // least -eps, and every column dual is at most zero. Their dual objective
    // is the reported one less n_rows * eps + n_cols * s, and weak duality puts
    // the optimum above it, so
    //
    //     primal - optimum <= duality_gap + n_rows * eps + n_cols * s.
    //
    // Zero exactly when the duals are feasible with no slack and the two
    // objectives agree, which is the case certified_optimal reports. A pruning
    // pricer that proves only its threshold leaves n_rows * tol here, and that
    // is the price of not having visited every pair.
    double  max_suboptimality = std::numeric_limits<double>::quiet_NaN();
};

// Which arithmetic the conclusion is taken in.
//
// Auto reports the exact conclusion when the exact conditions hold and the
// numerical one otherwise, which is the strongest statement the instance
// supports: the exact conditions imply the numerical ones at any non-negative
// tolerance, so Auto never certifies anything Double would refuse.
//
// Exact refuses to fall back, and is the mode to ask for when the point is the
// strength of the proof rather than the answer.
enum class Arithmetic { Auto, Exact, Double };

// What the exact reading may use beyond the doubles in `u` and `v`: potentials
// recovered from the matching when the doubles miss exactness, and expansions
// the caller already holds. Neither changes the numerical reading.
//
// `check` is for a caller that has already asked the exact conditions of `u`
// and `v` over every admissible pair, in passes it could afford: an
// edge-generation loop checks the pairs its master holds and prices the ones it
// omits, and the two together cover the complete problem without a sweep of
// the grid. `source` is what the certificate reports the expansions as; a
// caller that recovered them itself says so.
struct ExactDuals {
    bool recover = false;
    const std::vector<exact::Expansion>* u = nullptr;
    const std::vector<exact::Expansion>* v = nullptr;
    const exact::ExpansionCheck* check = nullptr;
    ExactDualsSource source = ExactDualsSource::supplied;
};

// min over admissible (i, j) of c_ij - u_i - v_j, with its argmin and the
// number of pairs violating dual feasibility by more than tol.
//
// With `exact` set the scan also counts the pairs whose reduced cost is
// negative in exact arithmetic, which is the count an exact certificate reads
// and is a different question from the count against tol: a pair at -1e-17
// violates exactly and does not violate at 1e-9, and a pair the double
// evaluation puts at 0 may be negative. The reported minimum stays the double
// evaluation, which is what a reader wants to see and what a pricing loop
// compares against; the exact count is what the conclusion rests on.
//
// Returns the empty scan (infinite minimum, argmin -1) on an empty problem or
// on a dual vector whose length does not match the source, rather than
// reading out of bounds.
template <class Source>
ReducedCostScan scan_reduced_costs(const Source& src,
                                   const std::vector<double>& u,
                                   const std::vector<double>& v,
                                   double tol,
                                   bool exact = false) {
    ReducedCostScan out;
    out.exact_checked = exact;

    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    if (nrow <= 0 || ncol <= 0) return out;
    if (static_cast<int64_t>(u.size()) != nrow) return out;
    if (static_cast<int64_t>(v.size()) != ncol) return out;

    // Forbidden arcs carry BIG, not Inf: reading one unguarded drags the
    // minimum to +1e100 and the argmin with it. for_each_admissible() is that
    // guard and the cost read as one question, so a source that computes its
    // costs answers it once, and a source naming where its pairs are is walked
    // over them rather than over its grid.
    for (int64_t i = 0; i < nrow; ++i) {
        const double ui = u[static_cast<std::size_t>(i)];
        for_each_admissible(src, i, [&](int64_t j, double c) {
            const double cbar = c - ui - v[static_cast<std::size_t>(j)];
            ++out.n_admissible;
            if (cbar < out.min_reduced_cost) {
                out.min_reduced_cost = cbar;
                out.arg_i = i;
                out.arg_j = j;
            }
            if (cbar < -tol) ++out.n_violations;
            if (exact &&
                exact::sign_reduced_cost(c, ui, v[static_cast<std::size_t>(j)]) < 0) {
                ++out.n_exact_violations;
            }
            return true;
        });
    }

    // Every admissible pair was evaluated, so the observed minimum is the floor.
    out.proven_floor = out.min_reduced_cost;
    return out;
}

// Two scans over disjoint sets of pairs, read as one scan over their union.
//
// This is what lets a scan be assembled from the passes that can afford it
// rather than from one sweep of the grid: an edge-generation loop knows the
// minimum over the pairs its master holds and the minimum over the pairs it
// omits, and their union is every admissible pair of the complete problem.
//
// Ties keep the earlier argmin in (i, j) order, which is the one a single
// ascending scan of the same pairs would have reported.
inline ReducedCostScan merge_scans(const ReducedCostScan& a, const ReducedCostScan& b) {
    ReducedCostScan out;
    out.n_admissible = a.n_admissible + b.n_admissible;
    out.n_violations = a.n_violations + b.n_violations;
    // The union was checked exactly only if both halves were. A half that was
    // not carries no exact count, and adding its zero to the other half's
    // would report the union as exactly feasible on the strength of pairs
    // nobody asked the question of.
    out.exact_checked = a.exact_checked && b.exact_checked;
    out.n_exact_violations = a.n_exact_violations + b.n_exact_violations;
    // A bound over the union is a bound over each half, so the union can claim
    // only the weaker of the two.
    out.proven_floor = std::min(a.proven_floor, b.proven_floor);

    const bool b_wins = b.min_reduced_cost < a.min_reduced_cost ||
                        (b.min_reduced_cost == a.min_reduced_cost && a.arg_i < 0) ||
                        (b.min_reduced_cost == a.min_reduced_cost && b.arg_i >= 0 &&
                         (b.arg_i < a.arg_i || (b.arg_i == a.arg_i && b.arg_j < a.arg_j)));
    const ReducedCostScan& best = b_wins ? b : a;
    out.min_reduced_cost = best.min_reduced_cost;
    out.arg_i = best.arg_i;
    out.arg_j = best.arg_j;
    return out;
}

// Certify `match` (0-based column per row, -1 unmatched, length src.nrow)
// against the duals `u` (length src.nrow) and `v` (length src.ncol).
//
// Four groups of conditions:
//   1. primal feasibility  - every column claimed at most once, every matched
//      pair admissible, every entry in range;
//   2. dual feasibility    - min_ij cbar_ij >= -tol over admissible pairs, and
//      max_j v_j <= tol;
//   3. complementary slackness, both halves - matched arcs tight, and
//      |v_j| <= tol on every column no row matched;
//   4. objective equality  - |primal - dual| within a magnitude-scaled
//      tolerance.
//
// The same four are also decided with no tolerance at all, and the report
// carries both readings. Groups 2 and 3 come down to the sign of
// c_ij - u_i - v_j, which lap_exact.h evaluates exactly; group 4 is not
// measured exactly but derived, since tight matched arcs, free unmatched
// columns and a matched row count equal to nrow make the two objectives the
// same sum. `mode` decides which reading the conclusion is taken from.
//
// Condition 3's second half is not redundant. A matching can be perfect,
// dual-feasible everywhere, tight on every matched arc, and carry a dual
// bound equal to the true optimum while its primal cost sits above the
// optimum, when a column is left unmatched holding v_j < 0. Checking only
// dual feasibility and matched-arc tightness certifies that wrong answer.
//
// Returns a report with primal_feasible = false on an empty problem or on any
// length mismatch, rather than reading out of bounds.
namespace detail {

// `supplied` is the reduced-cost scan when the caller already has one and null
// when the certificate has to take it itself. Everything else about the check
// is the same either way, so it is one body: a second copy of the conclusion
// would be free to drift from this one, and the conclusion is the whole point.
template <class Source>
CertificateReport certify_assignment_impl(const Source& src,
                                          const std::vector<int>& match,
                                          const std::vector<double>& u,
                                          const std::vector<double>& v,
                                          double tol,
                                          const ReducedCostScan* supplied,
                                          Arithmetic mode,
                                          const ExactDuals& extra) {
    CertificateReport rep;
    rep.tolerance = tol;
    const bool want_exact = (mode != Arithmetic::Double);

    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    rep.n_rows = nrow;
    rep.n_cols = ncol;

    if (nrow <= 0 || ncol <= 0) return rep;
    if (static_cast<int64_t>(match.size()) != nrow) return rep;
    if (static_cast<int64_t>(u.size()) != nrow) return rep;
    if (static_cast<int64_t>(v.size()) != ncol) return rep;

    // ---- primal feasibility ----
    std::vector<int64_t> col_claims(static_cast<std::size_t>(ncol), 0);
    for (int64_t i = 0; i < nrow; ++i) {
        const int64_t j = static_cast<int64_t>(match[static_cast<std::size_t>(i)]);
        if (j < -1 || j >= ncol) {
            ++rep.n_out_of_range;
            continue;
        }
        if (j < 0) continue;  // row deliberately unmatched
        ++rep.n_matched;
        ++col_claims[static_cast<std::size_t>(j)];
        if (!src.allowed(i, j)) ++rep.n_forbidden_matched;
    }
    for (int64_t j = 0; j < ncol; ++j) {
        if (col_claims[static_cast<std::size_t>(j)] > 1) ++rep.n_duplicate_cols;
    }
    rep.structurally_valid_matching = (rep.n_out_of_range == 0) &&
                                      (rep.n_duplicate_cols == 0) &&
                                      (rep.n_forbidden_matched == 0);
    rep.all_rows_matched = (rep.n_matched == nrow);
    rep.primal_feasible = rep.structurally_valid_matching && rep.all_rows_matched;

    // The primal objective is only meaningful once the matching is a matching:
    // summing over a duplicated column or a forbidden arc produces a number
    // that invites comparison with the dual bound but does not correspond to
    // any feasible solution. An unmatched row costs nothing and leaves the sum
    // meaningful, so the objective is reported for a partial matching even
    // though no conclusion may be drawn from it.
    // Infinity until the sum runs, so a bound assembled without one reports
    // no bound rather than a smaller number than it can justify.
    double env_primal = std::numeric_limits<double>::infinity();
    if (rep.structurally_valid_matching) {
        detail::CompensatedSum primal;
        for (int64_t i = 0; i < nrow; ++i) {
            const int64_t j = static_cast<int64_t>(match[static_cast<std::size_t>(i)]);
            if (j < 0) continue;
            primal.add(src.at(i, j));
        }
        rep.primal_objective = primal.value();
        env_primal = primal.envelope();
    }

    // ---- dual feasibility ----
    const ReducedCostScan scan = (supplied != nullptr)
        ? *supplied
        : scan_reduced_costs(src, u, v, tol, want_exact);
    rep.min_reduced_cost = scan.min_reduced_cost;
    rep.certified_reduced_cost_floor = scan.proven_floor;
    rep.worst_i = scan.arg_i;
    rep.worst_j = scan.arg_j;

    double max_v = -std::numeric_limits<double>::infinity();
    for (int64_t j = 0; j < ncol; ++j) {
        max_v = std::max(max_v, v[static_cast<std::size_t>(j)]);
    }
    rep.max_v = max_v;
    // The sign condition applies only where a column can go unmatched; see the
    // derivation at the top of this file.
    const bool sign_condition_applies = (ncol > nrow);
    rep.dual_feasible = (rep.min_reduced_cost >= -tol) &&
                        (!sign_condition_applies || rep.max_v <= tol);

    // The same condition with the band around zero removed. The sign condition
    // is one double against zero and is already exact; what needed the
    // expansion is the reduced cost of every admissible pair, which the scan
    // has counted.
    rep.n_exact_violations = scan.n_exact_violations;
    rep.exact_dual_feasible = scan.exact_checked &&
                              (scan.n_exact_violations == 0) &&
                              (!sign_condition_applies || rep.max_v <= 0.0);

    // Summed over ALL rows and ALL columns, unmatched columns included. That
    // is what turns a nonzero v_j on a freed column into a visible duality
    // gap instead of a term quietly dropped from the bound.
    detail::CompensatedSum dual;
    for (int64_t i = 0; i < nrow; ++i) dual.add(u[static_cast<std::size_t>(i)]);
    for (int64_t j = 0; j < ncol; ++j) dual.add(v[static_cast<std::size_t>(j)]);
    rep.dual_objective = dual.value();
    const double env_dual = dual.envelope();

    // ---- complementary slackness ----
    double max_matched_slack = 0.0;
    for (int64_t i = 0; i < nrow; ++i) {
        const int64_t j = static_cast<int64_t>(match[static_cast<std::size_t>(i)]);
        if (j < 0 || j >= ncol) continue;
        // A forbidden matched arc is already counted in n_forbidden_matched
        // and has made the primal infeasible; its at() is BIG, so folding it
        // into the slack would report 1e100 instead of the real worst arc.
        double c = 0.0;
        if (!cost_if_allowed(src, i, j, c)) continue;
        const double ui = u[static_cast<std::size_t>(i)];
        const double vj = v[static_cast<std::size_t>(j)];
        const double slack = std::abs(c - ui - vj);
        if (slack > max_matched_slack) max_matched_slack = slack;
        if (want_exact && exact::sign_reduced_cost(c, ui, vj) != 0) {
            ++rep.n_exact_untight;
        }
    }
    rep.max_matched_slack = max_matched_slack;
    rep.cs_matched_tight = (max_matched_slack <= tol);
    rep.exact_cs_matched_tight = want_exact && (rep.n_exact_untight == 0);

    double max_v_unmatched = 0.0;
    for (int64_t j = 0; j < ncol; ++j) {
        if (col_claims[static_cast<std::size_t>(j)] != 0) continue;
        const double a = std::abs(v[static_cast<std::size_t>(j)]);
        if (a > max_v_unmatched) max_v_unmatched = a;
    }
    rep.max_v_unmatched = max_v_unmatched;
    rep.cs_unmatched_free = (max_v_unmatched <= tol);
    rep.exact_cs_unmatched_free = (max_v_unmatched == 0.0);

    rep.complementary_slackness = rep.cs_matched_tight && rep.cs_unmatched_free;

    // ---- conclusion ----
    rep.duality_gap = rep.primal_objective - rep.dual_objective;

    // The shifted duals of the derivation on max_suboptimality. Clamped at zero
    // because a feasible primal cannot sit below the optimum, so a negative
    // total is rounding in the two objective sums and not a solution better
    // than optimal.
    //
    // The bound answers "how much can a feasible solution beat this one by",
    // which is a question about a feasible candidate. A partial matching has a
    // real objective and is not feasible, so the arithmetic below still
    // produces a number for it, and that number reads as a guarantee the
    // candidate cannot carry: a zero-cost problem with an uncovered row and
    // zero duals reported max_suboptimality = 0 beside primal_feasible = false.
    // No bound is reported unless the primal is feasible.
    if (!rep.primal_feasible) {
        rep.max_suboptimality = std::numeric_limits<double>::quiet_NaN();
    } else {
        // The gap is a difference of two compensated sums, and compensated
        // summation buys back the accumulation error rather than removing it.
        // Each sum's own envelope is charged here, so the number reported is an
        // upper bound in double arithmetic and not an estimate of one. Every
        // step of the assembly is rounded up, for the same reason.
        const double eps = std::max(0.0, -rep.certified_reduced_cost_floor);
        const double s = sign_condition_applies ? std::max(0.0, rep.max_v) : 0.0;
        // Adding nothing costs nothing: rounding up on a zero term would
        // turn an exactly zero bound into a denormal and report slack
        // where the arithmetic proved none.
        const auto add_up = [](double acc, double term) {
            return term > 0.0 ? next_up(acc + term) : acc;
        };
        double bound = rep.duality_gap;
        bound = add_up(bound, static_cast<double>(nrow) * eps);
        bound = add_up(bound, static_cast<double>(ncol) * s);
        bound = add_up(bound, env_primal);
        bound = add_up(bound, env_dual);
        rep.max_suboptimality =
            std::isnan(bound) ? bound : (bound > 0.0 ? bound : 0.0);
    }

    // The gap tolerance scales with the magnitude of the objective. Both sums
    // carry a relative rounding error of order eps per term, so an absolute
    // tol on a sum of 5*10^4 terms is below the representable resolution of
    // the sum itself and no correct solution could ever meet it. Compensated
    // summation buys back the accumulation error, not the fact that the
    // objective's own last bits are worth |objective| * eps.
    const double tol_gap = tol * std::max(1.0, std::abs(rep.primal_objective));
    const bool numerical = rep.primal_feasible &&
                           rep.dual_feasible &&
                           rep.complementary_slackness &&
                           (std::abs(rep.duality_gap) <= tol_gap);

    // The exact conclusion drops the objective comparison. Tight matched arcs
    // make the primal cost the sum of u over matched rows plus v over used
    // columns; free unmatched columns make the second sum the sum of v over
    // all columns; the row cover primal feasibility already requires makes the
    // first the sum of u over all rows. Those three together are the dual
    // objective, so the gap is zero and no tolerance decides it. The gap is
    // still computed and reported, as the independent numerical cross-check it
    // is.
    rep.exact_available = want_exact && scan.exact_checked;
    rep.exact_certificate = rep.exact_available &&
                            rep.primal_feasible &&
                            rep.exact_dual_feasible &&
                            rep.exact_cs_matched_tight &&
                            rep.exact_cs_unmatched_free;
    if (rep.exact_certificate) {
        // The doubles decided it, and they are the potentials returned: an
        // exact certificate always carries the potentials that decided it, so
        // re-checking one never depends on which source that was.
        rep.exact_duals_source = ExactDualsSource::solver;
        rep.exact_u.resize(u.size());
        rep.exact_v.resize(v.size());
        for (std::size_t i = 0; i < u.size(); ++i) {
            if (u[i] != 0.0) rep.exact_u[i].push_back(u[i]);
        }
        for (std::size_t j = 0; j < v.size(); ++j) {
            if (v[j] != 0.0) rep.exact_v[j].push_back(v[j]);
        }
    }

    // The doubles missed exactness. Expansions the caller holds are asked
    // next, and they are the duals the exact counts then describe.
    //
    // Failing that, a numerical pass means the doubles are optimal potentials
    // to within tol, and on computed costs a miss that small is almost always
    // the double failing to hold an optimal potential rather than the matching
    // failing to be optimal. So the potentials the matching itself determines
    // are recovered and asked instead. They prove the matching, not the duals,
    // so the counts keep describing the duals that were given; what changes is
    // the certificate, and exact_duals_source says why. Duals that do not pass
    // even the numerical reading certify nothing, and the check fails on them
    // as it always has. A matching that is not optimal has no exact potentials,
    // so a recovery that fails leaves the doubles' answer standing.
    if (want_exact && rep.primal_feasible && !rep.exact_certificate) {
        const auto adopt = [&](const std::vector<exact::Expansion>& eu,
                               const std::vector<exact::Expansion>& ev,
                               const exact::ExpansionCheck& check,
                               ExactDualsSource source, bool counts) {
            if (!check.checked) return;
            rep.exact_available = true;
            if (counts) {
                rep.n_exact_violations = check.n_violations;
                rep.n_exact_untight = check.n_untight;
                rep.exact_dual_feasible = check.n_violations == 0 && check.sign_ok;
                rep.exact_cs_matched_tight = check.n_untight == 0;
                rep.exact_cs_unmatched_free = check.unmatched_free;
            }
            if (check.holds()) {
                rep.exact_certificate = true;
                rep.exact_duals_source = source;
                rep.exact_u = eu;
                rep.exact_v = ev;
            }
        };
        if (extra.u != nullptr && extra.v != nullptr) {
            adopt(*extra.u, *extra.v,
                  extra.check != nullptr
                      ? *extra.check
                      : exact::check_expansion_duals(src, match, *extra.u, *extra.v),
                  extra.source, true);
        }
        if (!rep.exact_certificate && extra.recover && numerical) {
            const exact::AssignmentPotentials recovered =
                exact::recover_assignment_potentials(src, match, v);
            if (recovered.ok) {
                adopt(recovered.u, recovered.v,
                      exact::check_expansion_duals(src, match, recovered.u, recovered.v),
                      ExactDualsSource::recovered, false);
            }
        }
    }

    // An exact certificate proves the matching attains the optimum, so nothing
    // can beat it and the bound the doubles assembled above is replaced by the
    // number it was bounding.
    if (rep.exact_certificate) rep.max_suboptimality = 0.0;

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

}  // namespace detail

template <class Source>
CertificateReport certify_assignment(const Source& src,
                                     const std::vector<int>& match,
                                     const std::vector<double>& u,
                                     const std::vector<double>& v,
                                     double tol,
                                     Arithmetic mode = Arithmetic::Auto,
                                     const ExactDuals& extra = ExactDuals()) {
    return detail::certify_assignment_impl(src, match, u, v, tol, nullptr, mode, extra);
}

// The same certificate against a scan the caller already holds.
//
// The scan is the only part of the check that costs a pass over the pairs; the
// rest is O(nrow + ncol) over the matching and the duals. A caller that reached
// the minimum some other way -- an edge-generation loop proves it in two halves,
// one over the pairs its master holds and one over the pairs it omits -- has
// the expensive half already and would otherwise pay for it twice.
//
// The scan has to cover every admissible pair of `src`. One that covers fewer
// certifies a smaller problem than the one named here.
template <class Source>
CertificateReport certify_assignment(const Source& src,
                                     const std::vector<int>& match,
                                     const std::vector<double>& u,
                                     const std::vector<double>& v,
                                     double tol,
                                     const ReducedCostScan& scan,
                                     Arithmetic mode = Arithmetic::Auto,
                                     const ExactDuals& extra = ExactDuals()) {
    return detail::certify_assignment_impl(src, match, u, v, tol, &scan, mode, extra);
}

}  // namespace lap
