// src/flow/flow_rcpp.cpp
// R bindings for the flow model.
//
// The three steps of the module are bound separately and compose in R the way
// they compose in C++: a design compiles to a problem, the problem solves, and
// the flow and the potentials it produced are handed back to the certificate as
// an input to a check. Binding the composite instead would put the one place
// optimality is decided out of reach of R.
//
// A compiled design crosses the boundary as an explicit arc list, so the block
// is expanded here rather than in R: expansion reads the cost source through a
// virtual call per pair, which is a C++ loop and not an R one.
//
// Capacities and supplies are int64_t in the model and R has no 64-bit integer
// type, so they cross as doubles. Every count a matching design produces is far
// below 2^53, where a double is exact. The one value that is not is
// FLOW_INF_CAP, which crosses as Inf and is restored on the way back in.

#include <Rcpp.h>

#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"
#include "../core/lap_rcpp_convert.h"
#include "../core/lap_utils_rcpp.h"
#include "flow_candidates.h"
#include "flow_certify.h"
#include "flow_compile.h"
#include "flow_implicit.h"
#include "flow_implicit_rcpp.h"
#include "flow_lagrangian.h"
#include "flow_oracle.h"
#include "flow_problem.h"
#include "flow_push_relabel.h"
#include "flow_solve.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

namespace {

// Largest integer a double holds exactly. A count above it has already lost its
// low bits by the time it arrives, so it is rejected rather than rounded into a
// different problem.
constexpr double FLOW_R_EXACT_MAX = 9007199254740992.0;  // 2^53

int64_t count_from_r(double v, const char* what) {
    if (ISNAN(v)) {
        Rcpp::stop("flow model: %s is NA", what);
    }
    if (v == R_PosInf) return lap::FLOW_INF_CAP;
    if (v == R_NegInf || std::abs(v) > FLOW_R_EXACT_MAX) {
        Rcpp::stop("flow model: %s is outside the range a double counts exactly",
                   what);
    }
    if (v != std::floor(v)) {
        Rcpp::stop("flow model: %s is not a whole number", what);
    }
    return static_cast<int64_t>(v);
}

double count_to_r(int64_t v) {
    return (v >= lap::FLOW_INF_CAP) ? R_PosInf : static_cast<double>(v);
}

// The caller's cost matrix read where it lies. A compiler asks a cost source
// for one cell at a time and expansion visits each cell once, so copying the
// matrix into a lap::CostMatrix first would allocate a second copy of the
// caller's largest object to answer questions the original already answers.
//
// A pair is admissible when its distance is a number. R's NA arrives as a NaN
// payload and a caliper writes Inf, so one test covers both, and it is the
// same test expand_blocks() applies to the value it reads.
class RMatrixSource final : public lap::CostOracle {
public:
    explicit RMatrixSource(const Rcpp::NumericMatrix& mat) : mat_(mat) {}

    double at(int64_t i, int64_t j) const override {
        return mat_(static_cast<R_xlen_t>(i), static_cast<R_xlen_t>(j));
    }
    bool allowed(int64_t i, int64_t j) const override {
        return std::isfinite(at(i, j));
    }
    int64_t nrow() const override { return mat_.nrow(); }
    int64_t ncol() const override { return mat_.ncol(); }

private:
    const Rcpp::NumericMatrix& mat_;
};

// Node ids are 1-based in R and 0-based in the model, which is the only
// translation the arc columns need.
lap::FlowProblem problem_from_r(int n_nodes,
                                const Rcpp::NumericVector& supply,
                                const Rcpp::IntegerVector& tail,
                                const Rcpp::IntegerVector& head,
                                const Rcpp::NumericVector& lower,
                                const Rcpp::NumericVector& upper,
                                const Rcpp::NumericVector& cost) {
    const R_xlen_t n_arcs = tail.size();
    if (head.size() != n_arcs || lower.size() != n_arcs ||
        upper.size() != n_arcs || cost.size() != n_arcs) {
        Rcpp::stop("flow model: the arc columns have different lengths");
    }
    if (n_nodes < 0) {
        Rcpp::stop("flow model: n_nodes is negative");
    }
    if (supply.size() != n_nodes) {
        Rcpp::stop("flow model: supply has %d entries for %d nodes",
                   static_cast<int>(supply.size()), n_nodes);
    }

    lap::FlowProblem prob;
    prob.n_nodes = n_nodes;
    prob.supply.resize(static_cast<std::size_t>(n_nodes));
    for (int v = 0; v < n_nodes; ++v) {
        prob.supply[static_cast<std::size_t>(v)] =
            count_from_r(supply[v], "a supply");
    }

    prob.arcs.reserve(static_cast<std::size_t>(n_arcs));
    for (R_xlen_t a = 0; a < n_arcs; ++a) {
        if (tail[a] == NA_INTEGER || head[a] == NA_INTEGER) {
            Rcpp::stop("flow model: an arc endpoint is NA");
        }
        prob.arcs.emplace_back(static_cast<int32_t>(tail[a] - 1),
                               static_cast<int32_t>(head[a] - 1),
                               count_from_r(lower[a], "an arc lower bound"),
                               count_from_r(upper[a], "an arc upper bound"),
                               cost[a]);
    }

    // There are no blocks, so the arc array is already the whole problem and the
    // certificate may read it as an expanded one.
    prob.expanded = true;
    return prob;
}

// The stop test the solver is asked between augmentations. Two different
// answers to two different questions: a user interrupt unwinds the solve and
// reaches R as an interrupt condition, so a caller's tryCatch(interrupt = )
// sees the same thing it would see from an interrupted R loop; a spent time
// budget returns true, and the solve comes back with status "interrupted" and
// the flow it had reached. Nothing about a budget is exceptional, so nothing
// about it throws.
//
// The clock is steady rather than wall: a time budget measures work done, and a
// system clock stepping backwards would hand a solve an unbounded one.
class SolveDeadline {
public:
    explicit SolveDeadline(double seconds)
        : seconds_(seconds), start_(std::chrono::steady_clock::now()) {}

    bool operator()() {
        Rcpp::checkUserInterrupt();
        if (!R_FINITE(seconds_)) return false;
        const std::chrono::duration<double> spent =
            std::chrono::steady_clock::now() - start_;
        return spent.count() >= seconds_;
    }

private:
    double seconds_;
    std::chrono::steady_clock::time_point start_;
};

// A warm start crosses as counts, on the same terms as a capacity: whole
// numbers a double holds exactly. An empty vector is a cold start, which is
// what the FlowProblem's own empty vector already means.
std::vector<int64_t> warm_flow_from_r(const Rcpp::NumericVector& warm_flow,
                                      R_xlen_t n_arcs) {
    std::vector<int64_t> out;
    if (warm_flow.size() == 0) return out;
    if (warm_flow.size() != n_arcs) {
        Rcpp::stop("flow model: warm_flow has %d entries for %d arcs",
                   static_cast<int>(warm_flow.size()), static_cast<int>(n_arcs));
    }
    out.reserve(static_cast<std::size_t>(n_arcs));
    for (R_xlen_t a = 0; a < n_arcs; ++a) {
        out.push_back(count_from_r(warm_flow[a], "a warm flow"));
    }
    return out;
}

std::vector<double> warm_potential_from_r(const Rcpp::NumericVector& warm_potential,
                                          int n_nodes) {
    std::vector<double> out;
    if (warm_potential.size() == 0) return out;
    if (warm_potential.size() != n_nodes) {
        Rcpp::stop("flow model: warm_potential has %d entries for %d nodes",
                   static_cast<int>(warm_potential.size()), n_nodes);
    }
    for (R_xlen_t v = 0; v < warm_potential.size(); ++v) {
        if (!R_FINITE(warm_potential[v])) {
            Rcpp::stop("flow model: a warm potential is not finite");
        }
        out.push_back(warm_potential[v]);
    }
    return out;
}

// A flow that is not integral, or that carries a count no int64_t holds, is not
// a flow the LP has a primal for, and false says so rather than rounding it into
// one.
bool integral_flow_from_r(const Rcpp::NumericVector& flow, std::vector<int64_t>& out) {
    out.clear();
    out.reserve(static_cast<std::size_t>(flow.size()));
    for (R_xlen_t a = 0; a < flow.size(); ++a) {
        const double v = flow[a];
        if (ISNAN(v) || std::abs(v) > FLOW_R_EXACT_MAX || v != std::floor(v)) {
            return false;
        }
        out.push_back(static_cast<int64_t>(v));
    }
    return true;
}

// 1-based indices from R, 0-based and range-checked.
std::vector<int32_t> indices_from_r(const Rcpp::IntegerVector& x, int64_t limit,
                                    const char* what) {
    std::vector<int32_t> out(static_cast<std::size_t>(x.size()));
    for (R_xlen_t k = 0; k < x.size(); ++k) {
        if (x[k] == NA_INTEGER || x[k] < 1 || x[k] > limit) {
            Rcpp::stop("lagrangian step: %s holds an index outside 1..%d", what,
                       static_cast<int>(limit));
        }
        out[static_cast<std::size_t>(k)] = x[k] - 1;
    }
    return out;
}

Rcpp::NumericVector expansion_to_r(const lap::exact::Expansion& e) {
    return Rcpp::NumericVector(e.begin(), e.end());
}

Rcpp::List certificate_to_r(const lap::FlowCertificate& cert) {
    Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("primal_feasible") = cert.primal_feasible,
        Rcpp::Named("n_capacity_violations") =
            static_cast<double>(cert.n_capacity_violations),
        Rcpp::Named("n_conservation_violations") =
            static_cast<double>(cert.n_conservation_violations),
        Rcpp::Named("max_conservation_error") = cert.max_conservation_error,
        Rcpp::Named("primal_objective") = cert.primal_objective,
        Rcpp::Named("dual_feasible") = cert.dual_feasible,
        Rcpp::Named("min_residual_reduced_cost") = cert.min_residual_reduced_cost,
        Rcpp::Named("dual_tolerance") = cert.dual_tolerance,
        Rcpp::Named("worst_arc") = static_cast<double>(cert.worst_arc),
        Rcpp::Named("complementary_slackness") = cert.complementary_slackness,
        Rcpp::Named("n_cs_violations") = static_cast<double>(cert.n_cs_violations),
        Rcpp::Named("dual_objective") = cert.dual_objective,
        Rcpp::Named("duality_gap") = cert.duality_gap,
        Rcpp::Named("certified_optimal") = cert.certified_optimal,
        Rcpp::Named("tolerance") = cert.tolerance);
    out.push_back(cert.exact_available, "exact_available");
    out.push_back(cert.exact_certificate, "exact_certificate");
    out.push_back(static_cast<double>(cert.n_exact_violations), "n_exact_violations");
    out.push_back(std::string(exact_source_name(cert.exact_potentials_source)),
                  "exact_potentials_source");
    out.push_back(cert.exact_potential.empty()
                      ? Rcpp::RObject(R_NilValue)
                      : Rcpp::RObject(expansions_to_r(cert.exact_potential)),
                  "exact_potential");
    out.push_back(cert.conclusion_is_exact, "conclusion_is_exact");
    return out;
}

}  // namespace

Rcpp::List flow_solve_impl(int n_nodes,
                           Rcpp::NumericVector supply,
                           Rcpp::IntegerVector tail,
                           Rcpp::IntegerVector head,
                           Rcpp::NumericVector lower,
                           Rcpp::NumericVector upper,
                           Rcpp::NumericVector cost,
                           Rcpp::NumericVector warm_flow,
                           Rcpp::NumericVector warm_potential,
                           double time_limit,
                           double tol,
                           double relax_eps,
                           double max_augmentations,
                           bool return_potentials) {
    lap::FlowProblem prob =
        problem_from_r(n_nodes, supply, tail, head, lower, upper, cost);
    prob.warm_flow = warm_flow_from_r(warm_flow, tail.size());
    prob.warm_potential = warm_potential_from_r(warm_potential, n_nodes);

    lap::FlowOptions opts;
    opts.tol = tol;
    opts.relax_eps = relax_eps;
    opts.max_augmentations = count_from_r(max_augmentations, "max_augmentations");
    opts.return_potentials = return_potentials;

    SolveDeadline deadline(time_limit);
    opts.should_stop = std::ref(deadline);

    const lap::FlowResult res = lap::solve_min_cost_flow(prob, opts);

    Rcpp::NumericVector flow(static_cast<R_xlen_t>(res.flow.size()));
    for (std::size_t a = 0; a < res.flow.size(); ++a) {
        flow[static_cast<R_xlen_t>(a)] = static_cast<double>(res.flow[a]);
    }
    Rcpp::NumericVector potential(static_cast<R_xlen_t>(res.potential.size()));
    for (std::size_t v = 0; v < res.potential.size(); ++v) {
        potential[static_cast<R_xlen_t>(v)] = res.potential[v];
    }

    return Rcpp::List::create(
        Rcpp::Named("flow") = flow,
        Rcpp::Named("potential") = potential,
        Rcpp::Named("total_cost") = res.total_cost,
        Rcpp::Named("flow_sent") = static_cast<double>(res.flow_sent),
        Rcpp::Named("flow_required") = static_cast<double>(res.flow_required),
        Rcpp::Named("status") = res.status,
        Rcpp::Named("n_augmentations") = static_cast<double>(res.n_augmentations));
}

Rcpp::List flow_certify_impl(int n_nodes,
                             Rcpp::NumericVector supply,
                             Rcpp::IntegerVector tail,
                             Rcpp::IntegerVector head,
                             Rcpp::NumericVector lower,
                             Rcpp::NumericVector upper,
                             Rcpp::NumericVector cost,
                             Rcpp::NumericVector flow,
                             Rcpp::NumericVector potential,
                             double tol,
                             std::string arithmetic,
                             Rcpp::Nullable<Rcpp::NumericMatrix> exact_potential) {
    const lap::FlowProblem prob =
        problem_from_r(n_nodes, supply, tail, head, lower, upper, cost);

    std::vector<int64_t> f;
    if (!integral_flow_from_r(flow, f)) {
        lap::FlowCertificate rep;
        rep.tolerance = tol;
        return certificate_to_r(rep);
    }

    std::vector<double> pi(potential.begin(), potential.end());
    std::vector<lap::exact::Expansion> supplied;
    if (exact_potential.isNotNull()) {
        supplied = expansions_from_r(Rcpp::NumericMatrix(exact_potential.get()), false);
    }
    return certificate_to_r(lap::certify_flow(
        prob, f, pi, tol, arithmetic_from_string(arithmetic),
        exact_potential.isNotNull() ? &supplied : nullptr, /*recover=*/true));
}

// One solve of the balance network read exactly under moment multipliers: see
// flow_lagrangian.h. The problem crosses with its base costs, and the pair arcs
// with the units behind them; `u` and `w` hold one column per moment row.
Rcpp::List flow_lagrangian_step_impl(int n_nodes,
                                     Rcpp::NumericVector supply,
                                     Rcpp::IntegerVector tail,
                                     Rcpp::IntegerVector head,
                                     Rcpp::NumericVector lower,
                                     Rcpp::NumericVector upper,
                                     Rcpp::NumericVector cost,
                                     Rcpp::NumericVector flow,
                                     Rcpp::NumericVector potential,
                                     Rcpp::IntegerVector pair_arc,
                                     Rcpp::IntegerVector pair_left,
                                     Rcpp::IntegerVector pair_right,
                                     Rcpp::IntegerVector left_node,
                                     Rcpp::IntegerVector right_node,
                                     Rcpp::NumericMatrix u,
                                     Rcpp::NumericMatrix w,
                                     Rcpp::NumericVector b,
                                     Rcpp::NumericVector lambda) {
    const lap::FlowProblem prob =
        problem_from_r(n_nodes, supply, tail, head, lower, upper, cost);
    std::vector<int64_t> f;
    if (!integral_flow_from_r(flow, f) ||
        static_cast<std::size_t>(flow.size()) != prob.arcs.size()) {
        Rcpp::stop("lagrangian step: the flow is not an integral flow of the network");
    }
    if (potential.size() != n_nodes) {
        Rcpp::stop("lagrangian step: %d potentials for %d nodes",
                   static_cast<int>(potential.size()), n_nodes);
    }
    const R_xlen_t n_pairs = pair_arc.size();
    if (pair_left.size() != n_pairs || pair_right.size() != n_pairs) {
        Rcpp::stop("lagrangian step: the pair columns have different lengths");
    }

    lap::MomentRows rows;
    rows.n_rows = b.size();
    rows.n_left = left_node.size();
    rows.n_right = right_node.size();
    if (lambda.size() != b.size() || u.ncol() != b.size() || w.ncol() != b.size() ||
        u.nrow() != rows.n_left || w.nrow() != rows.n_right) {
        Rcpp::stop("lagrangian step: the moment rows and multipliers do not describe "
                   "each other");
    }
    rows.u.assign(u.begin(), u.end());
    rows.w.assign(w.begin(), w.end());
    rows.b.assign(b.begin(), b.end());
    for (double x : rows.u) if (!std::isfinite(x)) Rcpp::stop("lagrangian step: u is not finite");
    for (double x : rows.w) if (!std::isfinite(x)) Rcpp::stop("lagrangian step: w is not finite");
    for (double x : rows.b) if (!std::isfinite(x)) Rcpp::stop("lagrangian step: b is not finite");

    lap::PairArcs pairs;
    const std::vector<int32_t> arcs =
        indices_from_r(pair_arc, static_cast<int64_t>(prob.arcs.size()), "pair_arc");
    pairs.arc.assign(arcs.begin(), arcs.end());
    pairs.left = indices_from_r(pair_left, rows.n_left, "pair_left");
    pairs.right = indices_from_r(pair_right, rows.n_right, "pair_right");
    pairs.left_node = indices_from_r(left_node, n_nodes, "left_node");
    pairs.right_node = indices_from_r(right_node, n_nodes, "right_node");

    std::vector<double> lam(lambda.begin(), lambda.end());
    for (double x : lam) {
        if (!std::isfinite(x) || x < 0.0) {
            Rcpp::stop("lagrangian step: the multipliers must be finite and non-negative");
        }
    }
    const std::vector<double> pi(potential.begin(), potential.end());

    const lap::LagrangianStep step = lap::lagrangian_step(prob, f, pi, pairs, rows, lam);
    Rcpp::NumericVector flow_out(static_cast<R_xlen_t>(step.flow.size()));
    for (std::size_t a = 0; a < step.flow.size(); ++a) {
        flow_out[static_cast<R_xlen_t>(a)] = static_cast<double>(step.flow[a]);
    }
    // The potentials rounded to doubles: what a warm start and the reported
    // pair potentials read.
    Rcpp::NumericVector potential_out(static_cast<R_xlen_t>(step.pi.size()));
    for (std::size_t v = 0; v < step.pi.size(); ++v) {
        potential_out[static_cast<R_xlen_t>(v)] = lap::exact::approximate(step.pi[v]).value;
    }
    return Rcpp::List::create(
        Rcpp::Named("flow") = flow_out,
        Rcpp::Named("potential") = potential_out,
        Rcpp::Named("n_cancelled") = static_cast<double>(step.n_cancelled),
        Rcpp::Named("recovered") = step.recovered,
        Rcpp::Named("bound") = lap::exact::round_down(step.bound),
        Rcpp::Named("bound_exact") = expansion_to_r(step.bound),
        Rcpp::Named("pair_u") = expansions_to_r(step.pair_u),
        Rcpp::Named("pair_v") = expansions_to_r(step.pair_v));
}

// The designs match_couples() offers, compiled and routed. The caller names the
// design and states its shape; what comes back is how that design has to be
// solved and the maps that carry a solved answer back to the caller's units.
//
// Only the shape crosses. Which network a design compiles to, and which unit
// each of its nodes stands for, follow from the row and column counts alone, so
// the prices stay in the caller's matrix and the lowered problem is solved from
// there.
//
// Each design names the structural property its solve relies on, and the
// compiled network is checked against that property here rather than assumed to
// have it. A design that compiled to something else is an error and not a
// route: a network solved as a problem it is not is a wrong answer that
// validates and solves.
Rcpp::List flow_compile_couples_impl(std::string design,
                                     double      n_rows,
                                     double      n_cols,
                                     double      ratio) {
    const int64_t nr = count_from_r(n_rows, "the row count");
    const int64_t nc = count_from_r(n_cols, "the column count");
    const int64_t k  = count_from_r(ratio, "ratio");
    if (nr < 0 || nc < 0) {
        Rcpp::stop("flow model: a design cannot have a negative number of units");
    }

    const lap::ShapeOracle                     shape(nr, nc);
    const std::vector<lap::CategoryConstraint> no_categories;

    lap::CompiledDesign compiled;
    std::string         route;
    if (design == "one_to_one") {
        compiled = lap::compile_one_to_one(shape, no_categories);
        route    = "assignment";
    } else if (design == "fixed_ratio") {
        compiled = lap::compile_fixed_ratio(shape, k, no_categories);
        route    = "assignment";
    } else if (design == "with_replacement") {
        compiled = lap::compile_with_replacement(shape, k, no_categories);
        route    = "separable";
    } else {
        Rcpp::stop("flow model: there is no compiler for the design '%s'", design);
    }

    std::vector<int32_t> row_unit;
    std::vector<int32_t> col_unit;
    if (route == "assignment") {
        const lap::LoweredAssignment lowered = lap::lower_to_assignment(compiled);
        if (!lowered.valid) {
            Rcpp::stop("flow model: the '%s' design did not compile to an assignment",
                       design);
        }
        row_unit = lowered.row_unit;
        col_unit = lowered.col_unit;
    } else {
        if (!lap::is_row_separable(compiled.problem)) {
            Rcpp::stop("flow model: the '%s' design did not compile to a network its "
                       "rows can be solved apart in",
                       design);
        }
        row_unit = compiled.row_unit;
        col_unit = compiled.col_unit;
    }

    // A map that is not the identity is a design that reshaped its input, and
    // the cost matrix the lowered problem is solved from is the caller's read
    // through the maps rather than the caller's matrix itself.
    bool reshaped = row_unit.size() != static_cast<std::size_t>(nr) ||
                    col_unit.size() != static_cast<std::size_t>(nc);

    Rcpp::IntegerVector rows(static_cast<R_xlen_t>(row_unit.size()));
    for (std::size_t e = 0; e < row_unit.size(); ++e) {
        if (row_unit[e] != static_cast<int32_t>(e)) reshaped = true;
        rows[static_cast<R_xlen_t>(e)] = row_unit[e] + 1;
    }
    Rcpp::IntegerVector cols(static_cast<R_xlen_t>(col_unit.size()));
    for (std::size_t e = 0; e < col_unit.size(); ++e) {
        if (col_unit[e] != static_cast<int32_t>(e)) reshaped = true;
        cols[static_cast<R_xlen_t>(e)] = col_unit[e] + 1;
    }

    // What one row carries. The assignment designs put one unit on a row node;
    // matching with replacement puts the row's whole quota there, which is the
    // number of columns that row takes.
    const int64_t per_row =
        (compiled.n_rows > 0)
            ? compiled.problem.supply[static_cast<std::size_t>(compiled.row_base)]
            : 0;

    return Rcpp::List::create(
        Rcpp::Named("design") = design,
        Rcpp::Named("route") = route,
        Rcpp::Named("reshaped") = reshaped,
        Rcpp::Named("per_row") = count_to_r(per_row),
        Rcpp::Named("flow_required") = count_to_r(compiled.flow_required),
        Rcpp::Named("n_nodes") = static_cast<int>(compiled.problem.n_nodes),
        Rcpp::Named("row_unit") = rows,
        Rcpp::Named("col_unit") = cols);
}

// The per-search record of an assignment solve, in the shape the trace layer
// renders. Node ids come back 1-based; a row node is `source`/`sink` reported
// as its own row or column index, so the R side never has to know the node
// layout, and the path arcs come back as the (row, col) pairs they are.
Rcpp::List flow_trace_assignment_impl(Rcpp::NumericMatrix cost, bool maximize) {
    const int nr = cost.nrow();
    const int nc = cost.ncol();

    Rcpp::NumericMatrix work(nr, nc);
    for (int j = 0; j < nc; ++j) {
        for (int i = 0; i < nr; ++i) {
            const double x = cost(i, j);
            work(i, j) = maximize ? -x : x;
        }
    }

    const RMatrixSource source(work);
    lap::CompiledDesign design =
        lap::compile_one_to_one(source, std::vector<lap::CategoryConstraint>());

    lap::FlowTrace  trace;
    lap::FlowOptions opts;
    opts.trace = &trace;
    lap::FlowResult res = lap::solve_min_cost_flow(design.problem, opts);

    // Every pair arc of the one block the compiler emitted, keyed by arc index,
    // so a path arc names the (row, col) it crosses.
    const lap::BlockArcRange& block = design.problem.block_arcs[0];
    std::vector<int32_t> arc_row(design.problem.arcs.size(), -1);
    std::vector<int32_t> arc_col(design.problem.arcs.size(), -1);
    for (int64_t k = 0; k < block.n_arcs; ++k) {
        const std::pair<int32_t, int32_t>& rc =
            block.rc[static_cast<std::size_t>(k)];
        const std::size_t a = static_cast<std::size_t>(block.first_arc + k);
        arc_row[a] = rc.first;
        arc_col[a] = rc.second;
    }

    Rcpp::IntegerVector match(nr, NA_INTEGER);
    for (int64_t k = 0; k < block.n_arcs; ++k) {
        if (res.flow[static_cast<std::size_t>(block.first_arc + k)] <= 0) continue;
        const std::pair<int32_t, int32_t>& rc =
            block.rc[static_cast<std::size_t>(k)];
        match[rc.first] = rc.second + 1;
    }

    const int32_t row_base = design.row_base;
    const int32_t col_base = design.col_base;

    // A node reported to R as the unit it stands for: a row index, a negative
    // column index, or 0 for the source and the sink, which have no unit.
    auto unit_of = [&](int32_t v) -> int {
        if (v >= col_base && v < col_base + nc) return -(v - col_base + 1);
        if (v >= row_base && v < row_base + nr) return v - row_base + 1;
        return 0;
    };

    Rcpp::List steps(static_cast<R_xlen_t>(trace.steps.size()));
    for (std::size_t s = 0; s < trace.steps.size(); ++s) {
        const lap::FlowStep& st = trace.steps[s];

        const R_xlen_t nl = static_cast<R_xlen_t>(st.labelled.size());
        Rcpp::IntegerVector lab(nl);
        Rcpp::NumericVector lab_d(nl);
        Rcpp::IntegerVector t_row(nl);
        Rcpp::IntegerVector t_col(nl);
        for (R_xlen_t e = 0; e < nl; ++e) {
            lab[e]   = unit_of(st.labelled[static_cast<std::size_t>(e)]);
            lab_d[e] = st.dist[static_cast<std::size_t>(e)];
            const int64_t pa = st.pred_arcs[static_cast<std::size_t>(e)];
            if (pa < 0 || arc_row[static_cast<std::size_t>(pa)] < 0) {
                t_row[e] = NA_INTEGER;
                t_col[e] = NA_INTEGER;
            } else {
                t_row[e] = arc_row[static_cast<std::size_t>(pa)] + 1;
                t_col[e] = arc_col[static_cast<std::size_t>(pa)] + 1;
            }
        }

        const R_xlen_t np = static_cast<R_xlen_t>(st.path_arcs.size());
        Rcpp::IntegerVector p_row(np);
        Rcpp::IntegerVector p_col(np);
        Rcpp::LogicalVector p_fwd(np);
        int free_col = 0;
        for (R_xlen_t e = 0; e < np; ++e) {
            const std::size_t a =
                static_cast<std::size_t>(st.path_arcs[static_cast<std::size_t>(e)]);
            p_row[e] = (arc_row[a] < 0) ? NA_INTEGER : arc_row[a] + 1;
            p_col[e] = (arc_col[a] < 0) ? NA_INTEGER : arc_col[a] + 1;
            p_fwd[e] = st.path_forward[static_cast<std::size_t>(e)] != 0;
            if (arc_col[a] >= 0) free_col = arc_col[a] + 1;
        }

        Rcpp::NumericVector pot(static_cast<R_xlen_t>(st.potential.size()));
        for (R_xlen_t v = 0; v < pot.size(); ++v) {
            pot[v] = st.potential[static_cast<std::size_t>(v)];
        }

        steps[static_cast<R_xlen_t>(s)] = Rcpp::List::create(
            Rcpp::Named("source")    = unit_of(st.source),
            Rcpp::Named("sink")      = (st.sink < 0) ? 0 : unit_of(st.sink),
            Rcpp::Named("reached")   = (st.sink >= 0),
            Rcpp::Named("reach")     = st.reach,
            Rcpp::Named("units")     = static_cast<double>(st.units),
            Rcpp::Named("free_col")  = free_col,
            Rcpp::Named("labelled")  = lab,
            Rcpp::Named("dist")      = lab_d,
            Rcpp::Named("tree_row")  = t_row,
            Rcpp::Named("tree_col")  = t_col,
            Rcpp::Named("potential") = pot,
            Rcpp::Named("path_row")  = p_row,
            Rcpp::Named("path_col")  = p_col,
            Rcpp::Named("path_forward") = p_fwd);
    }

    Rcpp::NumericVector pot0(static_cast<R_xlen_t>(trace.potential_initial.size()));
    for (R_xlen_t v = 0; v < pot0.size(); ++v) {
        pot0[v] = trace.potential_initial[static_cast<std::size_t>(v)];
    }

    return Rcpp::List::create(
        Rcpp::Named("n_rows")    = nr,
        Rcpp::Named("n_cols")    = nc,
        Rcpp::Named("row_base")  = static_cast<int>(row_base) + 1,
        Rcpp::Named("col_base")  = static_cast<int>(col_base) + 1,
        Rcpp::Named("potential_initial") = pot0,
        Rcpp::Named("steps")     = steps,
        Rcpp::Named("match")     = match,
        Rcpp::Named("status")    = res.status,
        Rcpp::Named("total_cost") = maximize ? -res.total_cost : res.total_cost);
}

// The per-phase record of a push-relabel assignment solve, in the shape the
// trace layer renders. Duals come back in LAP form: the reduced cost of a pair
// is c(i, j) - u[i] - v[j], which the node potentials give as u[i] = -pi[row_i]
// and v[j] = pi[col_j].
Rcpp::List flow_trace_push_relabel_impl(Rcpp::NumericMatrix cost, bool maximize) {
    const int nr = cost.nrow();
    const int nc = cost.ncol();

    // The scaling bound is an integer bound, so the same preparation the solver
    // does is done here: negate for a maximizing run, then scale the finite
    // costs to integers.
    double max_abs = 0.0;
    bool all_integer = true;
    for (int j = 0; j < nc; ++j) {
        for (int i = 0; i < nr; ++i) {
            const double x = cost(i, j);
            if (!std::isfinite(x)) continue;
            max_abs = std::max(max_abs, std::abs(x));
            if (all_integer && std::abs(x - std::round(x)) > 1e-9) {
                all_integer = false;
            }
        }
    }
    const double scale = (!all_integer && max_abs > 0.0) ? 1e6 / max_abs : 1.0;

    Rcpp::NumericMatrix work(nr, nc);
    for (int j = 0; j < nc; ++j) {
        for (int i = 0; i < nr; ++i) {
            const double x = cost(i, j);
            if (!std::isfinite(x)) { work(i, j) = x; continue; }
            work(i, j) = std::round((maximize ? -x : x) * scale);
        }
    }

    const RMatrixSource source(work);
    lap::CompiledDesign design =
        lap::compile_one_to_one(source, std::vector<lap::CategoryConstraint>());

    lap::PRTrace    trace;
    lap::FlowOptions opts;
    lap::FlowResult  res =
        lap::solve_min_cost_flow_push_relabel(design.problem, opts, &trace);

    const lap::BlockArcRange& block = design.problem.block_arcs[0];
    const int32_t row_base = design.row_base;
    const int32_t col_base = design.col_base;

    // The matching a phase's flow stands for, read off the block's own arcs.
    auto match_of = [&](const std::vector<int64_t>& flow) {
        Rcpp::IntegerVector out(nr, NA_INTEGER);
        for (int64_t k = 0; k < block.n_arcs; ++k) {
            if (flow[static_cast<std::size_t>(block.first_arc + k)] <= 0) continue;
            const std::pair<int32_t, int32_t>& rc =
                block.rc[static_cast<std::size_t>(k)];
            out[rc.first] = rc.second + 1;
        }
        return out;
    };

    // Node potentials to LAP duals, undoing the scaling so the numbers are on
    // the caller's cost scale.
    auto duals_of = [&](const std::vector<double>& pot) {
        Rcpp::NumericVector u(nr), v(nc);
        for (int i = 0; i < nr; ++i) {
            u[i] = -pot[static_cast<std::size_t>(row_base + i)] / scale;
        }
        for (int j = 0; j < nc; ++j) {
            v[j] = pot[static_cast<std::size_t>(col_base + j)] / scale;
        }
        return Rcpp::List::create(Rcpp::Named("u") = u, Rcpp::Named("v") = v);
    };

    Rcpp::List phases(static_cast<R_xlen_t>(trace.phases.size()));
    for (std::size_t s = 0; s < trace.phases.size(); ++s) {
        const lap::PRPhase& ph = trace.phases[s];
        const Rcpp::List d = duals_of(ph.potential);
        phases[static_cast<R_xlen_t>(s)] = Rcpp::List::create(
            Rcpp::Named("eps")         = ph.eps / scale,
            Rcpp::Named("n_saturated") = static_cast<double>(ph.n_saturated),
            Rcpp::Named("n_pushes")    = static_cast<double>(ph.n_pushes),
            Rcpp::Named("n_relabels")  = static_cast<double>(ph.n_relabels),
            Rcpp::Named("dual_u")      = d["u"],
            Rcpp::Named("dual_v")      = d["v"],
            Rcpp::Named("match")       = match_of(ph.flow));
    }

    const double eps_start =
        trace.phases.empty() ? 0.0 : trace.eps_start / scale;

    return Rcpp::List::create(
        Rcpp::Named("n_rows")    = nr,
        Rcpp::Named("n_cols")    = nc,
        Rcpp::Named("eps_start") = eps_start,
        Rcpp::Named("phases")    = phases,
        Rcpp::Named("match")     = match_of(res.flow),
        Rcpp::Named("status")    = res.status);
}

namespace {

// What a compiled full matching is, beside its arcs: the orientation and the
// bounds the reading step needs, and where its nodes sit.
Rcpp::List full_match_shape_to_r(const lap::CompiledFullMatch& design) {
    return Rcpp::List::create(
        Rcpp::Named("symmetric") = design.symmetric,
        Rcpp::Named("n_centres") = count_to_r(design.n_centres),
        Rcpp::Named("n_units") = count_to_r(design.n_units),
        Rcpp::Named("min_controls") = count_to_r(design.min_controls),
        Rcpp::Named("max_capacity") = count_to_r(design.max_capacity),
        Rcpp::Named("flow_required") = count_to_r(design.design.flow_required));
}

Rcpp::List design_layout_to_r(const lap::CompiledDesign& design) {
    return Rcpp::List::create(
        Rcpp::Named("source_node") = static_cast<int>(lap::FLOW_SOURCE) + 1,
        Rcpp::Named("sink_node") = static_cast<int>(lap::FLOW_SINK) + 1,
        Rcpp::Named("row_base") = static_cast<int>(design.row_base) + 1,
        Rcpp::Named("n_rows") = static_cast<int>(design.n_rows),
        Rcpp::Named("col_base") = static_cast<int>(design.col_base) + 1,
        Rcpp::Named("n_cols") = static_cast<int>(design.n_cols));
}

// The pairs block 0 became arcs for, 1-based, with the index of the first one.
Rcpp::List block_to_r(const lap::FlowProblem& prob, int64_t first_arc) {
    const lap::BlockArcRange& blk = prob.block_arcs.at(0);
    Rcpp::IntegerVector block_row(static_cast<R_xlen_t>(blk.n_arcs));
    Rcpp::IntegerVector block_col(static_cast<R_xlen_t>(blk.n_arcs));
    for (int64_t k = 0; k < blk.n_arcs; ++k) {
        block_row[static_cast<R_xlen_t>(k)] = blk.rc[static_cast<std::size_t>(k)].first + 1;
        block_col[static_cast<R_xlen_t>(k)] = blk.rc[static_cast<std::size_t>(k)].second + 1;
    }
    return Rcpp::List::create(
        Rcpp::Named("first_arc") = static_cast<double>(first_arc + 1),
        Rcpp::Named("n_arcs") = static_cast<double>(blk.n_arcs),
        Rcpp::Named("row") = block_row,
        Rcpp::Named("col") = block_col);
}

}  // namespace

Rcpp::List flow_compile_full_match_impl(Rcpp::NumericMatrix cost,
                                        double min_controls,
                                        double max_controls) {
    const RMatrixSource source(cost);
    lap::CompiledFullMatch design = lap::compile_full_matching(
        source,
        count_from_r(min_controls, "min_controls"),
        count_from_r(max_controls, "max_controls"),
        std::vector<lap::CategoryConstraint>());

    Rcpp::List shape = full_match_shape_to_r(design);

    if (!design.bounds_feasible) {
        return Rcpp::List::create(
            Rcpp::Named("bounds_feasible") = false,
            Rcpp::Named("reason") = design.reason,
            Rcpp::Named("shape") = shape);
    }

    lap::FlowProblem& prob = design.design.problem;
    lap::expand_blocks(prob);

    const R_xlen_t n_arcs = static_cast<R_xlen_t>(prob.arcs.size());
    Rcpp::IntegerVector tail(n_arcs);
    Rcpp::IntegerVector head(n_arcs);
    Rcpp::NumericVector lower(n_arcs);
    Rcpp::NumericVector upper(n_arcs);
    Rcpp::NumericVector arc_cost(n_arcs);
    for (R_xlen_t a = 0; a < n_arcs; ++a) {
        const lap::FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        tail[a] = arc.tail + 1;
        head[a] = arc.head + 1;
        lower[a] = count_to_r(arc.lower);
        upper[a] = count_to_r(arc.upper);
        arc_cost[a] = arc.cost;
    }

    Rcpp::NumericVector supply(prob.n_nodes);
    for (int v = 0; v < prob.n_nodes; ++v) {
        supply[v] = static_cast<double>(prob.supply[static_cast<std::size_t>(v)]);
    }

    return Rcpp::List::create(
        Rcpp::Named("bounds_feasible") = true,
        Rcpp::Named("reason") = design.reason,
        Rcpp::Named("shape") = shape,
        Rcpp::Named("problem") = Rcpp::List::create(
            Rcpp::Named("n_nodes") = static_cast<int>(prob.n_nodes),
            Rcpp::Named("supply") = supply,
            Rcpp::Named("tail") = tail,
            Rcpp::Named("head") = head,
            Rcpp::Named("lower") = lower,
            Rcpp::Named("upper") = upper,
            Rcpp::Named("cost") = arc_cost),
        Rcpp::Named("block") = block_to_r(prob, prob.block_arcs.at(0).first_arc),
        Rcpp::Named("layout") = design_layout_to_r(design.design));
}

// A design over a lazy cost source, solved by generating the pairs its flow
// turns out to need. `design` names the compiler: "full_match" takes
// min_controls and max_controls as its two parameters, "one_to_one" takes
// none, and is what recovers the largest cheapest matching when no complete one
// exists. What comes back has the dense binding's shape where the two describe
// the same thing -- the block's pairs, the flow on them and the layout -- so R
// reads either one way. The block holds the pairs the search generated rather
// than every admissible pair, so its flow is returned beside it, and the
// problem's own arcs stay in C++.
Rcpp::List flow_design_implicit_impl(Rcpp::NumericMatrix left_mat,
                                     Rcpp::NumericMatrix right_mat,
                                     SEXP distance,
                                     Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                     double max_distance, Rcpp::List calipers,
                                     std::string design,
                                     double first, double second,
                                     double keep_per_row, double width, double tol,
                                     double max_rounds, bool certify) {
    try {
        const LazySource source = rcpp_lazy_source(left_mat, right_mat, distance, inv_cov,
                                                   max_distance, calipers, false);
        return std::visit([&](const auto& src) -> Rcpp::List {
            using Source = std::decay_t<decltype(src)>;
            const lap::SourceOracle<Source> oracle(src);
            const std::vector<lap::CategoryConstraint> none;

            lap::CompiledDesign compiled;
            Rcpp::RObject shape = R_NilValue;
            if (design == "full_match") {
                lap::CompiledFullMatch fm = lap::compile_full_matching(
                    oracle, count_from_r(first, "min_controls"),
                    count_from_r(second, "max_controls"), none);
                shape = full_match_shape_to_r(fm);
                if (!fm.bounds_feasible) {
                    return Rcpp::List::create(
                        Rcpp::Named("bounds_feasible") = false,
                        Rcpp::Named("reason") = fm.reason,
                        Rcpp::Named("shape") = shape);
                }
                compiled = std::move(fm.design);
            } else if (design == "one_to_one") {
                compiled = lap::compile_one_to_one(oracle, none);
            } else {
                Rcpp::stop("flow model: no implicit compiler for design '%s'", design.c_str());
            }

            lap::CandidateSet cand(src.nrow, src.ncol);
            const lap::ImplicitOptions opts =
                implicit_options_from_r(keep_per_row, width, tol, max_rounds, certify);
            const lap::DesignResult res =
                lap::solve_implicit_design(src, compiled.problem, cand, opts);
            const lap::FlowProblem& prob = compiled.problem;
            if (design == "full_match" && lap::min_distance_seen(src) < 0.0) {
                Rcpp::stop("full_match() needs non-negative distances: the cheapest "
                           "edge cover is a full matching only when no arc is worth "
                           "keeping for its own sake, and the distance function "
                           "returned %g. Shift it so its smallest value is zero.",
                           lap::min_distance_seen(src));
            }

            const lap::BlockArcRange& blk = prob.block_arcs.at(0);
            Rcpp::NumericVector block_flow(static_cast<R_xlen_t>(blk.n_arcs));
            for (int64_t k = 0; k < blk.n_arcs; ++k) {
                const std::size_t a = static_cast<std::size_t>(blk.first_arc + k);
                block_flow[static_cast<R_xlen_t>(k)] =
                    a < res.flow.size() ? static_cast<double>(res.flow[a]) : 0.0;
            }

            Rcpp::RObject certificate = R_NilValue;
            if (certify && !res.flow.empty() && res.flow_certificate.tolerance > 0.0) {
                Rcpp::List cert = certificate_to_r(res.flow_certificate);
                cert.push_back(res.flow_certificate.certified_optimal, "master_certified");
                cert.push_back(res.omitted_min_reduced_cost, "omitted_min_reduced_cost");
                cert.push_back(res.omitted_proven_floor, "omitted_proven_floor");
                cert.push_back(res.price_tol, "omitted_tolerance");
                cert.push_back(res.max_flow_certified, "max_flow_certified");
                cert["certified_optimal"] = res.certified;
                certificate = cert;
            }

            return Rcpp::List::create(
                Rcpp::Named("bounds_feasible") = true,
                Rcpp::Named("reason") = "",
                Rcpp::Named("shape") = shape,
                Rcpp::Named("status") = res.status,
                Rcpp::Named("total_cost") = res.total_cost,
                Rcpp::Named("flow_sent") = static_cast<double>(res.flow_sent),
                Rcpp::Named("flow_required") = static_cast<double>(res.flow_required),
                Rcpp::Named("block") = block_to_r(prob, 0),
                Rcpp::Named("flow") = block_flow,
                Rcpp::Named("potential") = Rcpp::NumericVector(res.potential.begin(),
                                                               res.potential.end()),
                Rcpp::Named("layout") = design_layout_to_r(compiled),
                Rcpp::Named("certificate") = certificate,
                Rcpp::Named("search") = Rcpp::List::create(
                    Rcpp::Named("seed_width") = static_cast<double>(res.seed_width),
                    Rcpp::Named("candidate_edges") = static_cast<double>(res.candidate_edges),
                    Rcpp::Named("possible_edges") = static_cast<double>(res.possible_edges),
                    Rcpp::Named("edges_evaluated") = static_cast<double>(res.edges_evaluated),
                    Rcpp::Named("n_rounds") = static_cast<double>(res.rounds.size()),
                    Rcpp::Named("rounds") = implicit_rounds_to_r(res.rounds, false)));
        }, source);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return Rcpp::List();
}
