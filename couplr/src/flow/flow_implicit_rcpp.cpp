// src/flow/flow_implicit_rcpp.cpp
// R bindings for the edge-generation loop in flow_implicit.h.
//
// The loop crosses to R once, not once per round. It owns the FlowProblem, the
// candidate set and the cost source, none of which has an R representation, so
// what a caller states is the problem -- a cost source and the knobs the search
// takes -- and what comes back is the answer, the certificate assembled for the
// complete implicit problem, and the record of what the search cost.
//
// Two entry points, one body. The loop is templated on the cost-source concept
// and both sources satisfy it: a lazy specification, which is the shape the
// memory saving is for, and a materialized matrix, which is what lets a caller
// hold the loop's answer against the dense solve of the same numbers.
//
// The templates are pure minimization, so `maximize` is handled the way
// lap_certify_rcpp.cpp handles it: the source is negated at construction, and
// the quantities carrying the cost unit -- the total, the duals, the per-round
// master cost, and the certificate's two objectives and gap -- are negated back
// on the way out.

#include <Rcpp.h>

#include "../core/lap_certify.h"
#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"
#include "../core/lap_rcpp_convert.h"
#include "../core/lap_types.h"
#include "../core/lap_utils.h"
#include "../core/lap_utils_rcpp.h"
#include "flow_candidates.h"
#include "flow_compile.h"
#include "flow_implicit.h"
#include "flow_implicit_rcpp.h"
#include "flow_oracle.h"
#include "flow_problem.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <memory>
#include <variant>
#include <cstdint>
#include <string>
#include <vector>

// The pieces the design path reports too. Declared in flow_implicit_rcpp.h.

int64_t implicit_knob_from_r(double v, const char* what) {
    if (ISNAN(v) || !R_finite(v)) {
        Rcpp::stop("edge generation: %s is not a finite number", what);
    }
    if (v != std::floor(v)) {
        Rcpp::stop("edge generation: %s is not a whole number", what);
    }
    return static_cast<int64_t>(v);
}

lap::ImplicitOptions implicit_options_from_r(double keep_per_row, double width,
                                             double tol, double max_rounds,
                                             bool certify) {
    lap::ImplicitOptions opts;
    opts.keep_per_row =
        static_cast<int>(implicit_knob_from_r(keep_per_row, "keep_per_row"));
    opts.width      = static_cast<int>(implicit_knob_from_r(width, "width"));
    opts.max_rounds = implicit_knob_from_r(max_rounds, "max_rounds");
    opts.tol        = tol;
    opts.certify    = certify;
    return opts;
}

Rcpp::IntegerVector implicit_match_to_r(const std::vector<int>& match, int64_t nrow) {
    Rcpp::IntegerVector out(static_cast<R_xlen_t>(nrow));
    for (R_xlen_t i = 0; i < out.size(); ++i) out[i] = 0;
    for (std::size_t i = 0; i < match.size(); ++i) {
        out[static_cast<R_xlen_t>(i)] = (match[i] >= 0) ? (match[i] + 1) : 0;
    }
    return out;
}

Rcpp::List implicit_rounds_to_r(const std::vector<lap::ImplicitRound>& rounds,
                                bool maximize) {
    const R_xlen_t n = static_cast<R_xlen_t>(rounds.size());

    Rcpp::NumericVector   round(n), candidate_pairs(n), block_arcs(n);
    Rcpp::NumericVector   flow_sent(n), flow_required(n), master_cost(n);
    Rcpp::NumericVector   master_seconds(n), min_reduced_cost(n), matched_slack(n);
    Rcpp::NumericVector   n_violators(n), n_evaluated(n), pairs_added(n);
    Rcpp::LogicalVector   exact_pricing(n);
    Rcpp::NumericVector   arcs_added(n), pricing_seconds(n);
    Rcpp::CharacterVector kind(n), master_status(n);

    for (R_xlen_t k = 0; k < n; ++k) {
        const lap::ImplicitRound& r = rounds[static_cast<std::size_t>(k)];
        round[k]            = static_cast<double>(r.round);
        kind[k]             = (r.kind == lap::ImplicitRound::Kind::priced) ? "priced"
                                                                          : "reseeded";
        master_status[k]    = r.master_status;
        candidate_pairs[k]  = static_cast<double>(r.candidate_pairs);
        block_arcs[k]       = static_cast<double>(r.block_arcs);
        flow_sent[k]        = static_cast<double>(r.flow_sent);
        flow_required[k]    = static_cast<double>(r.flow_required);
        master_cost[k]      = maximize ? -r.master_cost : r.master_cost;
        master_seconds[k]   = r.master_seconds;
        min_reduced_cost[k] = r.min_reduced_cost;
        n_violators[k]      = static_cast<double>(r.n_violators);
        exact_pricing[k]    = r.exact_pricing;
        n_evaluated[k]      = static_cast<double>(r.n_evaluated);
        pairs_added[k]      = static_cast<double>(r.pairs_added);
        arcs_added[k]       = static_cast<double>(r.arcs_added);
        pricing_seconds[k]  = r.pricing_seconds;
        matched_slack[k]    = r.matched_slack;
    }

    return Rcpp::List::create(
        Rcpp::Named("round") = round,
        Rcpp::Named("kind") = kind,
        Rcpp::Named("master_status") = master_status,
        Rcpp::Named("candidate_pairs") = candidate_pairs,
        Rcpp::Named("block_arcs") = block_arcs,
        Rcpp::Named("flow_sent") = flow_sent,
        Rcpp::Named("flow_required") = flow_required,
        Rcpp::Named("master_cost") = master_cost,
        Rcpp::Named("master_seconds") = master_seconds,
        Rcpp::Named("min_reduced_cost") = min_reduced_cost,
        Rcpp::Named("n_violators") = n_violators,
        Rcpp::Named("exact_pricing") = exact_pricing,
        Rcpp::Named("n_evaluated") = n_evaluated,
        Rcpp::Named("pairs_added") = pairs_added,
        Rcpp::Named("arcs_added") = arcs_added,
        Rcpp::Named("pricing_seconds") = pricing_seconds,
        Rcpp::Named("matched_slack") = matched_slack);
}

namespace {

Rcpp::NumericVector duals_to_r(const std::vector<double>& x, bool maximize) {
    Rcpp::NumericVector out(static_cast<R_xlen_t>(x.size()));
    for (std::size_t k = 0; k < x.size(); ++k) {
        out[static_cast<R_xlen_t>(k)] = maximize ? -x[k] : x[k];
    }
    return out;
}

Rcpp::List result_to_r(const lap::ImplicitResult& res, int64_t nrow, bool maximize) {
    const Rcpp::IntegerVector match = implicit_match_to_r(res.match, nrow);

    // A defaulted report has no rows, and a real one always has at least one,
    // so this is the question "was a certificate assembled" without a second
    // field carrying the answer.
    Rcpp::RObject certificate = R_NilValue;
    if (res.certificate.n_rows > 0) {
        lap::CertificateReport rep = res.certificate;
        restore_certificate_sign(rep, maximize);
        certificate = certificate_report_to_list(rep);
    }

    Rcpp::RObject witness = R_NilValue;
    if (res.status == "infeasible") {
        witness = hall_witness_to_list(res.witness);
    }

    return Rcpp::List::create(
        Rcpp::Named("match") = match,
        Rcpp::Named("total_cost") = maximize ? -res.total_cost : res.total_cost,
        Rcpp::Named("status") = res.status,
        Rcpp::Named("u") = duals_to_r(res.u, maximize),
        Rcpp::Named("v") = duals_to_r(res.v, maximize),
        Rcpp::Named("certificate") = certificate,
        Rcpp::Named("certified") = res.certified,
        Rcpp::Named("seed_width") = static_cast<double>(res.seed_width),
        Rcpp::Named("candidate_edges") = static_cast<double>(res.candidate_edges),
        Rcpp::Named("possible_edges") = static_cast<double>(res.possible_edges),
        Rcpp::Named("edges_evaluated") = static_cast<double>(res.edges_evaluated),
        Rcpp::Named("n_rounds") = static_cast<double>(res.rounds.size()),
        Rcpp::Named("rounds") = implicit_rounds_to_r(res.rounds, maximize),
        Rcpp::Named("witness") = witness,
        Rcpp::Named("witness_certified") = res.witness_certified);
}

// The body both entry points share: compile the one-to-one design over the
// source, start from an empty candidate set, and run the loop. The design is
// compiled here rather than in R because a compiled problem is a C++ object,
// and it is compiled over an oracle wrapping the same source the loop prices
// with, so the master and the pricer read one set of costs.
template <class Source>
Rcpp::List run_implicit(const Source& src, const lap::ImplicitOptions& opts,
                        bool maximize) {
    lap::require_rows_fit_cols(static_cast<int>(src.nrow), static_cast<int>(src.ncol));

    lap::SourceOracle<Source> oracle(src);
    lap::CompiledDesign design =
        lap::compile_one_to_one(oracle, std::vector<lap::CategoryConstraint>());
    lap::CandidateSet cand(src.nrow, src.ncol);

    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(src, design.problem, cand, opts);

    return result_to_r(res, src.nrow, maximize);
}

}  // namespace

Rcpp::List implicit_dense_impl(Rcpp::NumericMatrix cost, bool maximize,
                               double keep_per_row, double width, double tol,
                               double max_rounds, bool certify) {
    try {
        // The matrix a solver reads: NA, Inf and the forbidden sentinel are no
        // edge, and the costs are negated under maximize. The loop reads its
        // source through at()/allowed() like any other solver, so it is handed
        // the same prepared matrix.
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
        lap::forbid_sentinel_costs(cm);
        const lap::CostMatrix work = lap::prepare_for_solve(cm, maximize);

        return run_implicit(work, implicit_options_from_r(keep_per_row, width, tol,
                                                          max_rounds, certify),
                            maximize);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

// Matching with replacement over a lazy cost source. The rows do not compete, so
// the optimum over every pair is each row's own `per_row` cheapest admissible
// columns, and each row is one query to the structure the loop prices with: a
// tree over the columns where the metric carries a ball bound, a read of the row
// where it does not. The kept columns are the smallest under (cost, column),
// which is the order a sort of the row's costs leaves ties in.
namespace {

template <class Source>
Rcpp::List replace_body(const Source& src, int64_t k) {
    const int width = static_cast<int>(std::min<int64_t>(k, src.ncol));
    lap::RowSearch<Source> search(src);
    lap::detail::RowTopK keep(src.nrow, width);
    lap::RowScanWork work;
    lap::cheapest_per_row(src, search, keep, work);

    std::vector<int> rows, cols;
    std::vector<double> costs;
    keep.emit([&](int32_t i, int32_t j, double c) {
        rows.push_back(i + 1);
        cols.push_back(j + 1);
        costs.push_back(c);
    });
    // emit() hands a row's columns over in column order; the dense path lists a
    // row's partners cheapest first, ties by column.
    std::vector<std::size_t> order(rows.size());
    for (std::size_t t = 0; t < order.size(); ++t) order[t] = t;
    std::stable_sort(order.begin(), order.end(), [&](std::size_t a, std::size_t b) {
        if (rows[a] != rows[b]) return rows[a] < rows[b];
        if (costs[a] != costs[b]) return costs[a] < costs[b];
        return cols[a] < cols[b];
    });
    Rcpp::IntegerVector out_rows(static_cast<R_xlen_t>(order.size()));
    Rcpp::IntegerVector out_cols(static_cast<R_xlen_t>(order.size()));
    Rcpp::NumericVector out_cost(static_cast<R_xlen_t>(order.size()));
    for (std::size_t t = 0; t < order.size(); ++t) {
        out_rows[static_cast<R_xlen_t>(t)] = rows[order[t]];
        out_cols[static_cast<R_xlen_t>(t)] = cols[order[t]];
        out_cost[static_cast<R_xlen_t>(t)] = costs[order[t]];
    }
    return Rcpp::List::create(
        Rcpp::Named("rows") = out_rows,
        Rcpp::Named("cols") = out_cols,
        Rcpp::Named("distance") = out_cost,
        Rcpp::Named("edges_evaluated") = static_cast<double>(work.n_evaluated));
}

}  // namespace

Rcpp::List replace_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                             SEXP distance,
                             Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                             double max_distance, Rcpp::List calipers,
                             double per_row) {
    try {
        const LazySource source = rcpp_lazy_source(left_mat, right_mat, distance, inv_cov,
                                                   max_distance, calipers, false);
        const int64_t k = implicit_knob_from_r(per_row, "per_row");
        if (k < 1) Rcpp::stop("replacement matching: per_row must be at least 1");
        return std::visit([k](const auto& src) { return replace_body(src, k); }, source);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return Rcpp::List();
}

// ---------------------------------------------------------------------------
// A pricing session held across calls
// ---------------------------------------------------------------------------
//
// A network whose pair arcs R builds and solves itself -- the balance network
// of cardinality_match() -- still wants its omitted pairs priced without
// building them, and wants it once per solve of a search that runs many. The
// cost source, the tree over its columns and the candidate set are what those
// solves share, so they are held here behind an external pointer and each call
// asks one question of them.

namespace {

template <class Source>
struct PricingSessionOf {
    Source src;
    lap::RowSearch<Source> search;
    lap::CandidateSet cand;

    explicit PricingSessionOf(Source s)
        : src(std::move(s)), search(src), cand(src.nrow, src.ncol) {}
};

struct PricingSession {
    std::variant<std::unique_ptr<PricingSessionOf<lap::LazyCostMatrix>>,
                 std::unique_ptr<PricingSessionOf<lap::CallbackCostSource>>> held;
};

PricingSession& session_from(SEXP session) {
    Rcpp::XPtr<PricingSession> ptr(session);
    if (ptr.get() == nullptr) {
        Rcpp::stop("pricing session: the session has been released");
    }
    return *ptr;
}

Rcpp::List pairs_to_r(const std::vector<lap::CandidateSet::Pair>& pairs) {
    Rcpp::IntegerVector i(static_cast<R_xlen_t>(pairs.size()));
    Rcpp::IntegerVector j(static_cast<R_xlen_t>(pairs.size()));
    for (std::size_t t = 0; t < pairs.size(); ++t) {
        i[static_cast<R_xlen_t>(t)] = pairs[t].first + 1;
        j[static_cast<R_xlen_t>(t)] = pairs[t].second + 1;
    }
    return Rcpp::List::create(Rcpp::Named("i") = i, Rcpp::Named("j") = j);
}

}  // namespace

SEXP pricing_session_new_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              SEXP distance,
                              Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers) {
    try {
        LazySource source = rcpp_lazy_source(left_mat, right_mat, distance, inv_cov,
                                             max_distance, calipers, false);
        std::unique_ptr<PricingSession> session(new PricingSession());
        if (auto* lazy = std::get_if<lap::LazyCostMatrix>(&source)) {
            session->held = std::make_unique<PricingSessionOf<lap::LazyCostMatrix>>(
                std::move(*lazy));
        } else {
            session->held = std::make_unique<PricingSessionOf<lap::CallbackCostSource>>(
                std::move(std::get<lap::CallbackCostSource>(source)));
        }
        return Rcpp::XPtr<PricingSession>(session.release(), true);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return R_NilValue;
}

// Each row's `width` cheapest admissible columns, added to the session's
// candidate set and returned: the k-nearest seed.
Rcpp::List pricing_session_seed_impl(SEXP session, double width) {
    try {
        const int64_t w = implicit_knob_from_r(width, "width");
        if (w < 1) Rcpp::stop("pricing session: width must be at least 1");
        return std::visit([w](auto& s) {
            lap::detail::RowTopK keep(s->src.nrow,
                                      static_cast<int>(std::min<int64_t>(w, s->src.ncol)));
            lap::RowScanWork work;
            lap::cheapest_per_row(s->src, s->search, keep, work);
            std::vector<lap::CandidateSet::Pair> want;
            keep.emit([&](int32_t i, int32_t j, double) { want.emplace_back(i, j); });
            s->cand.note_evaluated(work.n_evaluated);
            Rcpp::List out = pairs_to_r(s->cand.add_pairs(want));
            out.push_back(static_cast<double>(work.n_evaluated), "n_evaluated");
            return out;
        }, session_from(session).held);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return Rcpp::List();
}

// The omitted pairs whose reduced cost is exactly negative, at most
// keep_per_row per row, added to the candidate set and returned. `u` and `v`
// hold the pair potentials as expansions, one row per unit, and a pair's arc
// costs its distance less `offset`, rounded, as the network holds it; see
// ExactPrices in flow_implicit.h. None returned means every omitted pair prices
// at or above zero, exactly.
Rcpp::List pricing_session_price_exact_impl(SEXP session, Rcpp::NumericMatrix u,
                                            Rcpp::NumericMatrix v, double offset,
                                            double keep_per_row) {
    try {
        const int keep = static_cast<int>(implicit_knob_from_r(keep_per_row, "keep_per_row"));
        if (!std::isfinite(offset)) Rcpp::stop("pricing session: the offset is not finite");
        return std::visit([&](auto& s) {
            if (u.nrow() != s->src.nrow || v.nrow() != s->src.ncol) {
                Rcpp::stop("pricing session: %d row potentials and %d column potentials "
                           "for a %d x %d source", static_cast<int>(u.nrow()),
                           static_cast<int>(v.nrow()), static_cast<int>(s->src.nrow),
                           static_cast<int>(s->src.ncol));
            }
            lap::implicit_detail::ExactPrices prices;
            prices.set(expansions_from_r(u, false), expansions_from_r(v, false), offset);
            const lap::BlockPricing priced = prices.price(s->src, s->search, s->cand, keep);
            Rcpp::List out = pairs_to_r(s->cand.add_pairs(lap::violator_pairs(priced.violators)));
            out.push_back(static_cast<double>(priced.n_violators), "n_violators");
            out.push_back(static_cast<double>(priced.n_evaluated), "n_evaluated");
            return out;
        }, session_from(session).held);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return Rcpp::List();
}

// The source's distance for pairs named by 1-based row and column, with the
// admissibility test applied: NA where a pair is not admissible.
Rcpp::NumericVector pricing_session_cost_impl(SEXP session, Rcpp::IntegerVector i,
                                              Rcpp::IntegerVector j) {
    if (i.size() != j.size()) Rcpp::stop("pricing session: i and j differ in length");
    return std::visit([&](auto& s) {
        Rcpp::NumericVector out(i.size());
        for (R_xlen_t t = 0; t < i.size(); ++t) {
            const int64_t a = static_cast<int64_t>(i[t]) - 1;
            const int64_t b = static_cast<int64_t>(j[t]) - 1;
            if (a < 0 || a >= s->src.nrow || b < 0 || b >= s->src.ncol) {
                Rcpp::stop("pricing session: pair index out of range");
            }
            double c = 0.0;
            out[t] = lap::cost_if_allowed(s->src, a, b, c) ? c : NA_REAL;
        }
        return out;
    }, session_from(session).held);
}

// The smallest and largest admissible distance over every pair and how many
// pairs are admissible, in one pass holding three numbers.
Rcpp::List pricing_session_range_impl(SEXP session) {
    return std::visit([](auto& s) {
        double lo = R_PosInf;
        double hi = R_NegInf;
        double count = 0.0;
        for (int64_t i = 0; i < s->src.nrow; ++i) {
            if ((i & 63) == 0) Rcpp::checkUserInterrupt();
            for (int64_t j = 0; j < s->src.ncol; ++j) {
                double c = 0.0;
                if (!lap::cost_if_allowed(s->src, i, j, c)) continue;
                if (c < lo) lo = c;
                if (c > hi) hi = c;
                count += 1.0;
            }
        }
        s->cand.note_evaluated(static_cast<int64_t>(count));
        return Rcpp::List::create(Rcpp::Named("min") = lo, Rcpp::Named("max") = hi,
                                  Rcpp::Named("n_admissible") = count);
    }, session_from(session).held);
}

double pricing_session_evaluated_impl(SEXP session) {
    return std::visit([](auto& s) { return static_cast<double>(s->cand.edges_evaluated()); },
                      session_from(session).held);
}

double implicit_seed_width_impl(double ncol) {
    return static_cast<double>(
        lap::implicit_seed_width(implicit_knob_from_r(ncol, "ncol")));
}

Rcpp::List implicit_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              SEXP distance,
                              Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              bool maximize,
                              double keep_per_row, double width, double tol,
                              double max_rounds, bool certify) {
    try {
        // The source bakes the calipers, the max_distance cut and the maximize
        // negation into at()/allowed() at construction, so it is already the
        // internal minimization.
        const LazySource source = rcpp_lazy_source(left_mat, right_mat, distance, inv_cov,
                                                   max_distance, calipers, maximize);
        const lap::ImplicitOptions opts =
            implicit_options_from_r(keep_per_row, width, tol, max_rounds, certify);
        return std::visit([&](const auto& cm) { return run_implicit(cm, opts, maximize); },
                          source);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
