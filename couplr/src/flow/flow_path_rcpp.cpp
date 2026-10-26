// src/flow/flow_path_rcpp.cpp
// R bindings for the design path in flow_path.h.
//
// A path crosses to R once for the whole sweep, for the reason one solve
// crosses once for the whole loop and then some: the objects a point hands the
// next point -- the compiled problem, the flow it ended on, the candidate set,
// the row structure -- have no R representation, and it is carrying them from
// one value to the next that makes the path cheaper than the solves it
// replaces. Driving the sweep from R would mean rebuilding all of them per
// point, which is the twenty independent solves the path is measured against.
//
// The knob is the distance cut. It is the one the roadmap's condition names and
// the one a lazy source can move without moving anything else: the coordinates,
// the metric, the per-variable calipers and the costs already reported all stay
// where they are, so every arc a point placed is still an arc at the next value
// and the flow carries over.
//
// The templates are pure minimization, so `maximize` is handled the way
// flow_implicit_rcpp.cpp handles it: the source is negated at construction and
// the quantities carrying the cost unit are negated back on the way out. A
// caliper value is a distance either way -- the cut is applied to the distance
// before the sign is -- so the sweep reads the same in both modes.

#include <Rcpp.h>

#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"
#include "../core/lap_rcpp_convert.h"
#include "../core/lap_utils_rcpp.h"
#include "flow_candidates.h"
#include "flow_compile.h"
#include "flow_implicit_rcpp.h"
#include "flow_oracle.h"
#include "flow_path.h"
#include "flow_problem.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace {

// One row per point, as columns. Everything here is one number or one string
// per point; the match vector, the certificate, the round record and the
// witness are lists beside it, because a data frame cell is not where a caller
// looks for a proof.
Rcpp::List points_to_r(const std::vector<lap::PathPoint>& points, bool maximize) {
    const R_xlen_t n = static_cast<R_xlen_t>(points.size());

    Rcpp::NumericVector   value(n), total_cost(n), seconds(n);
    Rcpp::NumericVector   n_matched(n), n_rounds(n), candidate_edges(n);
    Rcpp::NumericVector   block_arcs(n), pairs_added(n), edges_evaluated(n);
    Rcpp::LogicalVector   certified(n);
    Rcpp::CharacterVector status(n);

    for (R_xlen_t k = 0; k < n; ++k) {
        const lap::PathPoint& p = points[static_cast<std::size_t>(k)];
        value[k]           = p.value;
        status[k]          = p.status;
        n_matched[k]       = static_cast<double>(p.n_matched);
        total_cost[k]      = maximize ? -p.total_cost : p.total_cost;
        certified[k]       = p.certified;
        seconds[k]         = p.seconds;
        n_rounds[k]        = static_cast<double>(p.rounds.size());
        candidate_edges[k] = static_cast<double>(p.candidate_edges);
        block_arcs[k]      = static_cast<double>(p.block_arcs);
        pairs_added[k]     = static_cast<double>(p.pairs_added);
        edges_evaluated[k] = static_cast<double>(p.edges_evaluated);
    }

    return Rcpp::List::create(
        Rcpp::Named("value") = value,
        Rcpp::Named("status") = status,
        Rcpp::Named("n_matched") = n_matched,
        Rcpp::Named("total_cost") = total_cost,
        Rcpp::Named("certified") = certified,
        Rcpp::Named("seconds") = seconds,
        Rcpp::Named("n_rounds") = n_rounds,
        Rcpp::Named("candidate_edges") = candidate_edges,
        Rcpp::Named("block_arcs") = block_arcs,
        Rcpp::Named("pairs_added") = pairs_added,
        Rcpp::Named("edges_evaluated") = edges_evaluated);
}

Rcpp::List path_to_r(const lap::PathResult& res, int64_t nrow, bool maximize) {
    const R_xlen_t n = static_cast<R_xlen_t>(res.points.size());

    Rcpp::List match(n), certificate(n), rounds(n), witness(n);
    Rcpp::LogicalVector witness_certified(n);
    for (std::size_t k = 0; k < static_cast<std::size_t>(n); ++k) {
        const lap::PathPoint& p = res.points[k];
        match[static_cast<R_xlen_t>(k)] = implicit_match_to_r(p.match, nrow);
        rounds[static_cast<R_xlen_t>(k)] = implicit_rounds_to_r(p.rounds, maximize);

        // A defaulted report has no rows, and a real one always has at least
        // one, so this is the question "was a certificate assembled" without a
        // second field carrying the answer.
        if (p.certificate.n_rows > 0) {
            lap::CertificateReport rep = p.certificate;
            restore_certificate_sign(rep, maximize);
            certificate[static_cast<R_xlen_t>(k)] = certificate_report_to_list(rep);
        } else {
            certificate[static_cast<R_xlen_t>(k)] = R_NilValue;
        }

        witness_certified[static_cast<R_xlen_t>(k)] = p.witness_certified;
        if (p.status == "infeasible") {
            witness[static_cast<R_xlen_t>(k)] = hall_witness_to_list(p.witness);
        } else {
            witness[static_cast<R_xlen_t>(k)] = R_NilValue;
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("points") = points_to_r(res.points, maximize),
        Rcpp::Named("match") = match,
        Rcpp::Named("certificate") = certificate,
        Rcpp::Named("rounds") = rounds,
        Rcpp::Named("witness") = witness,
        Rcpp::Named("witness_certified") = witness_certified,
        Rcpp::Named("seed_width") = static_cast<double>(res.seed_width),
        Rcpp::Named("possible_edges") = static_cast<double>(res.possible_edges),
        Rcpp::Named("candidate_edges") = static_cast<double>(res.candidate_edges),
        Rcpp::Named("edges_evaluated") = static_cast<double>(res.edges_evaluated));
}

}  // namespace

Rcpp::List match_path_lazy_impl(Rcpp::NumericMatrix left_mat,
                                Rcpp::NumericMatrix right_mat,
                                SEXP distance,
                                Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                Rcpp::NumericVector values, Rcpp::List calipers,
                                bool maximize,
                                double keep_per_row, double width, double tol,
                                double max_rounds, bool certify) {
    try {
        if (values.size() == 0) {
            Rcpp::stop("design path: no values, so there is no path");
        }

        const std::vector<double> sweep(values.begin(), values.end());

        // The source is built at the first value and moved from there. It is
        // one object across the path, which is what lets the oracle the
        // compiled problem points at, and the tree the loop prices with, be
        // built once as well.
        LazySource source = rcpp_lazy_source(left_mat, right_mat, distance, inv_cov,
                                             sweep[0], calipers, maximize);

        lap::PathOptions opts;
        opts.implicit = implicit_options_from_r(keep_per_row, width, tol,
                                                max_rounds, certify);

        return std::visit([&](auto& cm) {
            using Source = std::decay_t<decltype(cm)>;
            lap::require_rows_fit_cols(static_cast<int>(cm.nrow),
                                       static_cast<int>(cm.ncol));

            lap::SourceOracle<Source> oracle(cm);
            lap::CompiledDesign design =
                lap::compile_one_to_one(oracle, std::vector<lap::CategoryConstraint>());
            lap::CandidateSet cand(cm.nrow, cm.ncol);

            const lap::PathResult res = lap::solve_path(
                cm, design.problem, cand, sweep,
                [](Source& src, lap::FlowProblem&, double v) {
                    src.set_max_distance(v);
                },
                opts);

            return path_to_r(res, cm.nrow, maximize);
        }, source);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
