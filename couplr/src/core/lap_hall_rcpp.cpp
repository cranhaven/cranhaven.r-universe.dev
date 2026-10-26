// src/core/lap_hall_rcpp.cpp
// Rcpp wrappers for the Hall-deficiency witness - calls the pure C++
// lap::hall_witness() template for the dense and the lazy cost source.

#include <Rcpp.h>
#include "lap_hall.h"
#include "lap_types.h"
#include "lap_lazy_types.h"
#include "lap_cost_view.h"
#include "lap_error.h"
#include "lap_rcpp_convert.h"
#include "lap_utils.h"
#include "lap_utils_rcpp.h"

// Compile-time smoke test: force the template body to be instantiated for
// every cost source the witness has to serve, so a change that only compiles
// for the dense case fails here rather than at the first lazy call site.
template lap::DeficiencySet lap::hall_witness<lap::CostMatrix>(
    const lap::CostMatrix&);
template lap::DeficiencySet lap::hall_witness<lap::LazyCostMatrix>(
    const lap::LazyCostMatrix&);
template lap::DeficiencySet lap::hall_witness<lap::PaddedCostView<lap::CostMatrix> >(
    const lap::PaddedCostView<lap::CostMatrix>&);

// Row and column counts are bounded by the dimensions of an R matrix, which
// are themselves R integers, so the int64_t counters fit an IntegerVector.
static Rcpp::IntegerVector indices_to_one_based(const std::vector<int64_t>& idx) {
    Rcpp::IntegerVector out(static_cast<R_xlen_t>(idx.size()));
    for (size_t t = 0; t < idx.size(); ++t) {
        out[static_cast<R_xlen_t>(t)] = static_cast<int>(idx[t]) + 1;
    }
    return out;
}

Rcpp::List hall_witness_to_list(const lap::DeficiencySet& witness) {
    Rcpp::IntegerVector matching(static_cast<R_xlen_t>(witness.matching.size()));
    for (size_t i = 0; i < witness.matching.size(); ++i) {
        matching[static_cast<R_xlen_t>(i)] =
            (witness.matching[i] >= 0) ? (witness.matching[i] + 1) : 0;
    }

    return Rcpp::List::create(
        Rcpp::Named("rows") = indices_to_one_based(witness.rows),
        Rcpp::Named("cols") = indices_to_one_based(witness.cols),
        Rcpp::Named("max_cardinality") = static_cast<int>(witness.max_cardinality),
        Rcpp::Named("deficiency") = static_cast<int>(witness.deficiency),
        Rcpp::Named("row_perfect") = witness.row_perfect,
        Rcpp::Named("verified") = witness.verified,
        Rcpp::Named("matching") = matching
    );
}

Rcpp::List hall_witness_dense_impl(Rcpp::NumericMatrix cost) {
    try {
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // rcpp_to_cost_matrix forbids NA and infinite entries; a cell already
        // carrying the sentinel is forbidden too, so the witness reads the
        // graph R reads.
        lap::forbid_sentinel_costs(cm);

        return hall_witness_to_list(lap::hall_witness(cm));

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

Rcpp::List hall_witness_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                  SEXP distance,
                                  Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                  double max_distance, Rcpp::List calipers) {
    try {
        // A metric other than Mahalanobis needs no inverse covariance;
        // lazy_cost_spec_inv_cov() returns NULL there, and a 0 x 0 matrix means
        // the same thing. rcpp_to_lazy_cost_matrix reads the argument only when
        // it is non-NULL and then demands p x p.
        Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov_arg = R_NilValue;
        if (inv_cov.isNotNull()) {
            Rcpp::NumericMatrix ic(inv_cov.get());
            if (ic.nrow() > 0 && ic.ncol() > 0) inv_cov_arg = inv_cov;
        }

        // maximize = false: LazyCostMatrix::allowed() applies the calipers and
        // the max_distance cut, and the flag only flips the sign that at()
        // reports. Admissibility, and so feasibility, is the same either way.
        const LazySource source = rcpp_lazy_source(
            left_mat, right_mat, distance, inv_cov_arg, max_distance, calipers, false);

        return std::visit([](const auto& cm) {
            return hall_witness_to_list(lap::hall_witness(cm));
        }, source);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
