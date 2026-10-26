// src/solvers/solve_jv_duals_impl.h
// The Jonker-Volgenant duals solve over any prepared cost source, for a
// translation unit whose source the pure C++ entry points do not know.
#pragma once

#include "solve_jv_duals.h"
#include "jv_core_impl.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"

#include <cmath>
#include <utility>

namespace lap {

namespace jv_duals_detail {

// The body the dense and lazy entry points share. Preparation is what differs
// and stays with the caller: a dense matrix is negated and masked into a
// separate `work` matrix while the original stays behind to price the result,
// a lazy one is prepared at construction and is its own reporter.
//
// `report_negated` says whether `report` holds negated costs, so the total is
// always reported on the original scale. `solved_negated` says whether the core
// ran on negated costs, which is what the duals have to be flipped back from.
template <typename SolveSourceT, typename ReportSourceT>
DualResult jv_duals_from(const SolveSourceT& work, const ReportSourceT& report,
                         bool report_negated, bool solved_negated,
                         bool warm_start = true) {
    const int n = static_cast<int>(work.nrow);

    detail::JvCoreOpts opts;
    // LAPJV pre-stages: column reduction + ARR. Inert for a LazyCostMatrix
    // (see jv_core.cpp).
    opts.use_warm_start = warm_start;
    auto core = detail::jv_core(work, opts);

    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        const int j = core.assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!report.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = report.at(i, j);
        if (report_negated) c = -c;
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    if (solved_negated) {
        for (auto& x : core.u) x = -x;
        for (auto& x : core.v) x = -x;
    }

    DualResult result;
    result.solution = LapResult(std::move(core.assignment), total, "optimal");
    result.u = std::move(core.u);
    result.v = std::move(core.v);
    return result;
}

}  // namespace jv_duals_detail

// A source that is "prepared" at construction -- forbidden cells read as BIG
// through at(), negated under maximize and reporting it through is_negated() --
// is both the source the core runs on and the source the result is priced from.
template <typename Source>
DualResult solve_jv_duals_prepared(const Source& cost) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) {
        DualResult result;
        result.solution = LapResult({}, 0.0, "optimal");
        return result;
    }
    lap::require_rows_fit_cols(n, m);

    ensure_each_row_has_option(cost);

    const bool negated = cost.is_negated();
    return jv_duals_detail::jv_duals_from(cost, cost,
                                          /*report_negated=*/negated,
                                          /*solved_negated=*/negated);
}

}  // namespace lap
