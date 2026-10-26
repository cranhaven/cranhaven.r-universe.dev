// src/solvers/network_simplex/solve_network_simplex.cpp
// Pure C++ Network Simplex LAP solver - NO Rcpp dependencies

#include "solve_network_simplex.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils.h"
#include "ns_types.h"
#include "ns_graph.h"
#include "ns_init.h"
#include "ns_pivot.h"
#include <vector>
#include <limits>
#include <cmath>
#include <string>

namespace lap {

// Convert CostMatrix to flat array for network simplex functions
// Network simplex expects column-major layout (R convention)
static std::vector<double> convert_to_column_major(const CostMatrix& cost) {
    int n = static_cast<int>(cost.nrow);
    int m = static_cast<int>(cost.ncol);
    // n*m computed in int64_t: as plain `int` this overflows the vector size
    // and every flat index below past a ~46,341-square matrix.
    const int64_t cell_count = static_cast<int64_t>(n) * static_cast<int64_t>(m);
    std::vector<double> result(static_cast<size_t>(cell_count));

    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            // cost.at(i,j) is row-major access
            // result[i + j*n] is column-major storage
            result[static_cast<size_t>(i + j * n)] = cost.at(i, j);
        }
    }

    return result;
}

LapResult solve_network_simplex(const CostMatrix& cost, bool maximize) {
    const int n_rows = static_cast<int>(cost.nrow);
    const int n_cols = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n_rows == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n_rows, n_cols);

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n_rows, n_cols);

    // Convert to column-major for network simplex internal functions
    std::vector<double> cost_data = convert_to_column_major(work);

    // Build network
    couplr::ns::NSState state;
    couplr::ns::build_assignment_network(state, cost_data.data(), n_rows, n_cols);

    // Initialize the spanning tree from a maximum-cardinality matching on the
    // allowed edges. ensure_each_row_has_option() above only rules out a row
    // with no edges at all; Hall's condition is decided here.
    const int matched_rows = couplr::ns::initialize_spanning_tree_greedy(state);
    if (matched_rows < n_rows) {
        LAP_THROW_INFEASIBLE(
            "Infeasible: no assignment covers every row (a maximum matching on "
            "the allowed edges covers " + std::to_string(matched_rows) + " of " +
            std::to_string(n_rows) + " rows)");
    }

    // Compute initial potentials
    couplr::ns::compute_potentials(state);

    // Main simplex loop. It ends either on the optimality condition, when
    // pricing scans every arc without finding a negative reduced cost, or on
    // the pivot cap. Which one happened is what the status is computed from.
    const long long max_pivots = couplr::ns::pivot_limit(state.num_nodes);
    couplr::ns::Termination termination = couplr::ns::Termination::PivotLimit;

    for (long long iter = 0; iter < max_pivots; ++iter) {
        int entering = couplr::ns::find_entering_arc(state);

        if (entering == couplr::ns::NO_ARC) {
            termination = couplr::ns::Termination::Optimality;
            break;
        }

        // Find leaving arc and compute delta
        couplr::ns::PivotInfo info = couplr::ns::find_leaving_arc(state, entering);

        // Perform pivot
        couplr::ns::do_pivot(state, info);
    }

    // Extract assignment from solution
    couplr::ns::NSResult ns_result =
        couplr::ns::extract_assignment(state, termination);

    // Convert NSResult to LapResult
    std::vector<int> assignment = std::move(ns_result.assignment);

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n_rows; ++i) {
        int j = assignment[i];
        if (j < 0) {
            // A matching covering every row was found before the pivot loop, so
            // this is a basis that stopped carrying that flow, not an input with
            // no assignment.
            LAP_THROW_CONVERGENCE(
                "network_simplex: final basis leaves row " + std::to_string(i + 1) +
                " unmatched after " + std::to_string(ns_result.pivot_count) +
                " pivots");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, ns_result.status);
}

}  // namespace lap
