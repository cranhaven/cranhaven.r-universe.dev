// src/solvers/solve_bruteforce.cpp
// Pure C++ brute-force LAP solver - NO Rcpp dependencies

#include "solve_bruteforce.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <algorithm>
#include <limits>
#include <numeric>
#include <cmath>

namespace lap {

LapResult solve_bruteforce(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Size limit for brute force
    if (n > 8) {
        LAP_THROW_DIMENSION("Brute-force solver only supports n <= 8 (too computationally expensive)");
    }

    // Bound the total enumeration work. The solver enumerates every injective
    // assignment: m! / (m - n)! = C(m, n) * n! of them. This grows without
    // bound in m even for small n (e.g. 8 x 1000), so cap it explicitly.
    {
        double injective_count = 1.0;
        for (int k = 0; k < n; ++k) {
            injective_count *= static_cast<double>(m - k);
        }
        if (injective_count > 1e8) {
            LAP_THROW_DIMENSION("Brute-force solver: problem too large "
                                "(injective-assignment enumeration exceeds limit); "
                                "use a different method");
        }
    }

    // Check feasibility - each row must have at least one allowed edge
    ensure_each_row_has_option(cost.mask, n, m);

    // Generate all column indices
    std::vector<int> cols(m);
    std::iota(cols.begin(), cols.end(), 0);

    // Track best solution
    double best_total = maximize ? -std::numeric_limits<double>::infinity()
                                 : std::numeric_limits<double>::infinity();
    std::vector<int> best_assignment(n, -1);
    bool found_feasible = false;

    // Generate all combinations of n columns from m
    // Using binary representation: 1 = include column, 0 = exclude
    std::vector<int> choose(m, 0);
    std::fill(choose.begin(), choose.begin() + n, 1);

    // Helper lambda to check if a selection is feasible and compute its cost
    auto evaluate_assignment = [&](const std::vector<int>& assignment) -> std::pair<bool, double> {
        double total = 0.0;
        for (int i = 0; i < n; ++i) {
            int j = assignment[i];

            // Check if edge is allowed
            if (!cost.allowed(i, j)) {
                return {false, 0.0};
            }

            double c = cost.at(i, j);

            // Check if cost is finite
            if (!std::isfinite(c)) {
                return {false, 0.0};
            }

            total += c;
        }
        return {true, total};
    };

    // Iterate through all combinations
    do {
        // Extract selected columns
        std::vector<int> selected;
        selected.reserve(n);
        for (int j = 0; j < m; ++j) {
            if (choose[j]) {
                selected.push_back(j);
            }
        }

        // Try all permutations of the selected columns
        std::sort(selected.begin(), selected.end());
        do {
            // Evaluate this assignment
            auto result = evaluate_assignment(selected);

            if (!result.first) {
                continue;  // Infeasible assignment
            }

            double total = result.second;
            found_feasible = true;

            // Check if this is better than current best
            bool is_better = maximize ? (total > best_total) : (total < best_total);

            if (is_better) {
                best_total = total;
                best_assignment = selected;
            }

        } while (std::next_permutation(selected.begin(), selected.end()));

    } while (std::prev_permutation(choose.begin(), choose.end()));

    // Check if we found any feasible solution
    if (!found_feasible) {
        LAP_THROW_INFEASIBLE("No feasible matching exists given forbidden edges");
    }

    // Return 0-based assignment with total cost from original matrix
    return LapResult(std::move(best_assignment), best_total, "optimal");
}

}  // namespace lap
