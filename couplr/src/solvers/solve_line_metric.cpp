// src/solvers/solve_line_metric.cpp
// Pure C++ 1D assignment solver (line metric) - NO Rcpp dependencies

#include "solve_line_metric.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>

namespace lap {

namespace {

// Cost function types
enum class CostType { L1, L2 };

// Parse cost type string
inline CostType parse_cost_type(const std::string& cost_str) {
    std::string lower_cost = cost_str;
    std::transform(lower_cost.begin(), lower_cost.end(), lower_cost.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    if (lower_cost == "l1" || lower_cost == "abs" || lower_cost == "manhattan") {
        return CostType::L1;
    } else if (lower_cost == "l2" || lower_cost == "sq" ||
               lower_cost == "squared" || lower_cost == "quadratic") {
        return CostType::L2;
    }
    LAP_THROW("Unknown cost type: '" + cost_str + "'. Use 'L1' or 'L2'.");
}

// Compute cost based on type
inline double compute_cost(double diff, CostType cost_type) {
    const double abs_diff = std::abs(diff);
    return (cost_type == CostType::L1) ? abs_diff : (abs_diff * abs_diff);
}

// Sort indices by values (ascending)
std::vector<int> argsort(const std::vector<double>& vec) {
    std::vector<int> indices(vec.size());
    for (size_t i = 0; i < vec.size(); ++i) {
        indices[i] = static_cast<int>(i);
    }
    std::sort(indices.begin(), indices.end(),
              [&vec](int i, int j) { return vec[i] < vec[j]; });
    return indices;
}

// Sort indices by values (descending)
std::vector<int> argsort_desc(const std::vector<double>& vec) {
    std::vector<int> indices(vec.size());
    for (size_t i = 0; i < vec.size(); ++i) {
        indices[i] = static_cast<int>(i);
    }
    std::sort(indices.begin(), indices.end(),
              [&vec](int i, int j) { return vec[i] > vec[j]; });
    return indices;
}

}  // anonymous namespace

LapResult solve_line_metric(const std::vector<double>& x,
                            const std::vector<double>& y,
                            const std::string& cost_str,
                            bool maximize) {
    const int n = static_cast<int>(x.size());
    const int m = static_cast<int>(y.size());

    // Validate dimensions
    if (n <= 0) {
        LAP_THROW_DIMENSION("x must have at least one element");
    }
    if (m <= 0) {
        LAP_THROW_DIMENSION("y must have at least one element");
    }
    if (n > m) {
        LAP_THROW_DIMENSION("n must be <= m (number of x values <= number of y values)");
    }

    // Parse cost type
    CostType cost_type = parse_cost_type(cost_str);

    // Get sorted indices. The min-cost matching on a line is monotone
    // (non-crossing), so both x and y are sorted ascending. The max-cost
    // matching is anti-monotone, so y is sorted descending; the same
    // order-preserving DP over the descending y ordering then explores exactly
    // the anti-monotone matchings.
    std::vector<int> xi = argsort(x);
    std::vector<int> yi = maximize ? argsort_desc(y) : argsort(y);

    // Create sorted arrays
    std::vector<double> xs(n), ys(m);
    for (int i = 0; i < n; ++i) xs[i] = x[xi[i]];
    for (int j = 0; j < m; ++j) ys[j] = y[yi[j]];

    // Handle square case (n == m) - direct order matching in the chosen ordering
    if (n == m) {
        double total = 0.0;
        for (int i = 0; i < n; ++i) {
            total += compute_cost(xs[i] - ys[i], cost_type);
        }

        // Build match vector (0-based, in original order)
        std::vector<int> assignment(n);
        for (int k = 0; k < n; ++k) {
            int i_orig = xi[k];
            int j_orig = yi[k];
            assignment[i_orig] = j_orig;  // 0-based
        }

        return LapResult(std::move(assignment), total, "optimal");
    }

    // Rectangular case (n < m) - dynamic programming.
    // dp[i][j] = best (min or max) cost to match first i elements of x to a
    // subsequence of the first j elements of the (ordered) y array.
    const double WORST = maximize ? -std::numeric_limits<double>::infinity()
                                  :  std::numeric_limits<double>::infinity();
    std::vector<std::vector<double>> dp(n + 1, std::vector<double>(m + 1, WORST));

    // Base case: 0 elements of x can be matched to any number of y elements with 0 cost
    for (int j = 0; j <= m; ++j) {
        dp[0][j] = 0.0;
    }

    // Fill DP table
    for (int i = 1; i <= n; ++i) {
        for (int j = i; j <= m; ++j) {  // j must be >= i (we need at least i elements to match i)
            // Option 1: skip y[j-1] (don't match it to anything)
            dp[i][j] = dp[i][j - 1];

            // Option 2: match x[i-1] to y[j-1]
            double match_cost = compute_cost(xs[i - 1] - ys[j - 1], cost_type);
            double alt = dp[i - 1][j - 1] + match_cost;

            if (maximize ? (alt > dp[i][j]) : (alt < dp[i][j])) {
                dp[i][j] = alt;
            }
        }
    }

    double total = dp[n][m];

    // Backtrack to find the matching
    std::vector<int> match_cols_sorted(n, -1);
    int i = n, j = m;

    while (i > 0) {
        // Check which choice was made
        if (j > 0 && dp[i][j] == dp[i][j - 1]) {
            // We skipped y[j-1]
            --j;
        } else {
            // We matched x[i-1] to y[j-1]
            match_cols_sorted[i - 1] = j - 1;
            --i;
            --j;
        }
    }

    // Convert from sorted indices back to original indices (0-based)
    std::vector<int> assignment(n);
    for (int i_sorted = 0; i_sorted < n; ++i_sorted) {
        int i_orig = xi[i_sorted];
        int j_sorted = match_cols_sorted[i_sorted];
        int j_orig = yi[j_sorted];
        assignment[i_orig] = j_orig;  // 0-based
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
