// src/core/lap_types.h
// Pure C++ types for LAP solvers - NO Rcpp dependencies
#pragma once

#include <vector>
#include <string>
#include <limits>
#include <cmath>
#include <cstdint>
#include <cstring>

namespace lap {

namespace detail {

// gamma_k in the standard floating-point sense: the factor a quantity
// accumulated over k rounded operations can be wrong by, relative to the sum
// of the magnitudes. Saturates rather than going negative when k*eps reaches
// one. Lives here because the ball tree charges it for the cost source's
// quadratic form and the certificate charges it for its compensated sums,
// and the two headers meet in the same translation unit.
inline double gamma_of(int64_t k) {
    const double e = 0.5 * std::numeric_limits<double>::epsilon();
    const double d = static_cast<double>(k) * e;
    if (!(d < 1.0)) return std::numeric_limits<double>::infinity();
    return d / (1.0 - d);
}

// std::nextafter(x, +inf) and std::nextafter(x, -inf), read off the
// representation instead of through the library call, which the compiler
// cannot inline. Same value for every double, NaN and the infinities included:
// a finite nonzero x moves one step of its own bits away from or towards zero,
// and a zero steps to the smallest denormal of the sign the direction gives.
// Every bound a pricing descent rounds outward passes through one of these at
// each node it reads.
inline double next_up(double x) {
    if (!(x < std::numeric_limits<double>::infinity())) return x;
    if (x == 0.0) return std::numeric_limits<double>::denorm_min();
    std::uint64_t bits;
    std::memcpy(&bits, &x, sizeof bits);
    bits = x > 0.0 ? bits + 1u : bits - 1u;
    std::memcpy(&x, &bits, sizeof bits);
    return x;
}

inline double next_down(double x) {
    if (!(x > -std::numeric_limits<double>::infinity())) return x;
    if (x == 0.0) return -std::numeric_limits<double>::denorm_min();
    std::uint64_t bits;
    std::memcpy(&bits, &x, sizeof bits);
    bits = x > 0.0 ? bits - 1u : bits + 1u;
    std::memcpy(&x, &bits, sizeof bits);
    return x;
}

}  // namespace detail


// Constants
constexpr double BIG = 1e100;   // Used for forbidden edges
constexpr double TOL = 1e-12;   // Tolerance for zero comparisons

// Flat row-major index, computed in 64-bit to avoid overflow past
// nrow*ncol > INT_MAX (~46,341 square) which plain `int` arithmetic
// silently wraps (undefined behavior) on matrices the package's own
// vignettes already reach (n=50,000).
inline int64_t flat_index(int64_t i, int64_t j, int64_t ncol) {
    return i * ncol + j;
}

// Cost matrix: row-major flat vector + dimensions
struct CostMatrix {
    std::vector<double> data;   // row-major, size = nrow * ncol
    std::vector<int> mask;      // 1=allowed, 0=forbidden, size = nrow * ncol
    int64_t nrow = 0;
    int64_t ncol = 0;

    CostMatrix() = default;

    CostMatrix(int64_t rows, int64_t cols)
        : data(static_cast<size_t>(rows * cols), 0.0)
        , mask(static_cast<size_t>(rows * cols), 1)
        , nrow(rows)
        , ncol(cols) {}

    CostMatrix(const std::vector<std::vector<double>>& mat) {
        if (mat.empty()) {
            nrow = ncol = 0;
            return;
        }
        nrow = static_cast<int64_t>(mat.size());
        ncol = static_cast<int64_t>(mat[0].size());
        data.resize(static_cast<size_t>(nrow * ncol));
        mask.resize(static_cast<size_t>(nrow * ncol), 1);
        for (int64_t i = 0; i < nrow; ++i) {
            for (int64_t j = 0; j < ncol; ++j) {
                double v = mat[static_cast<size_t>(i)][static_cast<size_t>(j)];
                data[static_cast<size_t>(flat_index(i, j, ncol))] = v;
                mask[static_cast<size_t>(flat_index(i, j, ncol))] = std::isfinite(v) ? 1 : 0;
            }
        }
    }

    // Element access
    double& at(int64_t i, int64_t j) { return data[static_cast<size_t>(flat_index(i, j, ncol))]; }
    double at(int64_t i, int64_t j) const { return data[static_cast<size_t>(flat_index(i, j, ncol))]; }

    // Check if edge is allowed (finite cost)
    bool allowed(int64_t i, int64_t j) const { return mask[static_cast<size_t>(flat_index(i, j, ncol))] != 0; }

    // Set edge as forbidden
    void forbid(int64_t i, int64_t j) {
        mask[static_cast<size_t>(flat_index(i, j, ncol))] = 0;
        data[static_cast<size_t>(flat_index(i, j, ncol))] = BIG;
    }

    // Check if empty
    bool empty() const { return nrow == 0 || ncol == 0; }
};

// Result of a single LAP solution
struct LapResult {
    std::vector<int> assignment;  // 0-based column indices, -1 = unmatched
    double total_cost = 0.0;
    std::string status = "optimal";

    LapResult() = default;

    LapResult(std::vector<int> assign, double cost, std::string stat = "optimal")
        : assignment(std::move(assign))
        , total_cost(cost)
        , status(std::move(stat)) {}

    // Check if row i is matched
    bool is_matched(int i) const {
        return i >= 0 && i < static_cast<int>(assignment.size()) &&
               assignment[i] >= 0;
    }

    // Number of matched rows
    int n_matched() const {
        int count = 0;
        for (int j : assignment) {
            if (j >= 0) ++count;
        }
        return count;
    }
};

// Result for k-best solutions
struct KBestResult {
    std::vector<LapResult> solutions;
    std::string status = "optimal";

    bool empty() const { return solutions.empty(); }
    size_t size() const { return solutions.size(); }
};

// Result with dual variables (for sensitivity analysis)
struct DualResult {
    LapResult solution;
    std::vector<double> u;  // row potentials
    std::vector<double> v;  // column potentials
};

}  // namespace lap
