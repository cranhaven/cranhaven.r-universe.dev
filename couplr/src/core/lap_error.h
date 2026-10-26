// src/core/lap_error.h
// Pure C++ error handling for LAP solvers - NO Rcpp dependencies
#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>

namespace lap {

// Custom exception for LAP solver errors
class LapException : public std::runtime_error {
public:
    explicit LapException(const std::string& msg)
        : std::runtime_error(msg) {}

    explicit LapException(const char* msg)
        : std::runtime_error(msg) {}
};

// Specific exception types for different error conditions
class InfeasibleException : public LapException {
public:
    explicit InfeasibleException(const std::string& msg = "No feasible solution exists")
        : LapException(msg) {}
};

class DimensionException : public LapException {
public:
    explicit DimensionException(const std::string& msg = "Invalid matrix dimensions")
        : LapException(msg) {}
};

class ConvergenceException : public LapException {
public:
    explicit ConvergenceException(const std::string& msg = "Algorithm did not converge")
        : LapException(msg) {}
};

// The shape every full-assignment solver here requires: a column available for
// each row. A taller problem is a shape the solver cannot take, which is why it
// raises DimensionException; InfeasibleException is what a solver raises when
// the shape is fine and the admissible edges still admit no perfect matching.
// cpp_tests asserts both halves of that split, one section each.
//
// The type does not reach R. The Rcpp boundary catches lap::LapException and
// re-raises e.what(), so a caller sees Rcpp::exception either way and the
// message is what carries which of the two conditions fired.
inline void require_rows_fit_cols(std::int64_t n_rows, std::int64_t n_cols) {
    if (n_rows > n_cols) {
        throw DimensionException(
            "solver requires nrow <= ncol; got " + std::to_string(n_rows) +
            " rows and " + std::to_string(n_cols) + " columns");
    }
}

}  // namespace lap

// Macro for throwing LAP exceptions in pure C++ code
// Use this instead of LAP_ERROR in solver implementations
#define LAP_THROW(msg) throw lap::LapException(msg)
#define LAP_THROW_INFEASIBLE(msg) throw lap::InfeasibleException(msg)
#define LAP_THROW_DIMENSION(msg) throw lap::DimensionException(msg)
#define LAP_THROW_CONVERGENCE(msg) throw lap::ConvergenceException(msg)
