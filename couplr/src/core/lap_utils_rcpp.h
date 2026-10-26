// src/core/lap_utils_rcpp.h - Rcpp utility functions (R bindings)
#pragma once

#include <Rcpp.h>
#include <vector>
#include <string>
#include <utility>
#include "lap_types.h"
#include "lap_lazy_types.h"
#include "lap_callback_source.h"
#include <variant>

// Error macro using Rcpp::stop for proper C++ stack unwinding
// (Rf_error uses longjmp which skips destructors, causing memory leaks)
#define LAP_ERROR(...) Rcpp::stop(__VA_ARGS__)

// Constants
constexpr double BIG = 1e100;  // Used for forbidden edges
constexpr double TOL = 1e-12;  // Tolerance for zero comparisons

// String key for 1-based match vectors (e.g., "3,1,2")
std::string match_to_key(const std::vector<int>& match);

// Apply NA exclusions (row,col are 0-based)
Rcpp::NumericMatrix apply_exclusions(
    Rcpp::NumericMatrix base,
    const std::vector<std::pair<int,int>>& ex);

// Apply Lawler-style constraints
//  - force_cols: for rows 1..r, force row i to column force_cols[i-1] (1-based, 0 = no force)
//  - forbid (i_forbid, j_forbid): forbid one 1-based pair (0 = no forbid)
Rcpp::NumericMatrix apply_constraints(
    const Rcpp::NumericMatrix& M,
    const std::vector<int>& force_cols,
    int i_forbid,
    int j_forbid);

// Build CSR-style "allowed" structure from a mask of size n*m.
// mask is length n*m, row-major, with zero meaning allowed (nonzero = forbidden),
// matching prepare_cost_matrix (mask = 1 for NA/Inf) and this file's build_allowed.
// row_ptr size is n + 1; cols collects allowed column indices per row (0-based).
void build_allowed(
    const std::vector<int>& mask,
    int64_t n,
    int64_t m,
    std::vector<int>& row_ptr,
    std::vector<int>& cols);

// Ensure each row has at least one finite option (used by some solvers)
void ensure_each_row_has_option(const std::vector<int>& mask, int64_t n, int64_t m);

// Check if matrix is feasible (each row has at least one finite value)
// Returns true if feasible, false otherwise (does not throw)
bool is_feasible(const Rcpp::NumericMatrix& M);

// Check if a matching result is valid (no forbidden edges chosen)
// Returns true if all matched edges are finite, false otherwise
bool is_valid_matching(const Rcpp::NumericMatrix& cost,
                       const std::vector<int>& match);

// Check if a perfect matching exists using greedy with backtracking
// More thorough than is_feasible() - actually tries to find a matching
// Returns true if a valid matching exists, false otherwise
bool has_valid_matching(const Rcpp::NumericMatrix& M);

// Overload for Rcpp::IntegerVector (converts and calls vector version)
inline void ensure_each_row_has_option(const Rcpp::IntegerVector& mask, int64_t n, int64_t m) {
  std::vector<int> mask_vec(mask.begin(), mask.end());
  ensure_each_row_has_option(mask_vec, n, m);
}

// Standard result builders
Rcpp::List make_result(const std::vector<int>& match, double total);
Rcpp::List make_result(const Rcpp::IntegerVector& match, double total);

// Central cost computation: sum original_cost[i, match[i]] over all matched rows
// This is the SINGLE SOURCE OF TRUTH for what "cost" means across all solvers.
//
// Parameters:
//   original_cost: The original cost matrix as passed by the user (NumericMatrix)
//   assignment: 1-based column indices (IntegerVector), 0 or NA_INTEGER for unmatched
//
// Returns: Total cost = sum of original_cost[i, assignment[i]-1] for all matched i
//
// Invariants:
//   - Works for both minimize and maximize (no negation!)
//   - Ignores dummy columns (assignment[i] > ncol(original_cost))
//   - Ignores unmatched rows (assignment[i] == 0 or NA_INTEGER)
//   - Only sums over real, finite edges
double compute_total_cost(const Rcpp::NumericMatrix& original_cost,
                          const Rcpp::IntegerVector& assignment);

// Convert an Rcpp cost matrix to a pure lap::CostMatrix.
// mask = 1 (allowed) for finite entries, 0 (forbidden) for NA/Inf.
// Single source of truth for the adapter pattern that wraps every pure
// lap::solve_* implementation as an Rcpp *_impl entry point.
lap::CostMatrix rcpp_to_cost_matrix(const Rcpp::NumericMatrix& cost);

// Convert R feature matrices (column-major) into a lap::LazyCostMatrix
// (row-major internally). `left_mat`/`right_mat` are the already-scaled,
// already-weighted matching-variable matrices (see R's build_cost_matrix()).
// `metric` is one of the strings accepted by compute_distance_matrix()
// ("euclidean"/"l2", "manhattan"/"l1"/"cityblock", "squared_euclidean"/
// "sqeuclidean"/"sq", "chebyshev"/"chebychev"/"maximum"/"max",
// "mahalanobis"/"maha"). `inv_cov` is the precomputed p x p inverse
// covariance matrix (only read for Mahalanobis; pass R_NilValue otherwise --
// computing it is R's job, matching compute_distance_matrix()'s existing
// pooled-covariance logic, not reimplemented here). `calipers` is an R
// list with one list(threshold, left, right) per caliper, carrying the raw
// values of its variable for each left and right unit (R's
// lazy_cost_spec_calipers()).
lap::LazyCostMatrix rcpp_to_lazy_cost_matrix(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    const std::string& metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
    double max_distance,
    Rcpp::List calipers,
    bool maximize);

// The cost source a lazy specification describes: a LazyCostMatrix for a
// built-in metric named by a string, a CallbackCostSource for a user's R
// function. `inv_cov` of 0 x 0 is the same as NULL. Every binding that solves a
// specification builds its source here and visits it, so a solver templated on
// the source concept serves both.
using LazySource = std::variant<lap::LazyCostMatrix, lap::CallbackCostSource>;

LazySource rcpp_lazy_source(const Rcpp::NumericMatrix& left_mat,
                            const Rcpp::NumericMatrix& right_mat,
                            SEXP distance,
                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                            double max_distance,
                            Rcpp::List calipers,
                            bool maximize);

// The distance of specific matched pairs, evaluated by the same code the lazy
// solve evaluated them with. The R side needs these to report a pair's distance
// and must not recompute them from the formula: a second implementation of the
// same metric lands an ulp away, and a caliper set at a reported distance then
// excludes the pair it was read from. `rows`/`cols` are 1-based.
Rcpp::NumericVector lazy_pair_distances_impl(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    SEXP metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
    const Rcpp::IntegerVector& rows,
    const Rcpp::IntegerVector& cols);

// The sample standard deviation of the distance over every pair, read in one
// pass and held in two running sums, which is what a caliper stated in standard
// deviations of the distance needs when the pairs are never stored. NA when
// there are fewer than two pairs.
double lazy_distance_sd_impl(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    SEXP metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov);

// Convert a pure lap::LapResult to the standard Rcpp result list.
// Assignment is shifted 0-based -> 1-based (0 = unmatched) and the total is
// recomputed from original_cost via compute_total_cost for cross-solver
// consistency (works for both minimize and maximize).
Rcpp::List lap_result_to_rcpp(const lap::LapResult& result,
                              const Rcpp::NumericMatrix& original_cost);

// Transpose helper (inline for performance)
inline Rcpp::NumericMatrix transpose(const Rcpp::NumericMatrix& M) {
  const int r = M.nrow(), c = M.ncol();
  Rcpp::NumericMatrix T(c, r);
  for (int i = 0; i < r; ++i)
    for (int j = 0; j < c; ++j)
      T(j, i) = M(i, j);
  return T;
}

// Base solver router (used by Murty, Lawler)
Rcpp::List run_base_solver_by_name(
    const Rcpp::NumericMatrix& cost,
    bool maximize,
    const std::string& method);
