// src/solvers/greedy_matching_rcpp.cpp
// Rcpp wrappers for greedy matching - delegate to the pure C++ implementations.
// The strategies live in greedy_matching.cpp (lap::greedy_matching_sorted,
// _row_best, _pq, and the lap::greedy_matching dispatcher), the single source of
// truth exercised by cpp_tests, so the shipped path and the tested path are
// identical.

#include <Rcpp.h>
#include <string>
#include "greedy_matching.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Greedy accepts partial matchings, so report n_matched alongside the standard
// match / total_cost pair.
static Rcpp::List greedy_result(const lap::LapResult& result, NumericMatrix cost) {
    Rcpp::List out = lap_result_to_rcpp(result, cost);
    int n_matched = 0;
    for (int a : result.assignment) if (a >= 0) ++n_matched;
    out["n_matched"] = n_matched;
    return out;
}

List greedy_matching_impl(NumericMatrix cost_matrix, bool maximize,
                          std::string strategy) {
    try {
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost_matrix);
        Rcpp::List out = greedy_result(lap::greedy_matching(cm, maximize, strategy),
                                       cost_matrix);
        out["status"] = "optimal";  // greedy is "optimal" for its strategy
        out["method_used"] = "greedy_" + strategy;
        return out;
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return List();
}
