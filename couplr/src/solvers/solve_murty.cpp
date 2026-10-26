// src/solve_murty.cpp
// [[Rcpp::plugins(cpp17)]]
//
// Murty's k-best assignment. The partitioning is kbest_by_partition(), shared
// with the Lawler backend, so the two differ only in which single-assignment
// oracle they call and in the shape they report.
#include <Rcpp.h>
#include <string>
#include <utility>
#include <vector>
#include "../core/kbest_partition.h"
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Internal Murty: returns List(matches = IntegerMatrix(got, n), totals = NumericVector(got))
Rcpp::List solve_murty_impl(NumericMatrix cost, int k, bool maximize, std::string single_method /*= "jv"*/) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  if (n == 0 || k <= 0) {
    return List::create(_["matches"] = IntegerMatrix(0, n),
                        _["totals"]  = NumericVector(0));
  }
  if (n > m) LAP_ERROR("Infeasible: n > m");

  // The enumeration is ordered on a minimization key, so a maximizing run
  // negates the total and the k largest come back in the k smallest's place.
  KBestOracle solve_one = [&](NumericMatrix M) {
    List ans = run_base_solver_by_name(M, maximize, single_method);
    IntegerVector match_iv = ans["match"];
    double total = as<double>(ans["total_cost"]);
    return std::make_pair(std::vector<int>(match_iv.begin(), match_iv.end()),
                          maximize ? -total : total);
  };

  std::vector<KBestSolution> found = kbest_by_partition(cost, k, solve_one);

  const int got = static_cast<int>(found.size());
  IntegerMatrix Mout(got, n);
  NumericVector Tout(got);
  for (int r = 0; r < got; ++r) {
    for (int c = 0; c < n; ++c) Mout(r, c) = found[static_cast<size_t>(r)].match[static_cast<size_t>(c)];
    Tout[r] = maximize ? -found[static_cast<size_t>(r)].order_cost
                       : found[static_cast<size_t>(r)].order_cost;
  }

  return List::create(_["matches"] = Mout, _["totals"] = Tout);
}
