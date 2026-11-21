#include <Rcpp.h>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;

// Inputs: 0/1 integer flags and group codes 1..G.
// Counts follow row order; NA propagates within each group.
// Rows with an NA group retain their flag.
// [[Rcpp::export(rng = false)]]
IntegerVector plm_group_cumsum(IntegerVector flag, IntegerVector group, int G) {
  const R_xlen_t n = flag.size();
  if (group.size() != n) stop("internal group length mismatch");
  IntegerVector out(n);
  std::vector<int> count(G, 0);
  for (R_xlen_t i = 0; i < n; ++i) {
    const int g = group[i];
    if (g == NA_INTEGER) {
      out[i] = flag[i];
      continue;
    }
    if (g < 1 || g > G) stop("internal group code out of range");
    int &c = count[g - 1];
    if (c == NA_INTEGER || flag[i] == NA_INTEGER) c = NA_INTEGER;
    else c += flag[i];
    out[i] = c;
  }
  return out;
}
