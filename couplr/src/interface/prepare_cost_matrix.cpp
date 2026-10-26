#include <Rcpp.h>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"
using namespace Rcpp;

// Internal implementation only (no Rcpp export)
Rcpp::List prepare_cost_matrix_impl(Rcpp::NumericMatrix cost, bool maximize) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  const std::size_t nm = static_cast<std::size_t>(n) * static_cast<std::size_t>(m);
  std::vector<double> rowmaj(nm);
  std::vector<int>    mask(nm, 0);

  double cmax = R_NegInf;

  // Convert column-major (R) to row-major buffer; NA or non-finite (+/-Inf)
  // -> +Inf and mask=1. Inf inputs must mark forbidden edges too, otherwise
  // cmax becomes Inf and the maximize flip below is silently skipped.
  for (int j = 0; j < m; ++j) {
    for (int i = 0; i < n; ++i) {
      const std::size_t idx = static_cast<std::size_t>(i) * m + j;  // row-major index
      double x = cost(i, j);
      if (Rcpp::NumericVector::is_na(x) || !R_finite(x)) {
        rowmaj[idx] = R_PosInf;
        mask[idx]   = 1;
      } else {
        rowmaj[idx] = x;
        if (x > cmax) cmax = x;
      }
    }
  }

  // If maximize, flip finite costs: c' = cmax - c
  if (maximize && R_finite(cmax)) {
    for (std::size_t k = 0; k < nm; ++k) {
      if (!R_finite(rowmaj[k])) continue; // keep +Inf for forbidden
      rowmaj[k] = cmax - rowmaj[k];
    }
  }

  return Rcpp::List::create(
    Rcpp::_["cost"] = Rcpp::NumericVector(rowmaj.begin(), rowmaj.end()),
    Rcpp::_["mask"] = Rcpp::IntegerVector(mask.begin(), mask.end()),
    Rcpp::_["n"]    = n,
    Rcpp::_["m"]    = m,
    Rcpp::_["cmax"] = cmax
  );
}
