#include <Rcpp.h>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;

static void plm_cumsum(const double *v, int m, int from, int step, double *r) {
  long double s = 0.0;
  for (int t = 0, k = from; t < m; ++t, k += step) {
    s += v[k];
    r[k] = (double)s;
  }
  if (!ISNAN((double)s)) return;
  bool nan = false, na = false;
  for (int t = 0, k = from; t < m; ++t, k += step) {
    if (!nan) nan = ISNAN(v[k]);
    if (nan && !na) na = R_IsNA(v[k]);
    if (nan) r[k] = na ? NA_REAL : R_NaN;
  }
}

// [[Rcpp::export(rng = false)]]
NumericMatrix plm_ehr_rsum(NumericMatrix M, IntegerVector lo, IntegerVector hi, int K, bool entry) {
  const int n = M.nrow(), p = M.ncol();
  if (lo.size() != n || hi.size() != n) stop("internal risk-set length mismatch");
  NumericMatrix out(K, p);
  std::vector<double> bh(K + 1), bl(entry ? K + 1 : 0), rh(K + 1), rl(entry ? K + 1 : 0);
  const int *l = lo.begin(), *h = hi.begin();
  for (int i = 0; i < n; ++i)
    if (l[i] != NA_INTEGER && h[i] != NA_INTEGER && l[i] <= h[i] && (l[i] < 1 || h[i] > K))
      stop("internal risk-set index out of range");
  for (int j = 0; j < p; ++j) {
    const double *x = M.begin() + (size_t)j * n;
    std::fill(bh.begin(), bh.end(), 0.0);
    std::fill(bl.begin(), bl.end(), 0.0);
    for (int i = 0; i < n; ++i) {
      if (l[i] == NA_INTEGER || h[i] == NA_INTEGER || l[i] > h[i]) continue;
      bh[h[i] - 1] += x[i];
      if (entry) bl[l[i] - 1] += x[i];
    }
    plm_cumsum(bh.data(), K + 1, K, -1, rh.data());
    double *o = out.begin() + (size_t)j * K;
    if (entry) {
      plm_cumsum(bl.data(), K + 1, K, -1, rl.data());
      for (int k = 0; k < K; ++k) o[k] = rh[k] - rl[k + 1];
    } else {
      for (int k = 0; k < K; ++k) o[k] = rh[k];
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix plm_ehr_wsum(NumericMatrix X, IntegerVector lo, IntegerVector hi) {
  const int K = X.nrow(), p = X.ncol(), n = lo.size();
  if (hi.size() != n) stop("internal risk-set length mismatch");
  NumericMatrix out(n, p);
  std::vector<double> col(K + 1), cs(K + 1);
  const int *l = lo.begin(), *h = hi.begin();
  for (int i = 0; i < n; ++i)
    if ((h[i] != NA_INTEGER && (h[i] < 0 || h[i] > K)) || (l[i] != NA_INTEGER && (l[i] < 1 || l[i] > K + 1)))
      stop("internal risk-set index out of range");
  for (int j = 0; j < p; ++j) {
    const double *x = X.begin() + (size_t)j * K;
    col[0] = 0.0;
    for (int k = 0; k < K; ++k) col[k + 1] = x[k];
    plm_cumsum(col.data(), K + 1, 0, 1, cs.data());
    double *o = out.begin() + (size_t)j * n;
    for (int i = 0; i < n; ++i) {
      const double a = h[i] == NA_INTEGER ? NA_REAL : cs[h[i]];
      const double b = l[i] == NA_INTEGER ? NA_REAL : cs[l[i] - 1];
      o[i] = a - b;
    }
  }
  return out;
}
