#include <Rcpp.h>
#include <cfloat>
#include <climits>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;

// rows: 1-based input indices in (id, time) order.
// Returns NULL when the caller must use the R recursion.
// NULL conditions: integer time overflow or noncontiguous subject rows in reverse recursion.

static inline bool li_time_diff(SEXP time, int a, int b, double &out) {
  if (TYPEOF(time) == REALSXP) {
    const double *t = REAL(time);
    out = t[a] - t[b];
    return true;
  }
  const int *t = INTEGER(time);
  if (t[a] == NA_INTEGER || t[b] == NA_INTEGER) {
    out = NA_REAL;
    return true;
  }
  const long long d = (long long)t[a] - (long long)t[b];
  if (d > INT_MAX || d < -INT_MAX) return false;
  out = (double)d;
  return true;
}

template <typename T>
static SEXP li_forward(IntegerVector rows, IntegerVector si, SEXP time, NumericVector y,
                       LogicalVector observed, NumericMatrix x, LogicalVector finite_x,
                       NumericVector eta) {
  const int n = rows.size(), N = y.size(), q = x.ncol(), p = eta.size();
  if (p != q + 2 || x.nrow() != N) stop("non-conformable linear-increment design");
  if (TYPEOF(time) != REALSXP && TYPEOF(time) != INTSXP) stop("numeric time required");
  NumericVector yo = clone(y);
  LogicalVector rec(N);
  double *yv = yo.begin();
  const double *e = eta.begin();
  int k = 0;
  while (k < n) {
    const int s = si[rows[k] - 1];
    bool anchored = false;
    int prev = -1;
    double yp = 0.0;
    for (; k < n && si[rows[k] - 1] == s; ++k) {
      const int r = rows[k] - 1;
      if (observed[r]) {
        anchored = true;
        prev = r;
        yp = yv[r];
        continue;
      }
      if (!anchored) continue;
      double gap;
      if (!li_time_diff(time, r, prev, gap)) return R_NilValue;
      if (!(R_FINITE(gap) && gap > 0) || !finite_x[prev]) {
        anchored = false;
        continue;
      }
      T acc = 0.0;
      acc += 1.0 * e[0];
      for (int j = 0; j < q; ++j) acc += x(prev, j) * e[j + 1];
      acc += yp * e[p - 1];
      const double yn = yp + gap * (double)acc;
      yv[r] = yn;
      rec[r] = TRUE;
      yp = yn;
      prev = r;
    }
  }
  return List::create(_["y"] = yo, _["rec"] = rec);
}

// [[Rcpp::export(rng = false)]]
SEXP li_forward_cpp(IntegerVector rows, IntegerVector si, SEXP time, NumericVector y,
                    LogicalVector observed, NumericMatrix x, LogicalVector finite_x,
                    NumericVector eta, bool long_double) {
  return long_double ? li_forward<long double>(rows, si, time, y, observed, x, finite_x, eta)
                     : li_forward<double>(rows, si, time, y, observed, x, finite_x, eta);
}

// [[Rcpp::export(rng = false)]]
SEXP li_reverse_cpp(IntegerVector rows, IntegerVector si, int n_subjects, SEXP time,
                    NumericVector y, LogicalVector observed, NumericMatrix C,
                    NumericVector eta) {
  const int n = rows.size(), N = y.size(), q = C.ncol(), p = eta.size();
  if (p != q + 2 || C.nrow() != N) stop("non-conformable linear-increment design");
  if (TYPEOF(time) != REALSXP && TYPEOF(time) != INTSXP) stop("numeric time required");
  std::vector<int> start((size_t)n_subjects + 1, -1), first((size_t)n_subjects + 1, -1);
  int k = 0;
  while (k < n) {
    const int s = si[rows[k] - 1];
    if (s < 1 || s > n_subjects || start[s] >= 0) return R_NilValue;
    start[s] = k;
    for (; k < n && si[rows[k] - 1] == s; ++k)
      if (first[s] < 0 && observed[rows[k] - 1]) first[s] = k;
  }
  NumericVector yo = clone(y);
  LogicalVector rec(N);
  double *yv = yo.begin();
  std::vector<int> top, len, prev;
  std::vector<double> yp;
  for (int s = 1; s <= n_subjects; ++s) {
    if (first[s] <= start[s]) continue;
    top.push_back(first[s]);
    len.push_back(first[s] - start[s]);
    prev.push_back(rows[first[s]] - 1);
    yp.push_back(yv[rows[first[s]] - 1]);
  }
  const int nr = top.size();
  std::vector<char> alive(nr, 1);
  std::vector<int> bg, br;
  std::vector<double> bdt;
  Shield<SEXP> call(Rf_lang3(Rf_install("%*%"), R_NilValue, eta));
  for (int depth = 1;; ++depth) {
    bool any = false;
    bg.clear(); br.clear(); bdt.clear();
    for (int i = 0; i < nr; ++i) {
      if (len[i] < depth) continue;
      any = true;
      if (!alive[i]) continue;
      const int r = rows[top[i] - depth] - 1;
      double dt;
      if (!li_time_diff(time, prev[i], r, dt)) return R_NilValue;
      if (!(R_FINITE(dt) && dt > 0)) {
        alive[i] = 0;
        continue;
      }
      bg.push_back(i); br.push_back(r); bdt.push_back(dt);
    }
    if (!any) break;
    const int m = bg.size();
    if (!m) continue;
    Shield<SEXP> w(Rf_allocMatrix(REALSXP, m, p));
    double *wv = REAL(w);
    for (int i = 0; i < m; ++i) {
      wv[i] = 1.0;
      for (int j = 0; j < q; ++j) wv[(R_xlen_t)(j + 1) * m + i] = C(br[i], j);
      wv[(R_xlen_t)(p - 1) * m + i] = yp[bg[i]];
    }
    SETCADR(call, w);
    Shield<SEXP> prod(Rcpp_fast_eval(call, R_BaseEnv));
    const double *pv = REAL(prod);
    for (int i = 0; i < m; ++i) {
      const int g = bg[i], r = br[i];
      const double yn = yp[g] + bdt[i] * pv[i];
      if (!R_FINITE(yn)) {
        alive[g] = 0;
        continue;
      }
      yv[r] = yn;
      rec[r] = TRUE;
      prev[g] = r;
      yp[g] = yn;
    }
  }
  return List::create(_["y"] = yo, _["rec"] = rec);
}

// Returns NULL when sum(m) overflows R's integer sum.
template <typename T>
static SEXP li_iirr_total(NumericMatrix V, IntegerVector m, NumericVector ew, NumericMatrix X,
                          NumericVector t, IntegerVector sid) {
  const int n = V.nrow(), q = V.ncol(), N = X.nrow(), p = X.ncol();
  if (m.size() != n || ew.size() != n || t.size() != N || sid.size() != N)
    stop("non-conformable IIRR score inputs");
  long long msum = 0;
  for (int i = 0; i < n; ++i) {
    if (m[i] == NA_INTEGER) return R_NilValue;
    msum += m[i];
  }
  if (msum > INT_MAX || msum < -INT_MAX) return R_NilValue;
  for (int i = 0; i < N; ++i)
    if (sid[i] == NA_INTEGER || sid[i] < 1 || sid[i] > n) stop("subject index out of range");
  const double M = (double)msum;
  T s = 0.0;
  for (int i = 0; i < n; ++i) s += ew[i];
  const double sw = s > DBL_MAX ? R_PosInf : (s < -DBL_MAX ? R_NegInf : (double)s);
  std::vector<double> prob(n), f(n);
  for (int i = 0; i < n; ++i) {
    prob[i] = ew[i] / sw;
    f[i] = (double)m[i] - M * prob[i];
  }
  NumericVector out(q + p);
  for (int j = 0; j < q; ++j) {
    const double *v = V.begin() + (R_xlen_t)j * n;
    T vb = 0.0;
    for (int i = 0; i < n; ++i) vb += v[i] * prob[i];
    const double vbar = (double)vb;
    T tot = 0.0;
    for (int i = 0; i < n; ++i) tot += (v[i] - vbar) * f[i];
    out[j] = (double)tot;
  }
  std::vector<double> acc((size_t)n * p, 0.0);
  const int *g = sid.begin();
  for (int j = 0; j < p; ++j) {
    const double *x = X.begin() + (R_xlen_t)j * N;
    double *a = acc.data() + (size_t)j * n;
    for (int i = 0; i < N; ++i) a[g[i] - 1] += x[i] * t[i];
  }
  for (int j = 0; j < p; ++j) {
    const double *a = acc.data() + (size_t)j * n;
    T tot = 0.0;
    for (int i = 0; i < n; ++i) tot += a[i];
    out[q + j] = (double)tot;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP li_iirr_total_cpp(NumericMatrix V, IntegerVector m, NumericVector ew, NumericMatrix X,
                       NumericVector t, IntegerVector sid, bool long_double) {
  return long_double ? li_iirr_total<long double>(V, m, ew, X, t, sid)
                     : li_iirr_total<double>(V, m, ew, X, t, sid);
}
