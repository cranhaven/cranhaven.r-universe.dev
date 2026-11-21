#include <Rcpp.h>
#include <R_ext/Utils.h>
#include <Rversion.h>
#include <algorithm>
#include <cfloat>
#include <cmath>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize ("fp-contract=off")
#endif

using namespace Rcpp;


// Write indices must be present and in range; NA read indices yield NA.
static void lw_target_rows(const IntegerVector &x, int upper) {
  const int *p = x.begin();
  for (R_xlen_t i = 0, m = x.size(); i < m; ++i) {
    if (p[i] == NA_INTEGER) stop("NAs are not allowed in subscripted assignments");
    if (p[i] < 1 || p[i] > upper) stop("subscript out of bounds");
  }
}

static void lw_source_rows(const IntegerVector &x, R_xlen_t upper) {
  const int *p = x.begin();
  for (R_xlen_t i = 0, m = x.size(); i < m; ++i)
    if (p[i] != NA_INTEGER && (p[i] < 1 || p[i] > upper)) stop("subscript out of bounds");
}

static int lw_slices(const NumericVector &covariance, int q) {
  const R_xlen_t qq = (R_xlen_t)q * q;
  if (q < 0 || (qq ? covariance.size() % qq : covariance.size())) stop("covariance is not q x q x K");
  if (qq) return (int)(covariance.size() / qq);
  SEXP dim = covariance.attr("dim");
  return Rf_length(dim) == 3 ? INTEGER(dim)[2] : 0;
}

static inline double lw_ld(long double s) {
  return s > DBL_MAX ? R_PosInf : (s < -DBL_MAX ? R_NegInf : (double)s);
}

static inline double lw_int(int x) { return x == NA_INTEGER ? NA_REAL : (double)x; }

static inline double lw_at(const double *x, int i) { return i == NA_INTEGER ? NA_REAL : x[i - 1]; }

// [[Rcpp::export(rng = false)]]
NumericMatrix lw_running_cov_sum(NumericVector covariance, IntegerVector weight,
                                 IntegerVector index, int q) {
  const int K = weight.size(), qq = q * q, S = lw_slices(covariance, q);
  if (index.size() != K) stop("length(index) must equal length(weight)");
  lw_source_rows(index, S);
  NumericMatrix out(q, q);
  double *o = out.begin();
  const double *cv = covariance.begin();
  const int *ix = index.begin(), *wt = weight.begin();
  const std::vector<double> na(qq, NA_REAL);
  for (int k = 0; k < K; ++k) {
    const double w = lw_int(wt[k]);
    const double *c = ix[k] == NA_INTEGER ? na.data() : cv + (R_xlen_t)(ix[k] - 1) * qq;
    for (int ab = 0; ab < qq; ++ab) o[ab] = o[ab] + w * c[ab];
  }
  return out;
}

// Sums for B consecutive cells ab0, ..., ab0 + B - 1 of the q x q slices.
template <int B>
static void lw_cov_cells(const double *cv, const double *na, const int *wt, const int *ix, int K,
                         int qq, int ab0, double *out) {
  long double s[B];
  for (int b = 0; b < B; ++b) s[b] = 0.0L;
  for (int k = 0; k < K; ++k) {
    const double w = lw_int(wt[k]);
    const double *c = (ix[k] == NA_INTEGER ? na : cv + (R_xlen_t)(ix[k] - 1) * qq) + ab0;
    for (int b = 0; b < B; ++b) s[b] += w * c[b];
  }
  for (int b = 0; b < B; ++b) out[ab0 + b] = lw_ld(s[b]);
}

// [[Rcpp::export(rng = false)]]
NumericMatrix lw_indexed_cov_sum(NumericVector covariance, IntegerVector weight,
                                 IntegerVector index, int q) {
  const int K = weight.size(), qq = q * q, S = lw_slices(covariance, q);
  if (index.size() != K) stop("length(index) must equal length(weight)");
  lw_source_rows(index, S);
  NumericMatrix out(q, q);
  const double *cv = covariance.begin();
  const int *ix = index.begin(), *wt = weight.begin();
  const std::vector<double> na(qq, NA_REAL);
  int ab = 0;
  for (; ab + 4 <= qq; ab += 4) lw_cov_cells<4>(cv, na.data(), wt, ix, K, qq, ab, out.begin());
  for (; ab < qq; ++ab) lw_cov_cells<1>(cv, na.data(), wt, ix, K, qq, ab, out.begin());
  return out;
}

// [[Rcpp::export(rng = false)]]
double lw_indexed_dot(IntegerVector weight, NumericVector x, IntegerVector index) {
  const int K = weight.size();
  if (index.size() != K) stop("length(index) must equal length(weight)");
  lw_source_rows(index, x.size());
  const int *ix = index.begin(), *wt = weight.begin();
  const double *xp = x.begin();
  long double s = 0.0;
  for (int k = 0; k < K; ++k) s += lw_int(wt[k]) * lw_at(xp, ix[k]);
  return lw_ld(s);
}

// [[Rcpp::export(rng = false)]]
double lw_indexed_sum(NumericVector x, IntegerVector index) {
  lw_source_rows(index, x.size());
  const int *ix = index.begin();
  const double *xp = x.begin();
  long double s = 0.0;
  for (R_xlen_t k = 0, m = index.size(); k < m; ++k) s += lw_at(xp, ix[k]);
  return lw_ld(s);
}


// [[Rcpp::export(rng = false)]]
NumericMatrix lw_centered_group_sum(NumericMatrix x, NumericMatrix center,
                                    IntegerVector center_row, IntegerVector group, int n) {
  const int m = x.nrow(), q = x.ncol(), mc = center.nrow();
  if (center.ncol() != q) stop("non-conformable arrays");
  if (center_row.size() != m || group.size() != m) stop("incorrect length for 'group'");
  lw_source_rows(center_row, mc);
  lw_target_rows(group, n);
  NumericMatrix out(n, q);
  const int *cr = center_row.begin(), *g = group.begin();
  const double *xp = x.begin(), *cp = center.begin();
  double *op = out.begin();
  for (int r = 0; r < m;) {
    int e = r + 1;
    while (e < m && g[e] == g[r]) ++e;
    for (int c = 0; c < q; ++c) {
      const double *xc = xp + (R_xlen_t)c * m, *cc = cp + (R_xlen_t)c * mc;
      double *o = op + g[r] - 1 + (R_xlen_t)c * n, acc = *o;
      for (int t = r; t < e; ++t) acc += xc[t] - lw_at(cc, cr[t]);
      *o = acc;
    }
    r = e;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix lw_event_score_loop(NumericMatrix V, NumericMatrix vb, IntegerVector event_id,
                                  IntegerVector em) {
  const int n = V.nrow(), q = V.ncol(), K = vb.nrow(), m = event_id.size();
  if (vb.ncol() != q) stop("non-conformable arrays");
  if (em.size() != m) stop("length(em) must equal length(event_id)");
  lw_target_rows(event_id, n);
  lw_source_rows(em, K);
  NumericMatrix out(n, q);
  const int *ei = event_id.begin(), *ek = em.begin();
  const double *vp = V.begin(), *bp = vb.begin();
  double *op = out.begin();
  for (int r = 0; r < m;) {
    int e = r + 1;
    while (e < m && ei[e] == ei[r]) ++e;
    const int i = ei[r] - 1;
    for (int c = 0; c < q; ++c) {
      const double v = vp[i + (R_xlen_t)c * n], *b = bp + (R_xlen_t)c * K;
      double *o = op + i + (R_xlen_t)c * n, u = *o;
      for (int t = r; t < e; ++t) u = (u + v) - lw_at(b, ek[t]);
      *o = u;
    }
    r = e;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix lw_group_sum(NumericMatrix x, IntegerVector group, int n) {
  const int m = x.nrow(), p = x.ncol();
  if (group.size() != m) stop("incorrect length for 'group'");
  lw_target_rows(group, n);
  NumericMatrix out(n, p);
  const int *g = group.begin();
  const double *xp = x.begin();
  double *op = out.begin();
  for (int r = 0; r < m;) {
    int e = r + 1;
    while (e < m && g[e] == g[r]) ++e;
    for (int c = 0; c < p; ++c) {
      const double *xc = xp + (R_xlen_t)c * m;
      double *o = op + g[r] - 1 + (R_xlen_t)c * n, acc = *o;
      for (int t = r; t < e; ++t) acc += xc[t];
      *o = acc;
    }
    r = e;
  }
  return out;
}


static inline double lw_vexp(double x) {
#if R_VERSION >= R_Version(4, 6, 0)
  if (std::fabs(x) <= 0x1p-26) return x + 1.0;
#endif
  return std::exp(x);
}

struct lw_thresholds {
  double tiny, ratio, rel;
  lw_thresholds()
      : tiny(R_strtod("1e-250", NULL)), ratio(R_strtod("1e6", NULL)), rel(R_strtod("1e-10", NULL)) {}
};

static const lw_thresholds &lw_thr() {
  static const lw_thresholds t;
  return t;
}

// Column-wise max(abs(x)); a NaN in a column yields NaN.
static double lw_max_abs(const double *x, R_xlen_t m) {
  if (!m) return R_NegInf;
  double s[4] = {0.0, 0.0, 0.0, 0.0};
  bool nan = false;
  R_xlen_t i = 0;
  for (; i + 4 <= m; i += 4)
    for (int b = 0; b < 4; ++b) {
      const double a = std::fabs(x[i + b]);
      nan |= std::isnan(a);
      s[b] = a > s[b] ? a : s[b];
    }
  for (; i < m; ++i) {
    const double a = std::fabs(x[i]);
    nan |= std::isnan(a);
    s[0] = a > s[0] ? a : s[0];
  }
  return nan ? R_NaN : std::max(std::max(s[0], s[1]), std::max(s[2], s[3]));
}

// Maximum over finite x[index] (1-based indices); signed-zero ties use the first zero.
static double lw_max_finite(const double *x, int m, const int *index) {
  double s[4] = {R_NegInf, R_NegInf, R_NegInf, R_NegInf};
  int i = 0;
  for (; i + 4 <= m; i += 4)
    for (int b = 0; b < 4; ++b) {
      const double v = index ? x[index[i + b] - 1] : x[i + b];
      s[b] = std::isfinite(v) && v > s[b] ? v : s[b];
    }
  for (; i < m; ++i) {
    const double v = index ? x[index[i] - 1] : x[i];
    s[0] = std::isfinite(v) && v > s[0] ? v : s[0];
  }
  const double best = std::max(std::max(s[0], s[1]), std::max(s[2], s[3]));
  if (best != 0.0) return best;
  double r = 0.0;
  bool updated = false;
  for (i = 0; i < m; ++i) {
    const double v = index ? x[index[i] - 1] : x[i];
    if (std::isfinite(v) && (v > r || !updated)) {
      r = v;
      updated = true;
    }
  }
  return r;
}

// `a > b` inside the bad-row test; an NA comparison marks the row.
static inline bool lw_gt(double a, double b) { return std::isnan(a) || std::isnan(b) || a > b; }

template <int B>
static void lw_colsum_block(const double *x, int len, int c0, long double *out) {
  long double s[B];
  for (int b = 0; b < B; ++b) s[b] = 0.0L;
  for (int t = 0; t < len; ++t)
    for (int b = 0; b < B; ++b) s[b] += x[(size_t)(c0 + b) * len + t];
  for (int b = 0; b < B; ++b) out[c0 + b] = s[b];
}

static void lw_colsums(const double *x, int len, int nc, long double *out) {
  int c = 0;
  for (; nc - c >= 4; c += 4) lw_colsum_block<4>(x, len, c, out);
  switch (nc - c) {
  case 3: lw_colsum_block<3>(x, len, c, out); break;
  case 2: lw_colsum_block<2>(x, len, c, out); break;
  case 1: lw_colsum_block<1>(x, len, c, out); break;
  }
}

// Output positions: fwd[c * stride + off + t] and rev[c * stride + t].
// Input columns each have len rows.
template <int B>
static void lw_cumsum_block(const double *x, int len, int c0, double *fwd, double *rev,
                            int stride, int off) {
  long double f[B], r[B];
  for (int b = 0; b < B; ++b) f[b] = r[b] = 0.0L;
  for (int t = 0; t < len; ++t) {
    const int u = len - 1 - t;
    for (int b = 0; b < B; ++b) {
      const size_t c = (size_t)(c0 + b);
      f[b] += x[c * len + t];
      fwd[c * stride + off + t] = (double)f[b];
      r[b] += x[c * len + u];
      rev[c * stride + u] = (double)r[b];
    }
  }
}

static void lw_cumsums(const double *x, int len, int nc, double *fwd, double *rev, int stride,
                       int off) {
  int c = 0;
  for (; nc - c >= 4; c += 4) lw_cumsum_block<4>(x, len, c, fwd, rev, stride, off);
  switch (nc - c) {
  case 3: lw_cumsum_block<3>(x, len, c, fwd, rev, stride, off); break;
  case 2: lw_cumsum_block<2>(x, len, c, fwd, rev, stride, off); break;
  case 1: lw_cumsum_block<1>(x, len, c, fwd, rev, stride, off); break;
  }
}

// fin, lo and hi: 1-based indices; K: number of risk states. NULL requests fallback.
// [[Rcpp::export(rng = false)]]
SEXP lw_liang_risk_core(NumericVector log_weight, NumericMatrix values, IntegerVector fin,
                        IntegerVector lo, IntegerVector hi, int K) {
  const int n = values.nrow(), q = values.ncol(), nf = fin.size();
  if (log_weight.size() != n || lo.size() != n || hi.size() != n) stop("non-conformable inputs");
  if (!nf || !q || K < 1) stop("empty risk-set input");
  lw_source_rows(fin, n);
  const int *fi = fin.begin(), *lp = lo.begin(), *hp = hi.begin();
  for (int i = 0; i < nf; ++i) {
    const int j = fi[i] - 1;
    if (lp[j] == NA_INTEGER || hp[j] == NA_INTEGER || lp[j] < 1 || hp[j] > K || lp[j] > hp[j])
      stop("risk-state index out of range");
  }
  const lw_thresholds &thr = lw_thr();
  const double *lw = log_weight.begin(), *vx = values.begin();
  const double s = lw_max_finite(lw, nf, fi);
  std::vector<double> X((size_t)nf * (q + 1));
  for (int i = 0; i < nf; ++i) X[i] = lw_vexp(lw[fi[i] - 1] - s);
  for (int j = 0; j < q; ++j) {
    const double *v = vx + (R_xlen_t)j * n;
    for (int i = 0; i < nf; ++i) X[(size_t)(j + 1) * nf + i] = v[fi[i] - 1] * X[i];
  }
  std::vector<long double> sums(q + 1);
  lw_colsums(X.data(), nf, q + 1, sums.data());
  const double sum_w = lw_ld(sums[0]), *w = X.data();
  std::vector<double> ctr(q), vc((size_t)nf * q);
  for (int j = 0; j < q; ++j) {
    const double *v = vx + (R_xlen_t)j * n;
    ctr[j] = (double)sums[j + 1] / sum_w;
    for (int i = 0; i < nf; ++i) vc[(size_t)j * nf + i] = v[fi[i] - 1] - ctr[j];
  }
  std::vector<int> pa, pb;
  for (int b = 0; b < q; ++b)
    for (int a = 0; a <= b; ++a) {
      pa.push_back(a);
      pb.push_back(b);
    }
  const int np = pa.size(), nc = 1 + q + np, L = K + 2;
  std::vector<double> Bh((size_t)L * nc, 0.0), Bl((size_t)L * nc, 0.0);
  for (int i = 0; i < nf; ++i) {
    const int j = fi[i] - 1, h = hp[j], l = lp[j];
    for (int c = 0; c < nc; ++c) {
      double m;
      if (c == 0) m = w[i];
      else if (c <= q) m = vc[(size_t)(c - 1) * nf + i] * w[i];
      else {
        const int r = c - 1 - q;
        m = (vc[(size_t)pa[r] * nf + i] * vc[(size_t)pb[r] * nf + i]) * w[i];
      }
      Bh[(size_t)c * L + h] += m;
      Bl[(size_t)c * L + l] += m;
    }
  }
  std::vector<double> rch((size_t)L * nc), fch((size_t)L * nc), rcl((size_t)L * nc),
      fcl((size_t)L * nc);
  lw_cumsums(Bh.data(), L, nc, fch.data(), rch.data(), L, 0);
  lw_cumsums(Bl.data(), L, nc, fcl.data(), rcl.data(), L, 0);
  std::vector<double> S((size_t)K * nc), big(K);
  std::vector<char> use_a(K);
  for (int k = 0; k < K; ++k) {
    use_a[k] = rch[k + 1] <= fcl[k + 1];
    big[k] = use_a[k] ? rch[k + 1] : fcl[k + 1];
  }
  for (int c = 0; c < nc; ++c) {
    const size_t o = (size_t)c * L;
    for (int k = 0; k < K; ++k)
      S[(size_t)c * K + k] = use_a[k] ? rch[o + k + 1] - rcl[o + k + 2] : fcl[o + k + 1] - fch[o + k];
  }
  for (int k = 0; k < K; ++k) {
    const double s0 = S[k];
    if (!(std::isfinite(s0) && s0 > thr.tiny && std::isfinite(big[k]) && big[k] <= thr.ratio * s0))
      return R_NilValue;
  }
  NumericVector logs(no_init(K));
  NumericMatrix means(no_init(K, q));
  NumericVector covs(no_init((R_xlen_t)q * q * K));
  LogicalVector bad(K);
  std::vector<double> mag(q);
  for (int j = 0; j < q; ++j) mag[j] = lw_max_abs(&vc[(size_t)j * nf], nf);
  std::vector<double> mu(q), cv(np);
  for (int k = 0; k < K; ++k) {
    const double s0 = S[k];
    bool b = false;
    for (int j = 0; j < q; ++j) mu[j] = S[(size_t)(1 + j) * K + k] / s0;
    for (int r = 0; r < np; ++r)
      cv[r] = S[(size_t)(1 + q + r) * K + k] / s0 - mu[pa[r]] * mu[pb[r]];
    double *cc = covs.begin() + (R_xlen_t)k * q * q;
    for (int r = 0; r < np; ++r) {
      cc[pa[r] + pb[r] * q] = cv[r];
      cc[pb[r] + pa[r] * q] = cv[r];
    }
    const double rounding = 0x1p-46 * big[k] / s0;
    for (int j = 0; j < q; ++j) {
      const double m = mu[j] + ctr[j];
      means[k + (R_xlen_t)j * K] = m;
      if (!std::isfinite(m) || lw_gt(rounding * mag[j], thr.rel * (1 + std::fabs(m)))) b = true;
    }
    for (int r = 0; r < np; ++r) {
      if (!std::isfinite(cv[r]) || lw_gt(rounding * (mag[pa[r]] * mag[pb[r]]), thr.rel * (1 + std::fabs(cv[r]))))
        b = true;
      if (pa[r] == pb[r] && (std::isnan(cv[r]) || cv[r] < 0)) b = true;
    }
    bad[k] = b;
    logs[k] = s + std::log(s0);
  }
  covs.attr("dim") = IntegerVector::create(q, q, K);
  return List::create(_["log_sum"] = logs, _["mean"] = means, _["covariance"] = covs,
                      _["bad"] = bad);
}

// lw and v: time-sorted; lo and hi: findInterval() bounds.
// Returns NULL for fallback; bad contains 1-based subject indices.
// [[Rcpp::export(rng = false)]]
SEXP lw_liang_interval_core(NumericVector lw, NumericMatrix v, IntegerVector lo, IntegerVector hi) {
  const int K = v.nrow(), q = v.ncol(), n = lo.size();
  if (lw.size() != K || hi.size() != n) stop("non-conformable inputs");
  const lw_thresholds &thr = lw_thr();
  const int *lp = lo.begin(), *hp = hi.begin();
  const double *lx = lw.begin(), *vx = v.begin();
  std::vector<int> ok;
  for (int i = 0; i < n; ++i) {
    if (lp[i] == NA_INTEGER || hp[i] == NA_INTEGER || lp[i] < 1 || hp[i] > K)
      stop("interval index out of range");
    if (hp[i] >= lp[i]) ok.push_back(i);
  }
  NumericVector outl(n, R_NegInf);
  NumericMatrix outm(n, q);
  IntegerVector none(0);
  if (ok.empty()) return List::create(_["log_sum"] = outl, _["mean"] = outm, _["bad"] = none);
  std::vector<int> cf(K + 1, 0);
  for (int t = 0; t < K; ++t) cf[t + 1] = cf[t] + (std::isfinite(lx[t]) ? 1 : 0);
  int min_lo = K + 1;
  for (int i : ok) {
    if (cf[hp[i]] - cf[lp[i] - 1] == 0) return R_NilValue;
    min_lo = std::min(min_lo, lp[i]);
  }
  const double s = lw_max_finite(lx, K, nullptr);
  const int nc = q + 1, L = K + 1;
  std::vector<double> X((size_t)K * nc);
  double *w = X.data();
  for (int t = 0; t < K; ++t) w[t] = lw_vexp(lx[t] - s);
  for (int t = 0; t < min_lo - 1; ++t) w[t] = 0;
  for (int j = 0; j < q; ++j) {
    const double *vj = vx + (R_xlen_t)j * K;
    for (int t = 0; t < K; ++t) X[(size_t)(j + 1) * K + t] = vj[t] * w[t];
  }
  std::vector<long double> sums(nc);
  lw_colsums(X.data(), K, nc, sums.data());
  const double sum_w = lw_ld(sums[0]);
  std::vector<double> ctr(q), vc((size_t)K * q), P((size_t)L * nc), Q((size_t)L * nc);
  for (int j = 0; j < q; ++j) {
    const double *vj = vx + (R_xlen_t)j * K;
    ctr[j] = (double)sums[j + 1] / sum_w;
    for (int t = 0; t < K; ++t) {
      vc[(size_t)j * K + t] = vj[t] - ctr[j];
      X[(size_t)(j + 1) * K + t] = vc[(size_t)j * K + t] * w[t];
    }
  }
  lw_cumsums(X.data(), K, nc, P.data(), Q.data(), L, 1);
  for (int c = 0; c < nc; ++c) P[(size_t)c * L] = Q[(size_t)c * L + K] = 0;
  const int m = ok.size();
  std::vector<double> S((size_t)m * nc), big(m);
  for (int r = 0; r < m; ++r) {
    const int i = ok[r], a = hp[i], b = lp[i] - 1;
    const bool use_p = P[a] <= Q[b];
    big[r] = use_p ? P[a] : Q[b];
    for (int c = 0; c < nc; ++c) {
      const size_t o = (size_t)c * L;
      S[(size_t)c * m + r] = use_p ? P[o + a] - P[o + b] : Q[o + b] - Q[o + a];
    }
  }
  for (int r = 0; r < m; ++r) {
    const double s0 = S[r];
    if (!(std::isfinite(s0) && s0 > thr.tiny && std::isfinite(big[r]) && big[r] <= thr.ratio * s0))
      return R_NilValue;
  }
  std::vector<double> mag(q);
  for (int j = 0; j < q; ++j) mag[j] = lw_max_abs(&vc[(size_t)j * K], K);
  std::vector<int> bad;
  double *om = outm.begin();
  for (int r = 0; r < m; ++r) {
    const int i = ok[r];
    const double s0 = S[r], rounding = 0x1p-46 * big[r] / s0;
    bool b = false;
    for (int j = 0; j < q; ++j) {
      const double mj = S[(size_t)(1 + j) * m + r] / s0 + ctr[j];
      om[i + (R_xlen_t)j * n] = mj;
      if (!std::isfinite(mj) || lw_gt(rounding * mag[j], thr.rel * (1 + std::fabs(mj)))) b = true;
    }
    outl[i] = s + std::log(s0);
    if (b) bad.push_back(i + 1);
  }
  return List::create(_["log_sum"] = outl, _["mean"] = outm, _["bad"] = wrap(bad));
}
