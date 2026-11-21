#include <Rcpp.h>
#include <Rversion.h>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;


[[noreturn]] static void ly_na_condition() { stop("missing value where TRUE/FALSE needed"); }

static inline double ly_exp(double x, bool r_dispatch) {
#if R_VERSION >= R_Version(4, 6, 0)
  if (r_dispatch && std::fabs(x) <= 0x1p-26) return x + 1.0;
#endif
  return std::exp(x);
}

// Stable order; NA and NaN last in input order.
static void ly_order(const double *x, int n, bool decreasing, std::vector<int> &o) {
  o.resize(n);
  std::iota(o.begin(), o.end(), 0);
  auto inc = [x](int a, int b) { return !ISNAN(x[a]) && (ISNAN(x[b]) || x[a] < x[b]); };
  auto dec = [x](int a, int b) { return !ISNAN(x[a]) && (ISNAN(x[b]) || x[a] > x[b]); };
  if (decreasing) {
    if (!std::is_sorted(o.begin(), o.end(), dec)) std::stable_sort(o.begin(), o.end(), dec);
  } else if (!std::is_sorted(o.begin(), o.end(), inc)) {
    std::stable_sort(o.begin(), o.end(), inc);
  }
}

// All-equal test: TRUE for length zero; errors on NA.
static bool ly_common(const NumericVector &x) {
  const R_xlen_t n = x.size();
  bool na = false;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (ISNAN(x[i]) || ISNAN(x[0])) na = true;
    else if (x[i] != x[0]) return false;
  }
  if (na) ly_na_condition();
  return true;
}

static inline void ly_mix_cov(double la, const double *ma, const double *ca, double lb,
                              const double *mb, const double *cb, double &lo, double *mo,
                              double *co, int q, double *d, bool rx) {
  const int qq = q * q;
  if (ISNAN(la)) ly_na_condition();
  if (la == R_NegInf) {
    lo = lb;
    if (mo != mb) std::copy(mb, mb + q, mo);
    if (co != cb) std::copy(cb, cb + qq, co);
    return;
  }
  if (ISNAN(lb)) ly_na_condition();
  if (lb == R_NegInf) {
    lo = la;
    if (mo != ma) std::copy(ma, ma + q, mo);
    if (co != ca) std::copy(ca, ca + qq, co);
    return;
  }
  const double z = std::max(la, lb);
  const double ll = z + std::log(ly_exp(la - z, rx) + ly_exp(lb - z, rx));
  const double wa = ly_exp(la - ll, rx), wb = ly_exp(lb - ll, rx), wab = wa * wb;
  for (int i = 0; i < q; ++i) d[i] = mb[i] - ma[i];
  for (int j = 0; j < q; ++j)
    for (int i = 0; i < q; ++i) {
      const int ij = i + j * q;
      co[ij] = (wa * ca[ij] + wb * cb[ij]) + wab * (0.0 + d[i] * d[j]);
    }
  for (int i = 0; i < q; ++i) mo[i] = wa * ma[i] + wb * mb[i];
  lo = ll;
}

static inline void ly_mix_mean(double la, const double *ma, double lb, const double *mb,
                               double &lo, double *mo, int q, bool rx) {
  if (ISNAN(la)) ly_na_condition();
  if (la == R_NegInf) {
    lo = lb;
    if (mo != mb) std::copy(mb, mb + q, mo);
    return;
  }
  if (ISNAN(lb)) ly_na_condition();
  if (lb == R_NegInf) {
    lo = la;
    if (mo != ma) std::copy(ma, ma + q, mo);
    return;
  }
  const double z = std::max(la, lb);
  const double ll = z + std::log(ly_exp(la - z, rx) + ly_exp(lb - z, rx));
  const double wa = ly_exp(la - ll, rx), wb = ly_exp(lb - ll, rx);
  for (int i = 0; i < q; ++i) mo[i] = wa * ma[i] + wb * mb[i];
  lo = ll;
}

// [[Rcpp::export(rng = false)]]
List ly_stable_risk_cpp(NumericVector log_weight, NumericMatrix values, NumericVector times,
                        NumericVector entry, NumericVector censor) {
  const int n = values.nrow(), q = values.ncol(), qq = q * q;
  if (log_weight.size() != n) stop("length(log_weight) == n is not TRUE");
  if (entry.size() != n) stop("length(entry) == n is not TRUE");
  if (censor.size() != n) stop("length(censor) == n is not TRUE");
  bool bad = false;
  for (int i = 0; i < n && !bad; ++i) bad = ISNAN(log_weight[i]);
  for (int i = 0; i < n && !bad; ++i) bad = log_weight[i] == R_PosInf;
  for (R_xlen_t i = 0; i < values.size() && !bad; ++i) bad = !R_FINITE(values[i]);
  if (bad) stop("Non-finite risk-set inputs.");
  const int K = times.size();
  std::vector<int> ord;
  ly_order(times.begin(), K, false, ord);
  std::vector<double> tt(K);
  for (int k = 0; k < K; ++k) tt[k] = times[ord[k]];
  NumericVector logs(K, R_NegInf);
  NumericMatrix means(K, q);
  std::fill(means.begin(), means.end(), NA_REAL);
  NumericVector covs(no_init((R_xlen_t)qq * K));
  std::fill(covs.begin(), covs.end(), 0.0);
  IntegerVector counts(K);
  covs.attr("dim") = IntegerVector::create(q, q, K);
  List out = List::create(_["log_sum"] = logs, _["mean"] = means, _["covariance"] = covs,
                          _["n_risk"] = counts);
  if (!K) return out;
  const double *lw = log_weight.begin(), *vx = values.begin(), *ce = censor.begin(),
               *en = entry.begin();
  double *lg = logs.begin(), *mn = means.begin(), *cv = covs.begin();
  int *ct = counts.begin();
  auto store = [&](int k, double l, const double *m, const double *c, int nr) {
    const int pos = ord[k];
    lg[pos] = l;
    for (int a = 0; a < q; ++a) mn[pos + (R_xlen_t)a * K] = m[a];
    std::copy(c, c + qq, cv + (R_xlen_t)pos * qq);
    ct[pos] = nr;
  };
  std::vector<double> d(q), zero(qq, 0.0);
  std::vector<int> oc, oe;
  if (ly_common(entry)) {
    ly_order(ce, n, true, oc);
    const bool named = !Rf_isNull(Rf_getAttrib(log_weight, R_NamesSymbol));
    const double e0 = n ? en[0] : NA_REAL;
    double ll = R_NegInf;
    std::vector<double> mm(q, 0.0), cc(qq, 0.0), vj(q);
    int nr = 0, pc = 0;
    for (int k = K - 1; k >= 0; --k) {
      const double t = tt[k];
      while (pc < n) {
        const int j = oc[pc];
        if (ISNAN(ce[j]) || ISNAN(t)) ly_na_condition();
        if (!(ce[j] >= t)) break;
        ++nr;
        for (int a = 0; a < q; ++a) vj[a] = vx[j + (R_xlen_t)a * n];
        ly_mix_cov(ll, mm.data(), cc.data(), lw[j], vj.data(), zero.data(), ll, mm.data(),
                   cc.data(), q, d.data(), named);
        ++pc;
      }
      if (ISNAN(t) || ISNAN(e0)) ly_na_condition();
      if (t > e0) store(k, ll, mm.data(), cc.data(), nr);
    }
  } else {
    int size = 1;
    while (size < n) size <<= 1;
    const int total = 2 * size;
    std::vector<double> ll(total, R_NegInf), mm((size_t)total * q, 0.0),
        cc((size_t)total * qq, 0.0);
    std::vector<int> nrv(total, 0);
    auto update = [&](int j, bool active) {
      int at = size + j;
      ll[at] = active ? lw[j] : R_NegInf;
      for (int a = 0; a < q; ++a) mm[(size_t)at * q + a] = active ? vx[j + (R_xlen_t)a * n] : 0.0;
      std::fill(cc.begin() + (size_t)at * qq, cc.begin() + (size_t)(at + 1) * qq, 0.0);
      nrv[at] = active ? 1 : 0;
      for (at /= 2; at >= 1; at /= 2) {
        const int a = 2 * at, b = a + 1;
        ly_mix_cov(ll[a], &mm[(size_t)a * q], &cc[(size_t)a * qq], ll[b], &mm[(size_t)b * q],
                   &cc[(size_t)b * qq], ll[at], &mm[(size_t)at * q], &cc[(size_t)at * qq], q,
                   d.data(), false);
        nrv[at] = nrv[a] + nrv[b];
      }
    };
    ly_order(en, n, false, oe);
    ly_order(ce, n, false, oc);
    int pe = 0, pc = 0;
    for (int k = 0; k < K; ++k) {
      const double t = tt[k];
      while (pe < n) {
        const int j = oe[pe];
        if (ISNAN(en[j]) || ISNAN(t)) ly_na_condition();
        if (!(en[j] < t)) break;
        update(j, true);
        ++pe;
      }
      while (pc < n) {
        const int j = oc[pc];
        if (ISNAN(ce[j]) || ISNAN(t)) ly_na_condition();
        if (!(ce[j] < t)) break;
        update(j, false);
        ++pc;
      }
      store(k, ll[1], &mm[q], &cc[qq], nrv[1]);
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
List ly_stable_interval_cpp(NumericVector times, NumericVector log_weight, NumericMatrix values,
                            NumericVector entry, NumericVector censor) {
  const int q = values.ncol(), nv = values.nrow(), K = times.size(), n = entry.size();
  const int nw = log_weight.size();
  bool bad = false;
  for (int i = 0; i < nw && !bad; ++i) bad = ISNAN(log_weight[i]);
  for (int i = 0; i < nw && !bad; ++i) bad = log_weight[i] == R_PosInf;
  for (R_xlen_t i = 0; i < values.size() && !bad; ++i) bad = !R_FINITE(values[i]);
  if (bad) stop("Non-finite interval-sum inputs.");
  std::vector<int> ord;
  ly_order(times.begin(), K, false, ord);
  std::vector<double> ts(K), lw(K), vs((size_t)K * q);
  for (int k = 0; k < K; ++k) {
    const int r = ord[k];
    ts[k] = times[r];
    lw[k] = r < nw ? log_weight[r] : NA_REAL;
  }
  for (int k = 0; k < K; ++k) {
    const int r = ord[k];
    if (r >= nv) stop("subscript out of bounds");
    for (int a = 0; a < q; ++a) vs[(size_t)k * q + a] = values[r + (R_xlen_t)a * nv];
  }
  for (int k = 0; k < K; ++k)
    if (ISNAN(ts[k])) stop("'vec' must be sorted non-decreasingly and not contain NAs");
  if (censor.size() != n) stop("length(censor) must equal length(entry)");
  std::vector<int> lo(n), hi(n);
  for (int i = 0; i < n; ++i) {
    lo[i] = ISNAN(entry[i]) ? NA_INTEGER
                            : (int)(std::upper_bound(ts.begin(), ts.end(), entry[i]) - ts.begin()) + 1;
    hi[i] = ISNAN(censor[i]) ? NA_INTEGER
                             : (int)(std::upper_bound(ts.begin(), ts.end(), censor[i]) - ts.begin());
  }
  NumericVector outl(n, R_NegInf);
  NumericMatrix outm(n, q);
  double *om = outm.begin();
  if (ly_common(entry)) {
    std::vector<int> oi;
    ly_order(censor.begin(), n, false, oi);
    const bool named = !Rf_isNull(Rf_getAttrib(log_weight, R_NamesSymbol));
    int at = n ? lo[0] : 0;
    double ll = R_NegInf;
    std::vector<double> mm(q, 0.0);
    for (int i : oi) {
      if (hi[i] == NA_INTEGER || lo[i] == NA_INTEGER) ly_na_condition();
      while (at <= hi[i] && at <= K) {
        ly_mix_mean(ll, mm.data(), lw[at - 1], &vs[(size_t)(at - 1) * q], ll, mm.data(), q,
                    named);
        ++at;
      }
      if (hi[i] >= lo[i]) {
        outl[i] = ll;
        for (int a = 0; a < q; ++a) om[i + (R_xlen_t)a * n] = mm[a];
      }
    }
  } else {
    int size = 1;
    while (size < K) size <<= 1;
    const int total = 2 * size;
    std::vector<double> ll(total, R_NegInf), mm((size_t)total * q, 0.0);
    for (int k = 0; k < K; ++k) {
      ll[size + k] = lw[k];
      std::copy(&vs[(size_t)k * q], &vs[(size_t)k * q] + q, &mm[(size_t)(size + k) * q]);
    }
    for (int at = size - 1; at >= 1; --at) {
      const int a = 2 * at, b = a + 1;
      ly_mix_mean(ll[a], &mm[(size_t)a * q], ll[b], &mm[(size_t)b * q], ll[at],
                  &mm[(size_t)at * q], q, false);
    }
    std::vector<double> sm(q);
    for (int i = 0; i < n; ++i) {
      if (hi[i] == NA_INTEGER || lo[i] == NA_INTEGER) ly_na_condition();
      if (hi[i] < lo[i]) continue;
      int a = size + lo[i] - 1, b = size + hi[i] - 1;
      double sl = R_NegInf;
      std::fill(sm.begin(), sm.end(), 0.0);
      while (a <= b) {
        if (a % 2 == 1) {
          ly_mix_mean(sl, sm.data(), ll[a], &mm[(size_t)a * q], sl, sm.data(), q, false);
          ++a;
        }
        if (b % 2 == 0) {
          ly_mix_mean(sl, sm.data(), ll[b], &mm[(size_t)b * q], sl, sm.data(), q, false);
          --b;
        }
        a /= 2;
        b /= 2;
      }
      outl[i] = sl;
      for (int c = 0; c < q; ++c) om[i + (R_xlen_t)c * n] = sm[c];
    }
  }
  return List::create(_["log_sum"] = outl, _["mean"] = outm);
}
