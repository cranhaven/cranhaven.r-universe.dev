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


// [[Rcpp::export(rng = false)]]
List iib_gamma_parts(NumericMatrix V, NumericMatrix vb, NumericVector covariance, IntegerVector dn,
                     IntegerVector ei, IntegerVector em) {
  const int n = V.nrow(), q = V.ncol(), K = vb.nrow(), qq = q * q;
  const R_xlen_t R = ei.size();
  if (vb.ncol() != q || dn.size() != K || covariance.size() != (R_xlen_t)qq * K || em.size() != R)
    stop("non-conformable arguments");
  NumericMatrix information(q, q);
  double *inf = information.begin();
  const double *cv = covariance.begin();
  for (int k = 0; k < K; ++k) {
    const double w = (double)dn[k];
    const double *c = cv + (R_xlen_t)k * qq;
    for (int ij = 0; ij < qq; ++ij) inf[ij] = inf[ij] + w * c[ij];
  }
  NumericMatrix U(n, q);
  double *u = U.begin();
  const double *v = V.begin(), *b = vb.begin();
  for (R_xlen_t r = 0; r < R; ++r) {
    if (ei[r] == NA_INTEGER || em[r] == NA_INTEGER) stop("subscript out of bounds");
    const int i = ei[r] - 1, k = em[r] - 1;
    if (i < 0 || i >= n || k < 0 || k >= K) stop("subscript out of bounds");
    for (int c = 0; c < q; ++c) {
      const R_xlen_t ic = i + (R_xlen_t)c * n;
      u[ic] = (u[ic] + v[ic]) - b[k + (R_xlen_t)c * K];
    }
  }
  return List::create(_["information"] = information, _["U"] = U);
}

// [[Rcpp::export(rng = false)]]
List iib_balance_sweep(NumericMatrix V, NumericVector tu, NumericVector entry, NumericVector censor,
                       IntegerVector oe, IntegerVector oc) {
  const int n = V.nrow(), q = V.ncol(), K = tu.size();
  if (entry.size() != n || censor.size() != n || oe.size() != n || oc.size() != n)
    stop("non-conformable arguments");
  IntegerVector n_risk(K);
  NumericMatrix mean(K, q);
  std::vector<double> sumV(q, 0.0);
  const double *v = V.begin();
  int pe = 0, pc = 0, nr = 0;
  for (int k = 0; k < K; ++k) {
    const double t = tu[k];
    while (pe < n && entry[oe[pe] - 1] < t) {
      const int j = oe[pe] - 1;
      ++nr;
      for (int c = 0; c < q; ++c) sumV[c] = sumV[c] + v[j + (R_xlen_t)c * n];
      ++pe;
    }
    while (pc < n && censor[oc[pc] - 1] < t) {
      const int j = oc[pc] - 1;
      --nr;
      for (int c = 0; c < q; ++c) sumV[c] = sumV[c] - v[j + (R_xlen_t)c * n];
      ++pc;
    }
    n_risk[k] = nr;
    if (nr <= 0) break;
    for (int c = 0; c < q; ++c) mean(k, c) = sumV[c] / (double)nr;
  }
  return List::create(_["n_risk"] = n_risk, _["mean"] = mean);
}

// [[Rcpp::export(rng = false)]]
List iib_scatter2(NumericMatrix M, NumericVector w, Nullable<NumericVector> s, IntegerVector i1, int n1,
                  IntegerVector i2, int n2) {
  const int R = M.nrow(), p = M.ncol();
  if (w.size() != R || i1.size() != R || i2.size() != R) stop("non-conformable arguments");
  NumericVector sv;
  const bool scaled = s.isNotNull();
  if (scaled) {
    sv = NumericVector(s);
    if (sv.size() != R) stop("non-conformable arguments");
  }
  for (int r = 0; r < R; ++r)
    if (i1[r] == NA_INTEGER || i1[r] < 1 || i1[r] > n1 || i2[r] == NA_INTEGER || i2[r] < 1 || i2[r] > n2)
      stop("subscript out of bounds");
  NumericMatrix a(n1, p), b(n2, p);
  const double *m = M.begin(), *wr = w.begin(), *sr = scaled ? sv.begin() : nullptr;
  for (int j = 0; j < p; ++j) {
    const double *mj = m + (R_xlen_t)j * R;
    double *aj = a.begin() + (R_xlen_t)j * n1, *bj = b.begin() + (R_xlen_t)j * n2;
    for (int r = 0; r < R; ++r) {
      double v = mj[r] * wr[r];
      if (scaled) v = v * sr[r];
      aj[i1[r] - 1] += v;
      bj[i2[r] - 1] += v;
    }
  }
  return List::create(a, b);
}

// [[Rcpp::export(rng = false)]]
bool iib_all_finite(NumericVector values) {
  const double *v = values.begin();
  for (R_xlen_t i = 0; i < values.size(); ++i)
    if (!std::isfinite(v[i])) return false;
  return true;
}

// The plan and value matrices must have matching row counts.

[[noreturn]] static void iib_na_condition() { stop("missing value where TRUE/FALSE needed"); }

static inline double iib_exp(double x, bool r_dispatch) {
#if R_VERSION >= R_Version(4, 6, 0)
  if (r_dispatch && std::fabs(x) <= 0x1p-26) return x + 1.0;
#endif
  return std::exp(x);
}

static void iib_order(const double *x, int n, std::vector<int> &o) {
  o.resize(n);
  std::iota(o.begin(), o.end(), 0);
  auto inc = [x](int a, int b) { return !std::isnan(x[a]) && (std::isnan(x[b]) || x[a] < x[b]); };
  if (!std::is_sorted(o.begin(), o.end(), inc)) std::stable_sort(o.begin(), o.end(), inc);
}

static bool iib_common(const NumericVector &x) {
  const R_xlen_t n = x.size();
  bool na = false;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (std::isnan(x[i]) || std::isnan(x[0])) na = true;
    else if (x[i] != x[0]) return false;
  }
  if (na) iib_na_condition();
  return true;
}

enum { IIB_MIX = 0, IIB_COPY_B = 1, IIB_KEEP_A = 2 };

// IIB_COPY_B: m <- mb, IIB_KEEP_A: m <- ma, IIB_MIX: m <- wa*ma + wb*mb.
static inline int iib_mix_log(double la, double lb, double &lo, double &wa, double &wb, bool rx) {
  wa = wb = 0.0;
  if (std::isnan(la)) iib_na_condition();
  if (la == R_NegInf) {
    lo = lb;
    return IIB_COPY_B;
  }
  if (std::isnan(lb)) iib_na_condition();
  if (lb == R_NegInf) {
    lo = la;
    return IIB_KEEP_A;
  }
  const double z = std::max(la, lb);
  const double ll = z + std::log(iib_exp(la - z, rx) + iib_exp(lb - z, rx));
  wa = iib_exp(la - ll, rx);
  wb = iib_exp(lb - ll, rx);
  lo = ll;
  return IIB_MIX;
}

static inline void iib_mix_mean(int kind, double wa, double wb, const double *ma, const double *mb,
                                double *mo, int q) {
  if (kind == IIB_COPY_B) {
    if (mo != mb) std::copy(mb, mb + q, mo);
  } else if (kind == IIB_KEEP_A) {
    if (mo != ma) std::copy(ma, ma + q, mo);
  } else {
    for (int i = 0; i < q; ++i) mo[i] = wa * ma[i] + wb * mb[i];
  }
}

// [[Rcpp::export(rng = false)]]
List iib_interval_plan(NumericVector times, NumericVector log_weight, int rows, NumericVector entry,
                       NumericVector censor) {
  const int K = times.size(), n = entry.size(), nw = log_weight.size();
  std::vector<int> ord;
  iib_order(times.begin(), K, ord);
  std::vector<double> ts(K), lw(K);
  for (int k = 0; k < K; ++k) {
    const int r = ord[k];
    ts[k] = times[r];
    lw[k] = r < nw ? log_weight[r] : NA_REAL;
  }
  for (int k = 0; k < K; ++k)
    if (ord[k] >= rows) stop("subscript out of bounds");
  for (int k = 0; k < K; ++k)
    if (std::isnan(ts[k])) stop("'vec' must be sorted non-decreasingly and not contain NAs");
  if (censor.size() != n) stop("length(censor) must equal length(entry)");
  std::vector<int> lo(n), hi(n);
  for (int i = 0; i < n; ++i) {
    lo[i] = std::isnan(entry[i]) ? NA_INTEGER
                            : (int)(std::upper_bound(ts.begin(), ts.end(), entry[i]) - ts.begin()) + 1;
    hi[i] = std::isnan(censor[i]) ? NA_INTEGER
                             : (int)(std::upper_bound(ts.begin(), ts.end(), censor[i]) - ts.begin());
  }
  NumericVector outl(n, R_NegInf);
  std::vector<int> src, kind, node, start, who;
  std::vector<double> wa, wb;
  double a_w, b_w;
  int size = 0;
  const bool sweep = iib_common(entry);
  if (sweep) {
    std::vector<int> oi;
    iib_order(censor.begin(), n, oi);
    const bool named = !Rf_isNull(Rf_getAttrib(log_weight, R_NamesSymbol));
    int at = n ? lo[0] : 0;
    double ll = R_NegInf;
    for (int i : oi) {
      if (hi[i] == NA_INTEGER || lo[i] == NA_INTEGER) iib_na_condition();
      while (at <= hi[i] && at <= K) {
        kind.push_back(iib_mix_log(ll, lw[at - 1], ll, a_w, b_w, named));
        src.push_back(ord[at - 1]);
        wa.push_back(a_w);
        wb.push_back(b_w);
        ++at;
      }
      if (hi[i] >= lo[i]) {
        outl[i] = ll;
        who.push_back(i);
        start.push_back((int)kind.size());
      }
    }
  } else {
    size = 1;
    while (size < K) size <<= 1;
    const int total = 2 * size;
    std::vector<double> ll(total, R_NegInf);
    for (int k = 0; k < K; ++k) ll[size + k] = lw[k];
    for (int at = size - 1; at >= 1; --at) {
      kind.push_back(iib_mix_log(ll[2 * at], ll[2 * at + 1], ll[at], a_w, b_w, false));
      wa.push_back(a_w);
      wb.push_back(b_w);
    }
    for (int i = 0; i < n; ++i) {
      if (hi[i] == NA_INTEGER || lo[i] == NA_INTEGER) iib_na_condition();
      if (hi[i] < lo[i]) continue;
      int a = size + lo[i] - 1, b = size + hi[i] - 1;
      double sl = R_NegInf;
      while (a <= b) {
        if (a % 2 == 1) {
          kind.push_back(iib_mix_log(sl, ll[a], sl, a_w, b_w, false));
          node.push_back(a);
          wa.push_back(a_w);
          wb.push_back(b_w);
          ++a;
        }
        if (b % 2 == 0) {
          kind.push_back(iib_mix_log(sl, ll[b], sl, a_w, b_w, false));
          node.push_back(b);
          wa.push_back(a_w);
          wb.push_back(b_w);
          --b;
        }
        a /= 2;
        b /= 2;
      }
      outl[i] = sl;
      who.push_back(i);
      start.push_back((int)kind.size());
    }
  }
  return List::create(_["rows"] = rows, _["n"] = n, _["size"] = size, _["order"] = wrap(ord),
                      _["kind"] = wrap(kind), _["source"] = wrap(src), _["node"] = wrap(node),
                      _["wa"] = wrap(wa), _["wb"] = wrap(wb), _["subject"] = wrap(who),
                      _["end"] = wrap(start), _["log_sum"] = outl);
}

// [[Rcpp::export(rng = false)]]
List iib_interval_apply(List plan, NumericMatrix values) {
  const int rows = as<int>(plan["rows"]), n = as<int>(plan["n"]), size = as<int>(plan["size"]);
  const int q = values.ncol();
  if (values.nrow() != rows) stop("non-conformable arguments");
  IntegerVector ord = plan["order"], kind = plan["kind"], src = plan["source"], node = plan["node"],
                who = plan["subject"], end = plan["end"];
  NumericVector wa = plan["wa"], wb = plan["wb"], logs = plan["log_sum"];
  NumericVector outl = clone(logs);
  NumericMatrix outm(n, q);
  double *om = outm.begin();
  const double *vx = values.begin();
  const int S = who.size(), K = ord.size();
  if (!size) {
    std::vector<double> mm(q, 0.0), vr(q);
    int s = 0;
    for (int j = 0; j < S; ++j) {
      for (; s < end[j]; ++s) {
        for (int c = 0; c < q; ++c) vr[c] = vx[src[s] + (R_xlen_t)c * rows];
        iib_mix_mean(kind[s], wa[s], wb[s], mm.data(), vr.data(), mm.data(), q);
      }
      for (int c = 0; c < q; ++c) om[who[j] + (R_xlen_t)c * n] = mm[c];
    }
  } else {
    const int total = 2 * size;
    std::vector<double> mm((size_t)total * q, 0.0);
    for (int k = 0; k < K; ++k)
      for (int c = 0; c < q; ++c) mm[(size_t)(size + k) * q + c] = vx[ord[k] + (R_xlen_t)c * rows];
    int s = 0;
    for (int at = size - 1; at >= 1; --at, ++s)
      iib_mix_mean(kind[s], wa[s], wb[s], &mm[(size_t)(2 * at) * q], &mm[(size_t)(2 * at + 1) * q],
                   &mm[(size_t)at * q], q);
    std::vector<double> sm(q);
    int t = 0;
    for (int j = 0; j < S; ++j) {
      std::fill(sm.begin(), sm.end(), 0.0);
      for (; s < end[j]; ++s, ++t)
        iib_mix_mean(kind[s], wa[s], wb[s], sm.data(), &mm[(size_t)node[t] * q], sm.data(), q);
      for (int c = 0; c < q; ++c) om[who[j] + (R_xlen_t)c * n] = sm[c];
    }
  }
  return List::create(_["log_sum"] = outl, _["mean"] = outm);
}
