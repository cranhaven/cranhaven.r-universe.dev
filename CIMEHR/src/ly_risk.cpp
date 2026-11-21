#include <Rcpp.h>
#include <Rversion.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <numeric>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize ("fp-contract=off")
#endif

using namespace Rcpp;


[[noreturn]] static void ly_risk_na_condition() { stop("missing value where TRUE/FALSE needed"); }

static inline double ly_risk_exp(double x, bool r_dispatch) {
#if R_VERSION >= R_Version(4, 6, 0)
  if (r_dispatch && std::fabs(x) <= 0x1p-26) return x + 1.0;
#endif
  return std::exp(x);
}

// Stable order; NA and NaN last in input order.
static void ly_risk_order(const double *x, int n, bool decreasing, std::vector<int> &o) {
  o.resize(n);
  std::iota(o.begin(), o.end(), 0);
  auto inc = [x](int a, int b) { return !ISNAN(x[a]) && (ISNAN(x[b]) || x[a] < x[b]); };
  auto dec = [x](int a, int b) { return !ISNAN(x[a]) && (ISNAN(x[b]) || x[a] > x[b]); };
  if (decreasing ? std::is_sorted(o.begin(), o.end(), dec) : std::is_sorted(o.begin(), o.end(), inc))
    return;
  std::vector<std::uint64_t> key, key2;
  std::vector<int> idx, idx2;
  key.reserve(n);
  idx.reserve(n);
  for (int i = 0; i < n; ++i) {
    if (ISNAN(x[i])) continue;
    const double v = x[i] == 0.0 ? 0.0 : x[i];
    std::uint64_t u;
    std::memcpy(&u, &v, sizeof u);
    u = (u >> 63) ? ~u : (u | 0x8000000000000000ULL);
    key.push_back(decreasing ? ~u : u);
    idx.push_back(i);
  }
  const int m = key.size();
  key2.resize(m);
  idx2.resize(m);
  std::vector<int> count(1 << 11);
  for (int shift = 0; shift < 64; shift += 11) {
    std::fill(count.begin(), count.end(), 0);
    for (int i = 0; i < m; ++i) ++count[(key[i] >> shift) & 2047];
    if (m && count[(key[0] >> shift) & 2047] == m) continue;
    for (int b = 0, s = 0; b < 2048; ++b) {
      const int c = count[b];
      count[b] = s;
      s += c;
    }
    for (int i = 0; i < m; ++i) {
      const int to = count[(key[i] >> shift) & 2047]++;
      key2[to] = key[i];
      idx2[to] = idx[i];
    }
    key.swap(key2);
    idx.swap(idx2);
  }
  std::copy(idx.begin(), idx.end(), o.begin());
  for (int i = 0, at = m; i < n; ++i)
    if (ISNAN(x[i])) o[at++] = i;
}

// Returns 1 for all-equal values, 0 for unequal values, or -1 for an NA result.
static int ly_risk_common(const NumericVector &x) {
  const R_xlen_t n = x.size();
  bool na = false;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (ISNAN(x[i]) || ISNAN(x[0])) na = true;
    else if (x[i] != x[0]) return 0;
  }
  return na ? -1 : 1;
}

// co == nullptr skips the covariance.
static inline void ly_risk_mix(double la, const double *ma, const double *ca, double lb,
                               const double *mb, const double *cb, double &lo, double *mo,
                               double *co, int q, double *d, bool rx) {
  const int qq = q * q;
  if (ISNAN(la)) ly_risk_na_condition();
  if (la == R_NegInf) {
    lo = lb;
    if (mo != mb) std::copy(mb, mb + q, mo);
    if (co && co != cb) std::copy(cb, cb + qq, co);
    return;
  }
  if (ISNAN(lb)) ly_risk_na_condition();
  if (lb == R_NegInf) {
    lo = la;
    if (mo != ma) std::copy(ma, ma + q, mo);
    if (co && co != ca) std::copy(ca, ca + qq, co);
    return;
  }
  const double z = std::max(la, lb);
  const double ea = la == z ? 1.0 : ly_risk_exp(la - z, rx);
  const double eb = lb == z ? 1.0 : ly_risk_exp(lb - z, rx);
  const double ll = z + std::log(ea + eb);
  const double wa = ly_risk_exp(la - ll, rx), wb = ly_risk_exp(lb - ll, rx);
  if (co) {
    const double wab = wa * wb;
    for (int i = 0; i < q; ++i) d[i] = mb[i] - ma[i];
    for (int j = 0; j < q; ++j)
      for (int i = 0; i < q; ++i) {
        const int ij = i + j * q;
        co[ij] = (wa * ca[ij] + wb * cb[ij]) + wab * (0.0 + d[i] * d[j]);
      }
  }
  for (int i = 0; i < q; ++i) mo[i] = wa * ma[i] + wb * mb[i];
  lo = ll;
}

// Risk-set traversal plan:
// status: 0 = run, 1 = NA condition, 2 = no times.
// ord = order(times).
// Sweep: oc = order(censor, decreasing = TRUE); stop[k] counts oc entries at time k.
// keep[k] indicates whether time k is stored.
// Tree: chg encodes leaf changes as 2 * j + active; chg_at and node_at delimit
// leaf changes and recomputed nodes for each time.
// [[Rcpp::export(rng = false)]]
List ly_risk_plan_cpp(NumericVector times, NumericVector entry, NumericVector censor) {
  const int K = times.size(), n = entry.size();
  if (censor.size() != n) stop("length(censor) must equal length(entry)");
  std::vector<int> ord;
  ly_risk_order(times.begin(), K, false, ord);
  List plan = List::create(_["K"] = K, _["n"] = n, _["status"] = 0,
                           _["ord"] = IntegerVector(ord.begin(), ord.end()));
  if (!K) {
    plan["status"] = 2;
    return plan;
  }
  const int common = ly_risk_common(entry);
  if (common < 0) {
    plan["status"] = 1;
    return plan;
  }
  plan["common"] = common == 1;
  std::vector<double> tt(K);
  for (int k = 0; k < K; ++k) tt[k] = times[ord[k]];
  const double *ce = censor.begin(), *en = entry.begin();
  std::vector<int> oc, oe;
  if (common) {
    ly_risk_order(ce, n, true, oc);
    const double e0 = n ? en[0] : NA_REAL;
    IntegerVector stop_at(K), keep(K);
    int pc = 0;
    for (int k = K - 1; k >= 0; --k) {
      const double t = tt[k];
      while (pc < n) {
        const int j = oc[pc];
        if (ISNAN(ce[j]) || ISNAN(t)) {
          plan["status"] = 1;
          return plan;
        }
        if (!(ce[j] >= t)) break;
        ++pc;
      }
      stop_at[k] = pc;
      if (ISNAN(t) || ISNAN(e0)) {
        plan["status"] = 1;
        return plan;
      }
      keep[k] = t > e0;
    }
    plan["oc"] = IntegerVector(oc.begin(), oc.end());
    plan["stop"] = stop_at;
    plan["keep"] = keep;
    return plan;
  }
  int size = 1;
  while (size < n) size <<= 1;
  ly_risk_order(en, n, false, oe);
  ly_risk_order(ce, n, false, oc);
  std::vector<int> chg, nodes, cur, up;
  IntegerVector chg_at(K + 1), node_at(K + 1);
  int pe = 0, pc = 0;
  for (int k = 0; k < K; ++k) {
    const double t = tt[k];
    while (pe < n) {
      const int j = oe[pe];
      if (ISNAN(en[j]) || ISNAN(t)) {
        plan["status"] = 1;
        return plan;
      }
      if (!(en[j] < t)) break;
      chg.push_back(2 * j + 1);
      cur.push_back(size + j);
      ++pe;
    }
    while (pc < n) {
      const int j = oc[pc];
      if (ISNAN(ce[j]) || ISNAN(t)) {
        plan["status"] = 1;
        return plan;
      }
      if (!(ce[j] < t)) break;
      chg.push_back(2 * j);
      cur.push_back(size + j);
      ++pc;
    }
    if (!cur.empty()) {
      std::sort(cur.begin(), cur.end());
      while (cur[0] > 1) {
        up.clear();
        for (int at : cur) {
          const int parent = at / 2;
          if (up.empty() || up.back() != parent) up.push_back(parent);
        }
        nodes.insert(nodes.end(), up.begin(), up.end());
        cur.swap(up);
      }
      cur.clear();
    }
    chg_at[k + 1] = chg.size();
    node_at[k + 1] = nodes.size();
  }
  plan["size"] = size;
  plan["chg"] = IntegerVector(chg.begin(), chg.end());
  plan["chg_at"] = chg_at;
  plan["nodes"] = IntegerVector(nodes.begin(), nodes.end());
  plan["node_at"] = node_at;
  return plan;
}

// [[Rcpp::export(rng = false)]]
List ly_risk_moments_cpp(NumericVector log_weight, NumericMatrix values, NumericVector times,
                         NumericVector entry, NumericVector censor, bool covariance,
                         Nullable<List> plan = R_NilValue) {
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
  const List pl = plan.isNull() ? ly_risk_plan_cpp(times, entry, censor) : List(plan.get());
  if (as<int>(pl["K"]) != K || as<int>(pl["n"]) != n) stop("risk plan does not match the inputs");
  const IntegerVector ord = pl["ord"];
  const int status = as<int>(pl["status"]);
  NumericVector logs(K, R_NegInf);
  NumericMatrix means(K, q);
  std::fill(means.begin(), means.end(), NA_REAL);
  NumericVector covs(covariance ? (R_xlen_t)qq * K : 0);
  List out = List::create(_["log_sum"] = logs, _["mean"] = means);
  if (covariance) {
    covs.attr("dim") = IntegerVector::create(q, q, K);
    out["covariance"] = covs;
  }
  if (status == 2) return out;
  if (status == 1) ly_risk_na_condition();
  const double *lw = log_weight.begin(), *vx = values.begin();
  double *lg = logs.begin(), *mn = means.begin(), *cv = covariance ? covs.begin() : nullptr;
  auto store = [&](int k, double l, const double *m, const double *c) {
    const int pos = ord[k];
    lg[pos] = l;
    for (int a = 0; a < q; ++a) mn[pos + (R_xlen_t)a * K] = m[a];
    if (cv) std::copy(c, c + qq, cv + (R_xlen_t)pos * qq);
  };
  std::vector<double> d(q), zero(qq, 0.0);
  if (as<bool>(pl["common"])) {
    const IntegerVector oc = pl["oc"], stop_at = pl["stop"], keep = pl["keep"];
    const bool named = !Rf_isNull(Rf_getAttrib(log_weight, R_NamesSymbol));
    double ll = R_NegInf;
    std::vector<double> mm(q, 0.0), cc(qq, 0.0), vj(q);
    double *cp = covariance ? cc.data() : nullptr;
    int pc = 0;
    for (int k = K - 1; k >= 0; --k) {
      for (; pc < stop_at[k]; ++pc) {
        const int j = oc[pc];
        for (int a = 0; a < q; ++a) vj[a] = vx[j + (R_xlen_t)a * n];
        ly_risk_mix(ll, mm.data(), cp, lw[j], vj.data(), zero.data(), ll, mm.data(), cp, q,
                    d.data(), named);
      }
      if (keep[k]) store(k, ll, mm.data(), cp);
    }
  } else {
    const int size = as<int>(pl["size"]), total = 2 * size, cq = covariance ? qq : 0;
    const IntegerVector chg = pl["chg"], chg_at = pl["chg_at"], nodes = pl["nodes"],
                        node_at = pl["node_at"];
    std::vector<double> ll(total, R_NegInf), mm((size_t)total * q, 0.0), cc((size_t)total * cq, 0.0);
    auto node_cov = [&](int at) { return cq ? &cc[(size_t)at * cq] : nullptr; };
    for (int k = 0; k < K; ++k) {
      for (int c = chg_at[k]; c < chg_at[k + 1]; ++c) {
        const int j = chg[c] / 2, at = size + j;
        const bool active = chg[c] % 2;
        ll[at] = active ? lw[j] : R_NegInf;
        for (int a = 0; a < q; ++a) mm[(size_t)at * q + a] = active ? vx[j + (R_xlen_t)a * n] : 0.0;
        if (cq) std::fill(cc.begin() + (size_t)at * cq, cc.begin() + (size_t)(at + 1) * cq, 0.0);
      }
      for (int c = node_at[k]; c < node_at[k + 1]; ++c) {
        const int at = nodes[c], a = 2 * at, b = a + 1;
        ly_risk_mix(ll[a], &mm[(size_t)a * q], node_cov(a), ll[b], &mm[(size_t)b * q], node_cov(b),
                    ll[at], &mm[(size_t)at * q], node_cov(at), q, d.data(), false);
      }
      store(k, ll[1], &mm[q], node_cov(1));
    }
  }
  return out;
}
