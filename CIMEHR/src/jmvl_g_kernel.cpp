#include <Rcpp.h>
#include <Rversion.h>
#include <cmath>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize ("fp-contract=off")
#endif

using namespace Rcpp;

// Subject integration using adaptive Gauss-Hermite quadrature.
// Returns NULL for caller handling of unsupported inputs or nonfinite maxima.

static inline double jmvl_exp(double x) {
#if R_VERSION >= R_Version(4, 6, 0)
  if (std::fabs(x) <= 0x1p-26) return x + 1.0;
#endif
  return std::exp(x);
}

static inline double jmvl_pmin(double x, double c) { return x > c ? c : x; }
static inline double jmvl_pmax(double x, double c) { return x < c ? c : x; }
static inline bool jmvl_same(double x, double y) { return x == y && std::signbit(x) == std::signbit(y); }

// [[Rcpp::export(rng = false)]]
SEXP jmvl_g_integrate_cpp(NumericVector r, SEXP yi, SEXP m, NumericVector exposure, NumericVector lr, SEXP k,
                          double su, double sv, double rho, double se, NumericVector gz, NumericVector logw,
                          SEXP details) {
  if (TYPEOF(yi) != INTSXP || TYPEOF(m) != INTSXP || TYPEOF(k) != INTSXP || TYPEOF(details) != LGLSXP ||
      XLENGTH(details) != 1 || LOGICAL(details)[0] == NA_LOGICAL)
    return R_NilValue;
  const bool det = LOGICAL(details)[0];
  const R_xlen_t N = r.size();
  const int n = Rf_length(m), G = gz.size();
  if (n < 2 || G < 2 || exposure.size() != n || lr.size() != n || Rf_length(k) != n || XLENGTH(yi) != N ||
      logw.size() != G || !Rf_isNull(Rf_getAttrib(m, R_NamesSymbol)) ||
      !Rf_isNull(Rf_getAttrib(k, R_NamesSymbol)) || !Rf_isNull(Rf_getAttrib(exposure, R_NamesSymbol)))
    return R_NilValue;
  const int *ys = INTEGER(yi), *ms = INTEGER(m), *ks = INTEGER(k);
  std::vector<double> rsum(n, 0.0), r2(n, 0.0);
  for (R_xlen_t j = 0; j < N;) {
    const int i = ys[j];
    if (i == NA_INTEGER || i < 1 || i > n) return R_NilValue;
    double s1 = rsum[i - 1], s2 = r2[i - 1];
    do {
      s1 += r[j];
      s2 += r[j] * r[j];
    } while (++j < N && ys[j] == i);
    rsum[i - 1] = s1;
    r2[i - 1] = s2;
  }

  const double c = sv * rho / su, t2 = sv * sv * (1 - rho * rho), e2 = se * se;
  const double log2pi = std::log(2 * M_PI), hl2pi = .5 * log2pi, le2 = std::log(e2), lsu = std::log(su);
  const double isu2 = 1 / (su * su), cc = c * c;
  std::vector<double> den(n), yb(n), yconst(n), A(n), B(n), lambda(n), mode(n);
  for (int i = 0; i < n; ++i) {
    const int ki = ks[i];
    if (ki == NA_INTEGER || ki < 0 || ms[i] == NA_INTEGER) return R_NilValue;
    const double kd = ki;
    den[i] = e2 + kd * t2;
    yb[i] = ki > 0 ? rsum[i] / kd : 0.0;
    const double ssec = jmvl_pmax(r2[i] - (ki > 0 ? rsum[i] * rsum[i] / kd : 0.0), 0.0);
    yconst[i] = ki > 0 ? -.5 * (kd * log2pi + (kd - 1) * le2 + std::log(den[i]) + ssec / e2) : 0.0;
    A[i] = (double)ms[i] + kd * c * yb[i] / den[i];
    B[i] = isu2 + kd * cc / den[i];
    lambda[i] = exposure[i] * jmvl_exp(lr[i]);
    mode[i] = (A[i] - lambda[i]) / (B[i] + lambda[i]);
  }

  std::vector<int> act(n), cs(n, 0);
  for (int i = 0; i < n; ++i) act[i] = i;
  std::vector<double> mprev(n), sprev(n), ma(n), mb(n);
  double rmx[2][2] = {{0.0, 0.0}, {0.0, 0.0}};
  int it = 1, na = n;
  for (;; ++it) {
    double mx = 0.0;
    bool nan = false;
    int w = 0;
    for (int t = 0; t < na; ++t) {
      const int i = act[t];
      const double m0 = mode[i];
      const double mu = lambda[i] * jmvl_exp(jmvl_pmin(m0, 700));
      const double st = jmvl_pmax(jmvl_pmin((A[i] - B[i] * m0 - mu) / (B[i] + mu), 5), -5);
      const double m1 = m0 + st, a = std::fabs(st);
      if (std::isnan(a)) nan = true;
      else if (a > mx) mx = a;
      if (jmvl_same(m1, m0)) {
        cs[i] = it;
        ma[i] = mb[i] = m0;
        double *r = rmx[it & 1];
        if (a > r[0]) r[0] = a;
        if (a > r[1]) r[1] = a;
      } else if (it > 1 && jmvl_same(m1, mprev[i])) {
        cs[i] = it - 1;
        ma[i] = mprev[i];
        mb[i] = m0;
        double *r = rmx[(it - 1) & 1];
        const double ap = std::fabs(sprev[i]);
        if (ap > r[0]) r[0] = ap;
        if (a > r[1]) r[1] = a;
      } else act[w++] = i;
      mprev[i] = m0;
      sprev[i] = st;
      mode[i] = m1;
    }
    na = w;
    if (nan) return R_NilValue;
    const int odd = it & 1;
    const double r0 = rmx[0][odd], r1 = rmx[1][1 - odd];
    if (r0 > mx) mx = r0;
    if (r1 > mx) mx = r1;
    if (mx < 2e-12 || it == 100) break;
  }
  if (it == 100) return R_NilValue;
  for (int i = 0; i < n; ++i)
    if (cs[i] > 0) mode[i] = ((it - cs[i]) & 1) ? ma[i] : mb[i];

  NumericVector ll(n), mean_u, second_u, scale;
  if (det) {
    mean_u = NumericVector(n);
    second_u = NumericVector(n);
    scale = NumericVector(n);
  }
  std::vector<double> hz2(G), terms(G), uu(G);
  const double *z = gz.begin(), *lw = logw.begin();
  for (int g = 0; g < G; ++g) hz2[g] = .5 * (z[g] * z[g]);
  for (int i = 0; i < n; ++i) {
    const double s = 1 / std::sqrt(B[i] + lambda[i] * jmvl_exp(mode[i]));
    const double mi = ms[i], hk = .5 * (double)ks[i], mo = mode[i], lri = lr[i], ex = exposure[i], ybi = yb[i],
                 yci = yconst[i], dni = den[i];
    double mxv = 0.0;
    bool seen = false;
    for (int g = 0; g < G; ++g) {
      const double u = mo + s * z[g], lu = lri + u, us = u / su, d = ybi - c * u;
      const double joint =
          mi * lu - ex * jmvl_exp(jmvl_pmin(lu, 700)) - .5 * (us * us) - lsu - hl2pi + yci - hk * (d * d) / dni;
      const double tm = joint + hz2[g] + lw[g];
      uu[g] = u;
      terms[g] = tm;
      if (std::isnan(tm)) return R_NilValue;
      if (!seen || tm > mxv) { mxv = tm; seen = true; }
    }
    if (!std::isfinite(mxv)) return R_NilValue;
    long double tot = 0.0;
    for (int g = 0; g < G; ++g) {
      const double e = jmvl_exp(terms[g] - mxv);
      terms[g] = e;
      tot += e;
    }
    const double totd = (double)tot;
    ll[i] = mxv + std::log(totd) + std::log(s) + hl2pi;
    if (det) {
      long double a1 = 0.0, a2 = 0.0;
      for (int g = 0; g < G; ++g) {
        const double w = terms[g] / totd;
        a1 += w * uu[g];
        a2 += w * (uu[g] * uu[g]);
      }
      mean_u[i] = (double)a1;
      second_u[i] = (double)a2;
      scale[i] = s;
    }
  }

  SEXP nm = Rf_getAttrib(lr, R_NamesSymbol);
  if (!det) {
    if (nm != R_NilValue) ll.attr("names") = nm;
    return ll;
  }
  NumericVector md(mode.begin(), mode.end());
  if (nm != R_NilValue) {
    ll.attr("names") = nm;
    md.attr("names") = nm;
    scale.attr("names") = nm;
  }
  return List::create(_["loglik"] = ll, _["mean_u"] = mean_u, _["second_u"] = second_u, _["mode"] = md,
                      _["scale"] = scale);
}
