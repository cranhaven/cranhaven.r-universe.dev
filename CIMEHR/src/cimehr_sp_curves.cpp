#include <Rcpp.h>
#include <cfloat>
#include <cmath>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize ("fp-contract=off")
#endif

using namespace Rcpp;

static inline double cimehr_sp_madd(double a, double b, double c, bool fused) {
  return fused ? std::fma(a, b, c) : c + a * b;
}

static bool cimehr_sp_maybe_nonfinite(const std::vector<double> &x) {
  const size_t n = x.size();
  if ((n & 1) != 0 && !R_FINITE(x[0])) return true;
  for (size_t i = n & 1; i < n; i += 2)
    if (!R_FINITE(x[i] + x[i + 1])) return true;
  return false;
}

// Returns 0 with the result in *fit, or 1 for caller handling.
static int cimehr_sp_local_linear_one(const double * /* t */, const double *dA, const double *dL,
    const std::vector<R_xlen_t> &win, const std::vector<double> &u, bool fused, double *fit) {
  const size_t w = win.size();
  std::vector<double> kd(w);
  long double s = 0.0;
  for (size_t l = 0; l < w; ++l) {
    const double K = 0.75 * (1.0 - u[l] * u[l]);
    kd[l] = K * dL[win[l]];
    s += kd[l];
  }
  const double scale = s > DBL_MAX ? R_PosInf : (s < -DBL_MAX ? R_NegInf : (double) s);
  if (!R_FINITE(scale) || scale <= 0) { *fit = NA_REAL; return 0; }
  double m11 = 0.0, m21 = 0.0, m12 = 0.0, m22 = 0.0;
  for (size_t l = 0; l < w; ++l) {
    const double wl = kd[l] / scale, uw = u[l] * wl;
    m11 = cimehr_sp_madd(1.0, wl, m11, fused);
    m21 = cimehr_sp_madd(u[l], wl, m21, fused);
    m12 = cimehr_sp_madd(1.0, uw, m12, fused);
    m22 = cimehr_sp_madd(u[l], uw, m22, fused);
  }
  if (!R_FINITE(m11) || !R_FINITE(m21) || !R_FINITE(m12) || !R_FINITE(m22)) { *fit = NA_REAL; return 0; }
  const double big = 1e300, tiny = 1e-300;
  if (std::fabs(m11) > big || std::fabs(m21) > big || std::fabs(m12) > big || std::fabs(m22) > big) return 1;
  const double p = m12 * m21, det = std::fma(m11, m22, -p) + std::fma(-m12, m21, p);
  const double anorm = std::max(std::fabs(m11) + std::fabs(m21), std::fabs(m12) + std::fabs(m22));
  const double inorm = std::max(std::fabs(m22) + std::fabs(m21), std::fabs(m12) + std::fabs(m11));
  if (!(std::fabs(det) > 1.01e-10 * anorm * inorm)) return 1;
  std::vector<double> v(w);
  for (size_t l = 0; l < w; ++l) v[l] = 0.75 * (1.0 - u[l] * u[l]) * dA[win[l]] / scale;
  if (cimehr_sp_maybe_nonfinite(v)) return 1;
  double b1 = 0.0, b2 = 0.0;
  for (size_t l = 0; l < w; ++l) {
    b1 = cimehr_sp_madd(1.0, v[l], b1, fused);
    b2 = cimehr_sp_madd(u[l], v[l], b2, fused);
  }
  if (!R_FINITE(b1) || !R_FINITE(b2)) return 1;
  double a11 = m11, a21 = m21, a12 = m12, a22 = m22;
  const bool swap = std::fabs(a21) > std::fabs(a11);
  if (swap) { std::swap(a11, a21); std::swap(a12, a22); std::swap(b1, b2); }
  if (std::fabs(a11) < tiny) return 1;
  const double l21 = (1.0 / a11) * a21;
  const double u22 = cimehr_sp_madd(-a12, l21, a22, fused);
  if (b1 != 0) b2 = cimehr_sp_madd(-b1, l21, b2, fused);
  if (b2 != 0) {
    b2 = b2 / u22;
    b1 = cimehr_sp_madd(-b2, a12, b1, fused);
  }
  if (b1 != 0) b1 = b1 / a11;
  *fit = b1;
  return 0;
}

// Inputs: finite t, dA and dL, with dL > 0.
// Window: {i : |(t[i] - g)/bandwidth| < 1}.
// [[Rcpp::export(rng = false)]]
List cimehr_sp_local_linear(NumericVector t, NumericVector dA, NumericVector dL,
    NumericVector times, double bandwidth, bool fused) {
  const R_xlen_t K = t.size(), T = times.size();
  if (dA.size() != K || dL.size() != K) stop("Baseline increments must have matching lengths.");
  const double *pt = t.begin(), *pa = dA.begin(), *pl = dL.begin();
  bool sorted = true;
  for (R_xlen_t i = 1; i < K && sorted; ++i) if (pt[i] < pt[i - 1]) sorted = false;
  std::vector<R_xlen_t> run;
  if (sorted) {
    run.resize(K);
    for (R_xlen_t i = 0; i < K; ++i) run[i] = i == 0 ? 0 : run[i - 1] + (pt[i] != pt[i - 1]);
  }
  NumericVector fit(T, NA_REAL);
  LogicalVector defer(T);
  std::vector<R_xlen_t> win;
  std::vector<double> u;
  for (R_xlen_t g = 0; g < T; ++g) {
    const double c = times[g];
    win.clear(); u.clear();
    if (sorted) {
      R_xlen_t lo = 0, hi = K;
      while (lo < hi) { R_xlen_t m = lo + (hi - lo) / 2; if ((pt[m] - c) / bandwidth > -1) hi = m; else lo = m + 1; }
      const R_xlen_t first = lo;
      hi = K;
      while (lo < hi) { R_xlen_t m = lo + (hi - lo) / 2; if ((pt[m] - c) / bandwidth >= 1) hi = m; else lo = m + 1; }
      const R_xlen_t last = lo;
      if (last - first < 3 || run[last - 1] - run[first] < 2) continue;
      for (R_xlen_t i = first; i < last; ++i) { win.push_back(i); u.push_back((pt[i] - c) / bandwidth); }
    } else {
      for (R_xlen_t i = 0; i < K; ++i) {
        const double ui = (pt[i] - c) / bandwidth;
        if (std::fabs(ui) < 1) { win.push_back(i); u.push_back(ui); }
      }
      int distinct = 0;
      double v1 = 0, v2 = 0;
      for (size_t l = 0; l < win.size() && distinct < 3; ++l) {
        const double x = pt[win[l]];
        if (distinct == 0) { v1 = x; distinct = 1; }
        else if (distinct == 1) { if (x != v1) { v2 = x; distinct = 2; } }
        else if (x != v1 && x != v2) distinct = 3;
      }
      if (distinct < 3) continue;
    }
    double value;
    if (cimehr_sp_local_linear_one(pt, pa, pl, win, u, fused, &value)) defer[g] = true;
    else fit[g] = value;
  }
  return List::create(_["fit"] = fit, _["defer"] = defer);
}

// Column-wise sd of finite entries; NA with fewer than two entries.
// mode: 0 long-double products; 1 double products with FMA on the final n % 8 entries.
// [[Rcpp::export(rng = false)]]
NumericVector cimehr_sp_column_sd(NumericMatrix draws, int mode) {
  const R_xlen_t B = draws.nrow();
  const R_xlen_t P = draws.ncol();
  NumericVector out(P);
  std::vector<double> z;
  z.reserve(B);
  for (R_xlen_t j = 0; j < P; ++j) {
    const double *x = draws.begin() + j * B;
    z.clear();
    for (R_xlen_t i = 0; i < B; ++i) if (R_FINITE(x[i])) z.push_back(x[i]);
    const R_xlen_t n = z.size();
    if (n < 2) { out[j] = NA_REAL; continue; }
    long double sum = 0.0, tmp;
    for (R_xlen_t k = 0; k < n; ++k) sum += z[k];
    tmp = sum / n;
    if (R_FINITE((double) tmp)) {
      sum = 0.0;
      for (R_xlen_t k = 0; k < n; ++k) sum += (z[k] - tmp);
      tmp = tmp + sum / n;
    }
    const long double xxm = (double) tmp;
    sum = 0.0;
    if (mode == 1) {
      const R_xlen_t n8 = n - n % 8;
      double acc = 0.0;
      for (R_xlen_t k = 0; k < n8; ++k) { const double a = z[k] - (double) xxm, pr = a * a; acc = acc + pr; }
      for (R_xlen_t k = n8; k < n; ++k) { const double a = z[k] - (double) xxm; acc = std::fma(a, a, acc); }
      sum = acc;
    } else {
      for (R_xlen_t k = 0; k < n; ++k) sum += (z[k] - xxm) * (z[k] - xxm);
    }
    out[j] = std::sqrt((double) (sum / (n - 1)));
  }
  return out;
}
