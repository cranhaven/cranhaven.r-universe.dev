#include <Rcpp.h>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;


static double plm_real_mean(const double *x, int n) {
  long double s = 0.0;
  for (int k = 0; k < n; ++k) s += x[k];
  const bool finite_s = R_FINITE((double)s);
  if (finite_s) {
    s /= n;
  } else {
    s = 0.0;
    for (int k = 0; k < n; ++k) s += x[k] / n;
  }
  if (finite_s && R_FINITE((double)s)) {
    long double t = 0.0;
    for (int k = 0; k < n; ++k) t += (x[k] - s);
    s += t / n;
  } else if (R_FINITE((double)s)) {
    long double t = 0.0;
    for (int k = 0; k < n; ++k) t += (x[k] - s) / n;
    s += t;
  }
  return (double)s;
}

static double plm_int_mean(const double *x, int n) {
  long double s = 0.0;
  for (int k = 0; k < n; ++k) s += x[k];
  return (double)(s / n);
}

static int plm_rcmp(double x, double y) {
  const bool nax = ISNAN(x), nay = ISNAN(y);
  if (nax && nay) return 0;
  if (nax) return 1;
  if (nay) return -1;
  if (x < y) return -1;
  if (x > y) return 1;
  return 0;
}

static void plm_psort(double *x, int lo, int hi, int k) {
  double v, w;
  int L, R, i, j;
  for (L = lo, R = hi; L < R;) {
    v = x[k];
    for (i = L, j = R; i <= j;) {
      while (plm_rcmp(x[i], v) < 0) i++;
      while (plm_rcmp(v, x[j]) < 0) j--;
      if (i <= j) { w = x[i]; x[i++] = x[j]; x[j--] = w; }
    }
    if (j < k) L = i;
    if (k < i) R = j;
  }
}

static void plm_psort0(double *x, int lo, int hi, const int *ind, int k) {
  if (k < 1 || hi - lo < 1) return;
  if (k <= 1) {
    plm_psort(x, lo, hi, ind[0] - 1);
  } else {
    int This = 0;
    const int mid = (lo + hi) / 2;
    for (int i = 0; i < k; i++) if (ind[i] - 1 <= mid) This = i;
    const int z = ind[This] - 1;
    plm_psort(x, lo, hi, z);
    plm_psort0(x, lo, z - 1, ind, This);
    plm_psort0(x, z + 1, hi, ind + This + 1, k - This - 1);
  }
}

static double plm_stat(double *x, int n, int stat, bool integer) {
  if (stat == 0) return integer ? plm_int_mean(x, n) : plm_real_mean(x, n);
  if (stat == 2 || stat == 3) {
    double s = x[0];
    for (int k = 1; k < n; ++k)
      if (stat == 2 ? x[k] < s : x[k] > s) s = x[k];
    return s;
  }
  const int half = (n + 1) / 2;
  if (n % 2 == 1) {
    plm_psort0(x, 0, n - 1, &half, 1);
    return x[half - 1];
  }
  const int ind[2] = {half, half + 1};
  plm_psort0(x, 0, n - 1, ind, 2);
  const double mid[2] = {x[half - 1], x[half]};
  return integer ? plm_int_mean(mid, 2) : plm_real_mean(mid, 2);
}

// Rows grouped by code (1..G) in input order; keep() selects included rows.
template <typename T, typename F>
static void plm_bucket(const T *x, const int *code, int n, int G, F keep, std::vector<int> &start, std::vector<double> &buf) {
  start.assign(G + 1, 0);
  for (int i = 0; i < n; ++i) if (keep(x[i])) start[code[i]]++;
  for (int g = 0; g < G; ++g) start[g + 1] += start[g];
  std::vector<int> pos(start.begin(), start.end() - 1);
  buf.resize(start[G]);
  for (int i = 0; i < n; ++i) if (keep(x[i])) buf[pos[code[i] - 1]++] = (double)x[i];
}

// stat: 0 mean, 1 median, 2 min, 3 max of y; columns of covs get the mean of
// their finite values (NA when a subject has none).
// [[Rcpp::export(rng = false)]]
NumericMatrix plm_subject_summary(SEXP y, List covs, IntegerVector code, int G, int stat) {
  const int n = code.size(), nc = covs.size();
  const int *cd = code.begin();
  for (int i = 0; i < n; ++i)
    if (cd[i] < 1 || cd[i] > G) stop("internal summary group code out of range");
  NumericMatrix out(G, nc + 1);
  std::vector<int> start;
  std::vector<double> buf;
  for (int j = -1; j < nc; ++j) {
    SEXP v = j < 0 ? y : VECTOR_ELT(covs, j);
    if (Rf_xlength(v) != n) stop("internal summary length mismatch");
    const bool integer = TYPEOF(v) != REALSXP;
    if (integer) {
      if (TYPEOF(v) != INTSXP) stop("internal summary type mismatch");
      const int *x = INTEGER(v);
      if (j < 0) plm_bucket(x, cd, n, G, [](int) { return true; }, start, buf);
      else plm_bucket(x, cd, n, G, [](int a) { return a != NA_INTEGER; }, start, buf);
    } else {
      const double *x = REAL(v);
      if (j < 0) plm_bucket(x, cd, n, G, [](double) { return true; }, start, buf);
      else plm_bucket(x, cd, n, G, [](double a) { return (bool)R_FINITE(a); }, start, buf);
    }
    double *o = out.begin() + (size_t)(j + 1) * G;
    for (int g = 0; g < G; ++g) {
      const int m = start[g + 1] - start[g];
      o[g] = m ? plm_stat(buf.data() + start[g], m, j < 0 ? stat : 0, integer) : NA_REAL;
    }
  }
  return out;
}
