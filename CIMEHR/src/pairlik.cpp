#include <Rcpp.h>
#include <Rmath.h>
#include <cmath>
#include <limits>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;

typedef long double accumulator;

// Rows are packed by subject as (y, x_1, ..., x_p).
struct PairData {
  std::vector<double> rec;
  const int *off;
  const int *P1;
  const int *P2;
  int p;
  int w;
  int K;
  PairData(const NumericVector &y_, const NumericMatrix &X_, const IntegerVector &rows_,
           const IntegerVector &off_, const IntegerVector &P1_, const IntegerVector &P2_)
    : off(off_.begin()), P1(P1_.begin()), P2(P2_.begin()), p(X_.ncol()), w(X_.ncol() + 1), K(P1_.size()) {
    const int N = X_.nrow(), R = rows_.size();
    const double *y = y_.begin(), *X = X_.begin();
    rec.resize((size_t)R * w);
    for (int r = 0; r < R; ++r) {
      const int a = rows_[r];
      double *q = &rec[(size_t)r * w];
      q[0] = y[a];
      for (int c = 0; c < p; ++c) q[1 + c] = X[a + (size_t)c * N];
    }
  }
  inline const double *row(int i) const { return rec.data() + (size_t)i * w; }
};

template <int P>
static inline double pair_eta(const double *a, const double *b, const double *theta, double *z, int p) {
  const int np = P > 0 ? P : p;
  const double dy = a[0] - b[0];
  double e = 0.0;
  for (int c = 0; c < np; ++c) {
    const double dx = a[1 + c] - b[1 + c];
    z[c] = dy * dx;
    e = std::fma(theta[c], z[c], e);
  }
  return e;
}

static inline double pair_plogis(double x) {
  if (ISNAN(x)) return x + 0.0 + 1.0;
  if (!R_FINITE(x)) return x > 0 ? 1.0 : 0.0;
  return 1 / (1 + std::exp(-x));
}

static const double pair_big = (std::numeric_limits<accumulator>::digits + 2) * M_LN2;

static const int pair_block = 256;

static void pair_flush(const double *xb, double *lb, int nb, accumulator &s) {
  for (int i = 0; i < nb; ++i) {
    const double ax = std::fabs(xb[i]);
    lb[i] = ax >= pair_big ? 0.0 : std::exp(-ax);
  }
  for (int i = 0; i < nb; ++i)
    if (std::fabs(xb[i]) < pair_big) lb[i] = std::log1p(lb[i]);
  for (int i = 0; i < nb; ++i) {
    const double x = xb[i];
    if (std::fabs(x) >= pair_big) {
      if (x > 0.0) { s += x; continue; }
      if (s >= 1.0) continue;
      lb[i] = std::log1p(std::exp(-std::fabs(x)));
    }
    const double m = x > 0.0 ? x : 0.0;
    s += m + lb[i];
  }
}

template <int P>
static double pair_objective(const PairData &d, const double *theta) {
  double z[P > 0 ? P : 1];
  std::vector<double> zv(P > 0 ? 0 : d.p);
  double *zp = P > 0 ? z : zv.data();
  double xb[pair_block], lb[pair_block];
  int nb = 0;
  accumulator s = 0.0;
  for (int k = 0; k < d.K; ++k) {
    const int s1 = d.P1[k] - 1, s2 = d.P2[k] - 1;
    const int b0 = d.off[s2], b1 = d.off[s2 + 1];
    for (int ia = d.off[s1]; ia < d.off[s1 + 1]; ++ia) {
      const double *ra = d.row(ia);
      for (int ib = b0; ib < b1; ++ib) {
        xb[nb++] = -pair_eta<P>(ra, d.row(ib), theta, zp, d.p);
        if (nb == pair_block) { pair_flush(xb, lb, nb, s); nb = 0; }
      }
    }
  }
  pair_flush(xb, lb, nb, s);
  return (double)s;
}

template <int P>
static void pair_gradient(const PairData &d, const double *theta, accumulator *g) {
  double z[P > 0 ? P : 1];
  std::vector<double> zv(P > 0 ? 0 : d.p);
  double *zp = P > 0 ? z : zv.data();
  const int np = P > 0 ? P : d.p;
  for (int k = 0; k < d.K; ++k) {
    const int s1 = d.P1[k] - 1, s2 = d.P2[k] - 1;
    const int b0 = d.off[s2], b1 = d.off[s2 + 1];
    for (int ia = d.off[s1]; ia < d.off[s1 + 1]; ++ia) {
      const double *ra = d.row(ia);
      for (int ib = b0; ib < b1; ++ib) {
        const double e = pair_eta<P>(ra, d.row(ib), theta, zp, d.p);
        const double q = 1.0 - pair_plogis(e);
        for (int c = 0; c < np; ++c) {
          const double t = zp[c] * q;
          g[c] += t;
        }
      }
    }
  }
}

template <int P>
static void pair_hessian(const PairData &d, const double *theta, double *H, double *Sp) {
  double z[P > 0 ? P : 1], bz[P > 0 ? P : 1], sk[P > 0 ? P : 1];
  std::vector<double> zv(P > 0 ? 0 : 3 * d.p);
  double *zp = P > 0 ? z : zv.data(), *bp = P > 0 ? bz : zv.data() + d.p, *sp = P > 0 ? sk : zv.data() + 2 * d.p;
  const int np = P > 0 ? P : d.p, K = d.K;
  for (int k = 0; k < K; ++k) {
    const int s1 = d.P1[k] - 1, s2 = d.P2[k] - 1;
    const int b0 = d.off[s2], b1 = d.off[s2 + 1];
    for (int c = 0; c < np; ++c) sp[c] = 0.0;
    for (int ia = d.off[s1]; ia < d.off[s1 + 1]; ++ia) {
      const double *ra = d.row(ia);
      for (int ib = b0; ib < b1; ++ib) {
        const double e = pair_eta<P>(ra, d.row(ib), theta, zp, d.p);
        const double pp = pair_plogis(e);
        const double q = 1.0 - pp;
        const double w = pp * q;
        for (int j = 0; j < np; ++j) bp[j] = zp[j] * w;
        for (int j = 0; j < np; ++j)
          for (int i = 0; i < np; ++i) H[i + j * np] = std::fma(zp[i], bp[j], H[i + j * np]);
        if (Sp)
          for (int c = 0; c < np; ++c) {
            const double t = zp[c] * q;
            sp[c] += t;
          }
      }
    }
    if (Sp)
      for (int c = 0; c < np; ++c) Sp[k + (size_t)c * K] = sp[c];
  }
}

#define PAIR_DISPATCH(F, ...)                         \
  switch (d.p) {                                      \
  case 1: F<1>(__VA_ARGS__); break;                   \
  case 2: F<2>(__VA_ARGS__); break;                   \
  case 3: F<3>(__VA_ARGS__); break;                   \
  case 4: F<4>(__VA_ARGS__); break;                   \
  case 5: F<5>(__VA_ARGS__); break;                   \
  case 6: F<6>(__VA_ARGS__); break;                   \
  default: F<0>(__VA_ARGS__); break;                  \
  }

// [[Rcpp::export(rng = false)]]
double pairlik_n_cross(IntegerVector off, IntegerVector P1, IntegerVector P2) {
  double total = 0.0;
  for (R_xlen_t k = 0; k < P1.size(); ++k) {
    const int s1 = P1[k] - 1, s2 = P2[k] - 1;
    total += (double)(off[s1 + 1] - off[s1]) * (double)(off[s2 + 1] - off[s2]);
  }
  return total;
}

// [[Rcpp::export(rng = false)]]
double pairlik_objective(NumericVector theta, NumericVector y, NumericMatrix X, IntegerVector rows,
                         IntegerVector off, IntegerVector P1, IntegerVector P2) {
  PairData d(y, X, rows, off, P1, P2);
  double s = 0.0;
  PAIR_DISPATCH(s = pair_objective, d, theta.begin())
  return s;
}

// [[Rcpp::export(rng = false)]]
NumericVector pairlik_gradient(NumericVector theta, NumericVector y, NumericMatrix X, IntegerVector rows,
                               IntegerVector off, IntegerVector P1, IntegerVector P2) {
  PairData d(y, X, rows, off, P1, P2);
  std::vector<accumulator> g(d.p, 0.0);
  PAIR_DISPATCH(pair_gradient, d, theta.begin(), g.data())
  NumericVector out(d.p);
  for (int c = 0; c < d.p; ++c) out[c] = -(double)g[c];
  return out;
}

// [[Rcpp::export(rng = false)]]
List pairlik_hessian(NumericVector theta, NumericVector y, NumericMatrix X, IntegerVector rows,
                     IntegerVector off, IntegerVector P1, IntegerVector P2, bool scores, int nsub) {
  PairData d(y, X, rows, off, P1, P2);
  const int p = d.p, K = d.K;
  NumericMatrix H(p, p);
  NumericMatrix Sp(scores ? K : 0, p);
  PAIR_DISPATCH(pair_hessian, d, theta.begin(), H.begin(), scores ? Sp.begin() : (double *)NULL)
  if (!scores) return List::create(_["H"] = H);
  NumericMatrix hsub(nsub, p);
  for (int k = 0; k < K; ++k) {
    const int a = d.P1[k] - 1, b = d.P2[k] - 1;
    for (int c = 0; c < p; ++c) hsub(a, c) = hsub(a, c) + Sp(k, c);
    for (int c = 0; c < p; ++c) hsub(b, c) = hsub(b, c) + Sp(k, c);
  }
  return List::create(_["H"] = H, _["Sp"] = Sp, _["hsub"] = hsub);
}
