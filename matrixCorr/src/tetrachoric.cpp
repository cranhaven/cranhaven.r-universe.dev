// Thiago de Paula Oliveira
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <algorithm>
#include <functional>
#include <vector>

#include "matrixCorr_detail.h"

// ===== Imports from matrixCorr_detail =====

// clamp policy
using matrixCorr_detail::clamp_policy::nan_preserve;

// standard normal helpers
using matrixCorr_detail::norm1::Phi;
using matrixCorr_detail::norm1::phi;
using matrixCorr_detail::norm1::qnorm01;

// Brent 1-D optimiser
using matrixCorr_detail::brent::optimize;

// Bivariate normal CDF utilities
using matrixCorr_detail::bvn_adaptive::Phi2;
using matrixCorr_detail::bvn_adaptive::rect_prob;

using namespace Rcpp;

// ---------- internal helpers (TU-agnostic; no policy aliases) ----------
static inline double tetra_loglik(double rho, double a, double b, double c, double d){
  using matrixCorr_detail::norm1::Phi;
  using matrixCorr_detail::norm1::qnorm01;
  using matrixCorr_detail::bvn_adaptive::Phi2;

  const double N   = a + b + c + d;
  const double eps = 1e-12;

  const double p_row1 = matrixCorr_detail::clamp_policy::nan_preserve((a + b) / N, eps, 1.0 - eps);
  const double p_col1 = matrixCorr_detail::clamp_policy::nan_preserve((a + c) / N, eps, 1.0 - eps);
  const double q1 = qnorm01(p_row1);
  const double q2 = qnorm01(p_col1);

  const double p11 = std::max(rect_prob(-INFINITY, q1, -INFINITY, q2, rho), 1e-16);
  const double p1_ = Phi(q1);
  const double _1p = Phi(q2);
  const double p10 = std::max(p1_ - p11, 1e-16);
  const double p01 = std::max(_1p - p11, 1e-16);
  const double p00 = std::max(1.0 - p1_ - _1p + p11, 1e-16);

  return a*std::log(p11) + b*std::log(p10) + c*std::log(p01) + d*std::log(p00);
}

static inline void build_cutpoints(const NumericMatrix& N,
                                   std::vector<double>& alpha,
                                   std::vector<double>& beta)
{
  using matrixCorr_detail::norm1::qnorm01;

  const int R = N.nrow(), C = N.ncol();
  NumericVector rowp(R), colp(C);
  double tot = 0.0;
  for (int i=0;i<R;++i)
    for (int j=0;j<C;++j){
      rowp[i] += N(i,j);
      colp[j] += N(i,j);
      tot     += N(i,j);
    }
    if (tot <= 0) stop("Empty table");

    rowp = rowp / tot; colp = colp / tot;

    const double eps = 1e-12;
    NumericVector rowcum(R-1), colcum(C-1);
    double acc = 0.0;
    for (int i=0;i<R-1;++i){ acc += rowp[i]; rowcum[i] = matrixCorr_detail::clamp_policy::nan_preserve(acc, eps, 1.0 - eps); }
    acc = 0.0;
    for (int j=0;j<C-1;++j){ acc += colp[j]; colcum[j] = matrixCorr_detail::clamp_policy::nan_preserve(acc, eps, 1.0 - eps); }

    alpha.assign(R+1, 0.0);
    beta.assign(C+1, 0.0);
    alpha[0] = -INFINITY; alpha[R] = INFINITY;
    beta[0]  = -INFINITY; beta[C]  = INFINITY;
    for (int i=1;i<=R-1;++i) alpha[i] = qnorm01(rowcum[i-1]);
    for (int j=1;j<=C-1;++j) beta[j]  = qnorm01(colcum[j-1]);
}

static inline double polychoric_loglik(double rho, const NumericMatrix& N,
                                       const std::vector<double>& alpha,
                                       const std::vector<double>& beta)
{
  using matrixCorr_detail::bvn_adaptive::rect_prob;

  const int R = N.nrow(), C = N.ncol();
  double ll = 0.0;
  for (int i=1;i<=R; ++i){
    for (int j=1;j<=C; ++j){
      const double pij = std::max(rect_prob(alpha[i-1], alpha[i], beta[j-1], beta[j], rho), 1e-16);
      ll += N(i-1, j-1) * std::log(pij);
    }
  }
  return ll;
}

// ---------- exports ----------

// [[Rcpp::export]]
double matrixCorr_tetrachoric_mle_cpp(Rcpp::NumericMatrix tab, double correct = 0.5){
  using matrixCorr_detail::norm1::Phi;
  using matrixCorr_detail::norm1::qnorm01;
  using matrixCorr_detail::bvn_adaptive::Phi2;

  if (tab.nrow() != 2 || tab.ncol() != 2) return NA_REAL;

  // Copy counts
  double a = tab(0,0), b = tab(0,1), c = tab(1,0), d = tab(1,1);
  const bool any_zero = (a <= 0.0) || (b <= 0.0) || (c <= 0.0) || (d <= 0.0);

  // Continuity correction only when requested (psych behavior)
  if (correct > 0.0 && any_zero){
    if (a <= 0.0) a += correct;
    if (b <= 0.0) b += correct;
    if (c <= 0.0) c += correct;
    if (d <= 0.0) d += correct;
  }

  // Fixed cut-points (tau) from marginals — always (psych keeps τ fixed)
  const double N   = a + b + c + d;
  const double eps = 1e-12;
  const double p_row1 = matrixCorr_detail::clamp_policy::nan_preserve((a + b) / N, eps, 1.0 - eps);
  const double p_col1 = matrixCorr_detail::clamp_policy::nan_preserve((a + c) / N, eps, 1.0 - eps);
  const double t1 = qnorm01(p_row1);
  const double t2 = qnorm01(p_col1);

  // Log-probability floor: very tiny when correct==0 to mimic psych
  const double floor_eps = (correct == 0.0) ? 1e-300 : 1e-16;

  auto nll = [&](double rho){
    // rectangle probs with fixed cutpoints
    const double p11 = std::max(Phi2(t1, t2, rho), floor_eps);
    const double p1_ = Phi(t1);
    const double _1p = Phi(t2);
    const double p10 = std::max(p1_ - p11,         floor_eps);
    const double p01 = std::max(_1p - p11,         floor_eps);
    const double p00 = std::max(1.0 - p1_ - _1p + p11, floor_eps);
    return -( a*std::log(p11) + b*std::log(p10) + c*std::log(p01) + d*std::log(p00) );
  };

  // Coarse scan to bracket a good region, then Brent
  const double lo = -0.99999, hi = 0.99999;
  const int    G  = 101;                      // small grid for speed
  const double h  = (hi - lo) / (G - 1);

  double r_best = lo, v_best = nll(lo);
  for (int g = 1; g < G; ++g){
    const double r = lo + g*h;
    const double v = nll(r);
    if (v < v_best){ v_best = v; r_best = r; }
  }
  double a_br = std::max(lo, r_best - 6*h);
  double b_br = std::min(hi, r_best + 6*h);
  if (b_br - a_br < 12*h) { a_br = std::max(lo, r_best - 12*h);
    b_br = std::min(hi, r_best + 12*h); }

  const double rho_hat = matrixCorr_detail::brent::optimize(nll, a_br, b_br, 1e-6, 200);
  return matrixCorr_detail::clamp_policy::nan_preserve(rho_hat, -1.0, 1.0);
}

// [[Rcpp::export]]
double matrixCorr_polychoric_mle_cpp(NumericMatrix tab, double correct = 0.5){
  const int R = tab.nrow(), C = tab.ncol();
  if (R < 2 || C < 2) return NA_REAL;

  NumericMatrix N = clone(tab);
  for (int i=0;i<R;++i)
    for (int j=0;j<C;++j)
      if (N(i,j) <= 0.0) N(i,j) += correct;

      std::vector<double> alpha, beta;
      build_cutpoints(N, alpha, beta);

      auto nll = [&](double rho){ return -polychoric_loglik(rho, N, alpha, beta); };

      // --- robust global search: coarse grid then local Brent refine ---
      const double lo = -0.99999, hi = 0.99999;
      const int    G  = 801;
      const double h  = (hi - lo) / (G - 1);

      double best_rho = lo, best_val = nll(lo);
      for (int g = 1; g < G; ++g) {
        const double r = lo + g * h;
        const double v = nll(r);
        if (v < best_val) { best_val = v; best_rho = r; }
      }

      double a_br = std::max(lo, best_rho - 5*h);
      double b_br = std::min(hi, best_rho + 5*h);
      if (b_br - a_br < 20*h) {
        a_br = std::max(lo, best_rho - 10*h);
        b_br = std::min(hi, best_rho + 10*h);
      }

      const double est = matrixCorr_detail::brent::optimize(nll, a_br, b_br, 1e-6, 300);
      return matrixCorr_detail::clamp_policy::nan_preserve(est, -1.0, 1.0);
}

// [[Rcpp::export]]
double matrixCorr_biserial_latent_cpp(NumericVector x, LogicalVector y){
  using matrixCorr_detail::norm1::phi;
  using matrixCorr_detail::norm1::qnorm01;

  const int n = x.size();
  if (n != y.size() || n < 2) return NA_REAL;

  double mx = 0.0; for (int i=0;i<n;++i) mx += x[i]; mx /= n;
  double v  = 0.0; for (int i=0;i<n;++i){ const double d = x[i]-mx; v += d*d; }
  if (v <= 0.0) return NA_REAL;
  const double sx = std::sqrt(v / (n - 1));

  int n1 = 0, n0 = 0; double s1 = 0.0, s0 = 0.0;
  for (int i=0;i<n;++i){
    if (y[i]){ s1 += x[i]; n1++; } else { s0 += x[i]; n0++; }
  }
  if (n1 == 0 || n0 == 0) return NA_REAL;

  const double mean1 = s1 / n1, mean0 = s0 / n0;
  const double p     = matrixCorr_detail::clamp_policy::nan_preserve(double(n1)/n, 1e-12, 1.0 - 1e-12);
  const double q     = 1.0 - p;
  const double zp    = phi(qnorm01(p));
  const double r     = ((mean1 - mean0) / sx) * (p * q) / zp;
  return matrixCorr_detail::clamp_policy::nan_preserve(r, -1.0, 1.0);
}

// [[Rcpp::export]]
double matrixCorr_polyserial_mle_cpp(NumericVector x, IntegerVector y){
  using matrixCorr_detail::norm1::Phi;
  using matrixCorr_detail::norm1::qnorm01;

  const int n = x.size();
  if (n != y.size() || n < 2) return NA_REAL;

  // standardise x
  double mx = 0.0; for (int i=0;i<n;++i) mx += x[i]; mx /= n;
  double v  = 0.0; for (int i=0;i<n;++i){ const double d = x[i]-mx; v += d*d; }
  if (v <= 0.0) return NA_REAL;
  const double sx = std::sqrt(v / (n - 1));
  std::vector<double> xs(n);
  for (int i=0;i<n;++i) xs[i] = (x[i] - mx) / sx;

  // map y to 1..K and build cutpoints from marginals
  IntegerVector yu = clone(y);
  int ymin = yu[0], ymax = yu[0];
  for (int i=1;i<n;++i){ if (yu[i]<ymin) ymin=yu[i]; if (yu[i]>ymax) ymax=yu[i]; }
  for (int i=0;i<n;++i) yu[i] = yu[i] - ymin + 1;
  const int K = ymax - ymin + 1;
  if (K < 2) return NA_REAL;

  std::vector<double> beta(K+1);
  beta[0] = -INFINITY; beta[K] = INFINITY;
  std::vector<double> freq(K, 0.0);
  for (int i=0;i<n;++i) freq[ yu[i]-1 ] += 1.0;

  const double eps = 1e-12;
  double acc = 0.0;
  for (int k=1; k<=K-1; ++k){
    acc += freq[k-1] / n;
    acc = matrixCorr_detail::clamp_policy::nan_preserve(acc, eps, 1.0 - eps);
    beta[k] = qnorm01(acc);
  }

  auto nll = [&](double rho){
    rho = matrixCorr_detail::clamp_policy::nan_preserve(rho, -0.999999, 0.999999);
    const double sig = std::sqrt(1.0 - rho*rho);
    double negLL = 0.0;

#ifdef _OPENMP
#pragma omp parallel for reduction(+:negLL)
#endif
    for (int i=0; i<n; ++i){
      const int k  = yu[i];
      const double xz = xs[i];
      const double u = (beta[k]   - rho * xz) / sig;
      const double l = (beta[k-1] - rho * xz) / sig;
      const double pk = std::max(Phi(u) - Phi(l), 1e-16);
      negLL += -std::log(pk);
    }
    return negLL;
  };

  const double est = matrixCorr_detail::brent::optimize(nll, -0.99999, 0.99999, 1e-6, 100);
  return matrixCorr_detail::clamp_policy::nan_preserve(est, -1.0, 1.0);
}

// [[Rcpp::export]]
double matrixCorr_polydi_mle_cpp(NumericMatrix tab, double correct = 0.5){
  const int R = tab.nrow(), C = tab.ncol();
  if (R < 2 || C != 2) return NA_REAL;
  return matrixCorr_polychoric_mle_cpp(tab, correct); // reuse
}
