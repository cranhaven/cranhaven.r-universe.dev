// Thiago de Paula Oliveira
// Repeated-measures Bland–Altman via stabilized EM/GLS with optional AR(1)
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <unordered_map>
#include <string>
#include <sstream>
#include <cmath>
#include <numeric>
#include <algorithm>

// bring in your helpers
#include "matrixCorr_detail.h"

using namespace Rcpp;
using namespace arma;

// ---- use selected helpers from matrixCorr_detail ----
using matrixCorr_detail::clamp_policy::nan_preserve;
using matrixCorr_detail::linalg::inv_sympd_safe;
using matrixCorr_detail::linalg::solve_sympd_safe;
using matrixCorr_detail::moments::sample_var;
using matrixCorr_detail::moments::sample_cov;
using matrixCorr_detail::indexing::reindex;

// ---------- local helpers ----------
static inline bool all_finite(const arma::vec& v) {
  for (arma::uword i = 0; i < v.n_elem; ++i) if (!std::isfinite(v[i])) return false;
  return true;
}

// ---- AR(1) block precision (STRICT contiguity: t_{k+1} = t_k + 1) ----
static inline void ar1_precision_from_time_ba(const std::vector<int>& tim, double rho, mat& Cinv) {
  const int n = (int)tim.size();
  Cinv.zeros(n, n);
  if (n == 0) return;
  const double r2 = rho * rho;
  const double denom = std::max(1.0 - r2, 1e-12);
  int s = 0;
  while (s < n) {
    if (tim[s] < 0) { Cinv(s,s) += 1.0; ++s; continue; }  // singleton/NA -> iid
    int e = s;
    while (e + 1 < n && tim[e+1] >= 0 && tim[e+1] == tim[e] + 1) ++e; // contiguous only
    const int L = e - s + 1;
    if (L == 1) {
      Cinv(s,s) += 1.0;
    } else {
      Cinv(s,s) += 1.0 / denom;
      Cinv(e,e) += 1.0 / denom;
      for (int t = s+1; t <= e-1; ++t) Cinv(t,t) += (1.0 + r2) / denom;
      for (int t = s; t <= e-1; ++t) {
        Cinv(t, t+1) += -rho / denom;
        Cinv(t+1, t) += -rho / denom;
      }
    }
    s = e + 1;
  }
  Cinv.diag() += 1e-10; // tiny ridge
}

// ---------- pair builder: subject-time matched differences ----------
struct PairData {
  std::vector<double> d;     // y2 - y1
  std::vector<double> mean;  // (y1 + y2)/2
  std::vector<int>    subj;
  std::vector<int>    time;
};
static inline std::string key_of(int s, int t) {
  std::ostringstream oss; oss<<s<<'#'<<t; return oss.str();
}

static PairData make_pairs(const NumericVector& y,
                           const IntegerVector& subject,
                           const IntegerVector& method,
                           const IntegerVector& time) {
  const int n = y.size();
  if (subject.size()!=n || method.size()!=n || time.size()!=n)
    stop("lengths of y, subject, method, time must match.");
  struct V { bool has1=false, has2=false; double y1=NA_REAL, y2=NA_REAL; int s, t; };
  std::unordered_map<std::string, V> H; H.reserve((size_t)n);
  for (int i=0;i<n;++i) {
    if (IntegerVector::is_na(subject[i]) || IntegerVector::is_na(method[i]) || IntegerVector::is_na(time[i])) continue;
    if (NumericVector::is_na(y[i])) continue;
    int s = subject[i], t = time[i], m = method[i];
    if (m!=1 && m!=2) continue;     // only the two target methods for this pair
    auto key = key_of(s,t);
    auto &v = H[key]; v.s = s; v.t = t;
    if (m==1) { v.has1=true; v.y1=y[i]; }
    else      { v.has2=true; v.y2=y[i]; }
  }
  PairData P;
  P.d.reserve(H.size()); P.mean.reserve(H.size());
  P.subj.reserve(H.size()); P.time.reserve(H.size());
  for (auto &kv : H) {
    const V& v = kv.second;
    if (v.has1 && v.has2 && std::isfinite(v.y1) && std::isfinite(v.y2)) {
      P.d.push_back(v.y2 - v.y1);
      P.mean.push_back(0.5 * (v.y1 + v.y2));
      P.subj.push_back(v.s);
      P.time.push_back(v.t);
    }
  }
  if (P.d.empty()) stop("No complete subject-time pairs (both methods present).");
  return P;
}

// group rows by subject, sorted by time (NA/negative at end)
struct BySubjBA {
  std::vector< std::vector<int> > rows;
  std::vector< std::vector<int> > tim;
};
static BySubjBA index_by_subject_ba(const std::vector<int>& subj_idx,
                                    const std::vector<int>& time) {
  const int m = *std::max_element(subj_idx.begin(), subj_idx.end()) + 1;
  BySubjBA S; S.rows.assign(m,{}); S.tim.assign(m,{});
  for (size_t i=0;i<subj_idx.size();++i) {
    int j = subj_idx[i];
    S.rows[j].push_back((int)i);
    S.tim[j].push_back(time[i]);
  }
  for (int i=0;i<m;++i) {
    auto &r = S.rows[i]; auto &t = S.tim[i];
    std::vector<int> ord(r.size()); std::iota(ord.begin(), ord.end(), 0);
    std::stable_sort(ord.begin(), ord.end(), [&](int a, int b){
      int ta=t[a], tb=t[b];
      if (ta<0 && tb<0) return a<b;
      if (ta<0) return false;
      if (tb<0) return true;
      return ta<tb;
    });
    std::vector<int> r2, t2; r2.reserve(r.size()); t2.reserve(t.size());
    for (int k:ord) { r2.push_back(r[k]); t2.push_back(t[k]); }
    r.swap(r2); t.swap(t2);
  }
  return S;
}

// ------------ per-subject precompute -----------
struct Precomp {
  int n_i=0;
  mat X_i;   // n_i x p
  vec y_i;   // n_i
  mat Cinv;  // n_i x n_i  (I or AR1 precision)

  // sufficient stats with Cinv (scale-free):
  mat XTCX;  // p x p
  vec XTCy;  // p
  vec UTCX;  // p (1^T C X)
  double UTCy = 0.0;   // 1^T C y
  double UCU  = 0.0;   // 1^T C 1
};

static std::vector<Precomp> precompute_blocks(const mat& X, const vec& y,
                                              const BySubjBA& S,
                                              bool use_ar1, double rho) {
  const int p = X.n_cols;
  const int m = (int)S.rows.size();
  std::vector<Precomp> out(m);
  for (int i=0;i<m;++i) {
    const auto& rows = S.rows[i];
    const auto& tim  = S.tim[i];
    const int n_i = (int)rows.size();
    if (n_i==0) continue;
    Precomp P;
    P.n_i = n_i;
    P.X_i.set_size(n_i,p); P.y_i.set_size(n_i);
    std::vector<int> tim_ord(n_i,-1);
    for (int k=0;k<n_i;++k) { int g = rows[k]; P.X_i.row(k)=X.row(g); P.y_i[k]=y[g]; tim_ord[k]=tim[k]; }
    if (use_ar1) ar1_precision_from_time_ba(tim_ord, rho, P.Cinv);
    else P.Cinv.eye(n_i, n_i);

    vec ones(n_i, fill::ones);

    // C-side sufficient stats
    mat CX = P.Cinv * P.X_i;
    P.XTCX = P.X_i.t() * CX;
    P.XTCy = P.X_i.t() * (P.Cinv * P.y_i);
    vec CU = P.Cinv * ones;
    P.UTCX = (ones.t() * CX).t();
    P.UTCy = arma::as_scalar(ones.t() * (P.Cinv * P.y_i));
    P.UCU  = arma::as_scalar(ones.t() * CU);

    out[i] = std::move(P);
  }
  return out;
}

// ---------- EM/GLS for differences: random subject intercept + (optional) AR(1) ----------
struct FitOut {
  arma::vec beta;
  double su2=NA_REAL, se2=NA_REAL;
  arma::vec su_term;    // E[u_i^2|y] per subject (for delta)
  arma::vec se_term;    // per-subject contribution for se2 variance (normalized)
  int iter = 0;
  bool converged = false;
  std::string warn;
};

static FitOut fit_diff_em(const mat& X, const vec& y,
                          const BySubjBA& S,
                          const std::vector<Precomp>& PC,
                          int max_iter, double tol) {
  const int p = X.n_cols;
  const int m = (int)S.rows.size();
  const int n = y.n_rows;

  const double vref = arma::var(y, /*unbiased*/1);
  const double EPS  = std::max(1e-12, vref * 1e-12);
  const double MAXV = std::max(10.0 * vref, 1.0); // tight, model-based cap

  // method-of-moments-ish init
  std::vector<double> mu_s(m, 0.0); std::vector<int> cnt_s(m, 0);
  for (int i=0;i<m;++i) for (int r : S.rows[i]) { mu_s[i] += y[r]; ++cnt_s[i]; }
  int used_mu = 0; for (int i=0;i<m;++i) if (cnt_s[i]>0){ mu_s[i] /= (double)cnt_s[i]; ++used_mu; }
  arma::vec mu_s_vec(used_mu, arma::fill::zeros);
  { int k=0; for (int i=0;i<m;++i) if (cnt_s[i]>0) mu_s_vec[k++] = mu_s[i]; }
  double var_mu = (used_mu>=2 ? arma::var(mu_s_vec, /*unbiased*/true) : 0.0);

  double num_w = 0.0, den_w = 0.0;
  for (int i=0;i<m;++i) if ((int)S.rows[i].size() >= 2) {
    arma::vec yi(S.rows[i].size());
    for (size_t k=0;k<S.rows[i].size();++k) yi[k] = y[ S.rows[i][k] ];
    double vsi = arma::var(yi, /*unbiased*/true);
    num_w += ((int)yi.n_elem - 1) * vsi;
    den_w += ((int)yi.n_elem - 1);
  }
  double se2_init = (den_w > 0.5 ? num_w / den_w : std::max(0.0, vref * 0.5));
  double su2_init = std::max(0.0, var_mu - se2_init / std::max(1.0, arma::mean(arma::conv_to<arma::vec>::from(cnt_s))));
  se2_init = nan_preserve(se2_init, EPS, MAXV);
  su2_init = nan_preserve(su2_init, 0.0, MAXV);

  auto damp_to_ratio = [](double oldv, double newv, double rmax){
    if (!std::isfinite(newv)) return oldv;
    if (oldv <= 0.0) return nan_preserve(newv, 1e-12, rmax);
    double lo = oldv / rmax, hi = oldv * rmax;
    return nan_preserve(newv, std::min(lo, hi), std::max(lo, hi));
  };

  double su2 = su2_init, se2 = se2_init;
  vec beta(p, fill::zeros);
  vec su_term(m, fill::zeros);
  vec se_term(m, fill::zeros);

  bool ok = true;
  int   it = 0;

  for (it=0; it<max_iter; ++it) {
    // (1) Assemble XtViX, XtViy
    mat XtViX(p,p, fill::zeros);
    vec XtViy(p,  fill::zeros);
    const double inv_se = 1.0/std::max(se2, EPS);

    for (int i=0;i<m;++i) {
      const Precomp& P = PC[i];
      if (P.n_i==0) continue;

      double M = (1.0/std::max(su2,EPS)) + inv_se * P.UCU; // scalar
      M = std::max(M, 1e-12);
      const double Minv = 1.0 / M;

      double S_uy = inv_se * P.UTCy;
      double Z_y  = Minv * S_uy;

      vec S_ux = inv_se * P.UTCX;     // p
      vec Z_X  = Minv * S_ux;         // p

      mat XTRinvX = inv_se * P.XTCX;
      vec XTRinvY = inv_se * P.XTCy;

      XtViy += XTRinvY - S_ux * Z_y;
      XtViX += XTRinvX - S_ux * Z_X.t();
    }

    // (2) GLS beta
    mat XtViX_inv;
    if (!inv_sympd_safe(XtViX_inv, XtViX) || !XtViX_inv.is_finite()) { ok=false; break; }
    beta = XtViX_inv * XtViy;
    if (!all_finite(beta)) { ok=false; break; }

    // (3) M-step (stabilized)
    double su_acc = 0.0;
    double se_num = 0.0;   // direct numerator for sigma_e^2
    vec r_global = y - X * beta;

    for (int i=0;i<m;++i) {
      const Precomp& P = PC[i];
      if (P.n_i==0) continue;

      vec r_i(P.n_i);
      for (int k=0;k<P.n_i;++k) r_i[k] = r_global[ S.rows[i][k] ];

      const double inv_su = 1.0/std::max(su2,EPS);
      double M = inv_su + (1.0/std::max(se2,EPS)) * P.UCU; // scalar
      M = std::max(M, 1e-12);
      const double Minv = 1.0 / M;

      double Utr = (1.0/std::max(se2,EPS)) *
        arma::as_scalar( arma::ones<vec>(P.n_i).t() * (P.Cinv * r_i) );

      double b_i = Minv * Utr;                      // BLUP(u_i)
      double Eu2 = b_i*b_i + Minv;                  // E[u_i^2 | y]

      su_acc += Eu2;
      su_term[i] = Eu2;

      vec e = r_i - arma::ones<vec>(P.n_i) * b_i;   // residuals after u_i

      // Quadratic form under R^{-1} (Cinv):
      double q = arma::as_scalar( e.t() * (P.Cinv * e) );

      // ---- trace term on the R-side ----
      //Var(u|y) * (1' Cinv 1)
      double num_i = q + P.UCU * Minv;
      se_num += num_i;
      se_term[i] = num_i / std::max(1, P.n_i);      // per-subject normalized contribution
    }

    double su2_new = su_acc / std::max(1, m);
    double se2_new = se_num / std::max(1, n);

    // trust-region damping (avoid explosions/implosions)
    su2_new = damp_to_ratio(su2, su2_new, 3.0);
    se2_new = damp_to_ratio(se2, se2_new, 3.0);

    // clamp to sane range
    su2_new = nan_preserve(su2_new, 0.0, MAXV);
    se2_new = nan_preserve(se2_new, EPS, MAXV);

    if (!std::isfinite(su2_new) || !std::isfinite(se2_new)) { ok=false; break; }

    double diff = std::fabs(su2_new - su2) + std::fabs(se2_new - se2);
    su2 = su2_new; se2 = se2_new;
    if (diff < tol) { ok = true; break; }
  }

  FitOut out;
  if (!all_finite(beta)) beta.zeros();
  out.beta = beta;
  out.su2 = su2;
  out.se2 = se2;
  out.su_term = su_term;
  out.se_term = se_term;
  out.iter = it + 1;
  out.converged = ok;

  if (!ok) {
    out.warn = "EM did not converge cleanly; stabilized updates applied.";
    if (!std::isfinite(out.su2) || !std::isfinite(out.se2)) {
      out.su2 = nan_preserve(std::max(0.0, 0.1 * vref), 0.0, MAXV);
      out.se2 = nan_preserve(std::max(0.0, 0.9 * vref), 1e-12, MAXV);
    }
  }
  return out;
}

// subject-equal bias (equal weight to each subject's mean difference)
static inline double bias_subject_equal_weight(const std::vector<double>& d,
                                               const std::vector<int>& subj_idx,
                                               int m,
                                               arma::vec& subj_means_out) {
  std::vector<double> sum(m,0.0); std::vector<int> cnt(m,0);
  for (size_t i=0;i<d.size();++i){ sum[subj_idx[i]] += d[i]; ++cnt[subj_idx[i]]; }
  subj_means_out.set_size(m);
  int used=0; double acc=0.0;
  for (int i=0;i<m;++i) {
    if (cnt[i]>0) { double mu_i = sum[i]/(double)cnt[i]; subj_means_out[i]=mu_i; acc += mu_i; ++used; }
    else subj_means_out[i]=0.0;
  }
  if (used==0) stop("No data per subject to compute bias.");
  return acc / (double)used;
}

// ----- AR(1) rho from residuals (robust for short L)
static double estimate_rho_moments(const FitOut& fit,
                                   const arma::mat& X,
                                   const arma::vec& y,
                                   const BySubjBA& S,
                                   const std::vector<Precomp>& /*unused*/) {
  const arma::vec beta = fit.beta;
  const double EPS = 1e-12;

  double z_sum = 0.0, w_sum = 0.0;   // Fisher-z

  for (size_t i = 0; i < S.rows.size(); ++i) {
    const auto& rows = S.rows[i];
    const auto& tim  = S.tim[i];
    const int n_i = (int)rows.size();
    if (n_i <= 2) continue;

    int s = 0;
    while (s < n_i) {
      if (tim[s] < 0) { ++s; continue; }
      int e = s;
      while (e + 1 < n_i && tim[e+1] == tim[e] + 1) ++e;
      const int L = e - s + 1;
      if (L >= 3) {
        arma::vec r(L);
        for (int k = 0; k < L; ++k)
          r[k] = y[ rows[s+k] ] - arma::as_scalar( X.row( rows[s+k] ) * beta );

        // Detrend by (intercept + linear time) within the block
        arma::vec t(L); for (int k = 0; k < L; ++k) t[k] = (double)k;
        arma::mat Z(L, 2); Z.col(0).ones(); Z.col(1) = t - arma::mean(t);
        arma::mat ZZ = Z.t() * Z;
        arma::vec Zy = Z.t() * r;
        arma::vec b  = solve_sympd_safe(ZZ, Zy);
        arma::vec u  = r - Z * b;

        if (L <= 3) { s = e + 1; continue; } // need at least 4 after detrend to be stable

        // Lag-1 on detrended residuals
        arma::vec u1 = u.subvec(0, L - 2);
        arma::vec u2 = u.subvec(1, L - 1);
        double den = arma::dot(u1, u1);
        if (den <= EPS) { s = e + 1; continue; }
        double rho = arma::dot(u1, u2) / den;
        rho = nan_preserve(rho, -0.999, 0.999);

        // small-sample adjustment
        double adj = (1.0 - rho * rho) / std::max(3, L);
        double rho_bc = nan_preserve(rho + adj, -0.999, 0.999);

        // Fisher-z pool; effective weight approx. L - 3
        double w = std::max(1.0, (double)L - 3.0);
        z_sum += w * std::atanh(rho_bc);
        w_sum += w;
      }
      s = e + 1;
    }
  }
  if (w_sum <= 0.0) return 0.0;
  return nan_preserve(std::tanh(z_sum / w_sum), -0.999, 0.999);
}


// [[Rcpp::export]]
Rcpp::List bland_altman_repeated_em_ext_cpp(
    Rcpp::NumericVector y,          // stacked responses
    Rcpp::IntegerVector subject,    // subject id (any integers)
    Rcpp::IntegerVector method,     // MUST be 1 or 2 for the selected pair
    Rcpp::IntegerVector time,       // pairing key (replicate/time)
    bool include_slope = false,     // proportional bias: slope vs pair mean
    bool use_ar1 = false,           // AR(1) within subject on residuals
    double ar1_rho = NA_REAL,       // if NA and use_ar1=TRUE, estimate it
    int    max_iter = 200,
    double tol = 1e-6,
    double conf_level = 0.95,
    double two_arg = NA_REAL,
    bool   use_cov_su_se = true
) {
  if (y.size()==0) stop("Empty input.");
  if (use_ar1 && Rcpp::NumericVector::is_na(ar1_rho)==false && std::fabs(ar1_rho) >= 0.999)
    stop("ar1_rho must be in (-0.999, 0.999).");

  // 1) Build paired differences
  PairData P = make_pairs(y, subject, method, time);

  // 2) Subject indexing over PAIRS (not raw rows)
  std::vector<int> subj_idx; int m=0;
  reindex(P.subj, subj_idx, m);              // (replaces reindex_subjects_ba)
  BySubjBA S = index_by_subject_ba(subj_idx, P.time);

  // 3) Fixed-effects design: [Intercept, (optional) scaled pair mean]
  const int p = include_slope ? 2 : 1;
  mat X(P.d.size(), p, fill::zeros);
  for (size_t i=0;i<P.d.size();++i) X(i,0) = 1.0;

  vec x2; double x2_mean = 0.0, x2_scale = 1.0;
  if (include_slope) {
    x2 = vec(P.mean.data(), P.mean.size(), /*copy*/ false);
    x2_mean  = arma::mean(x2);
    x2_scale = arma::stddev(x2);
    if (!std::isfinite(x2_scale) || x2_scale < 1e-9) x2_scale = 1.0;
    vec x2c = (x2 - x2_mean) / x2_scale;
    X.col(1) = x2c;
  }
  vec ydiff(P.d.data(), P.d.size(), /*copy*/ false);

  // Strategy:
  //   - If AR1 requested & rho is NA: fit iid, estimate rho, refit with AR1.
  //   - Else: fit directly with requested structure.
  bool ar1_estimated = false;
  double rho_used = NA_REAL;
  FitOut fit;

  if (use_ar1 && Rcpp::NumericVector::is_na(ar1_rho)) {
    // First pass: iid
    std::vector<Precomp> PC_iid = precompute_blocks(X, ydiff, S, /*use_ar1*/false, 0.0);
    fit = fit_diff_em(X, ydiff, S, PC_iid, max_iter, tol);

    // Estimate rho from residuals
    double rho_hat = estimate_rho_moments(fit, X, ydiff, S, PC_iid);
    rho_used = rho_hat;
    ar1_estimated = true;

    // Second pass: AR1 with rho_hat
    std::vector<Precomp> PC_ar1 = precompute_blocks(X, ydiff, S, /*use_ar1*/true, rho_hat);
    fit = fit_diff_em(X, ydiff, S, PC_ar1, max_iter, tol);

  } else {
    if (use_ar1) rho_used = nan_preserve(ar1_rho, -0.999, 0.999);
    std::vector<Precomp> PC = precompute_blocks(X, ydiff, S, use_ar1, (use_ar1 ? rho_used : 0.0));
    fit = fit_diff_em(X, ydiff, S, PC, max_iter, tol);
  }

  vec beta = fit.beta;
  double beta0 = beta[0];
  double beta1 = (p==2 ? beta[1] : NA_REAL);
  if (include_slope) {
    // Undo centering/scaling: beta1_orig = beta1 / x2_scale
    // intercept_orig = beta0 - beta1_orig * x2_mean
    double beta1_orig = beta1 / x2_scale;
    double beta0_orig = beta0 - beta1_orig * x2_mean;
    beta0 = beta0_orig;
    beta1 = beta1_orig;
  }
  const double su2 = fit.su2;         // subject random intercept variance
  const double se2 = fit.se2;         // residual variance (for a single difference)

  // 6) Bias from subject-equal weighting
  arma::vec subj_means;
  double mu0 = bias_subject_equal_weight(P.d, subj_idx, m, subj_means);

  // 7) LoA for a single new paired measurement
  const double alpha = 1.0 - std::min(std::max(conf_level, 0.0), 1.0);
  const double z     = R::qnorm(1.0 - 0.5 * alpha, 0.0, 1.0, 1, 0); // CI multiplier
  double two         = two_arg;                                     // LoA multiplier
  if (!std::isfinite(two) || two <= 0.0) {
    // Fallback: use the conf_level-implied width (e.g., 1.96 at 95%)
    two = z;
  }

  const double V_loa = nan_preserve(su2 + se2, 0.0, 1e12);
  const double sd_loa = std::sqrt(V_loa);
  const double loa_lower = mu0 - two * sd_loa;
  const double loa_upper = mu0 + two * sd_loa;

  // 8) Delta-method CIs
  // Var(mu0) = var(subject means)/m  (equal-weight)
  int m_used = 0;
  for (int i = 0; i < m; ++i) if (!S.rows[i].empty()) ++m_used;
  arma::vec subj_means_used(m_used);
  for (int i = 0, k = 0; i < m; ++i) if (!S.rows[i].empty()) subj_means_used[k++] = subj_means[i];

  double var_mu0 = (m_used >= 2 ? sample_var(subj_means_used) / std::max(1, m_used) : 0.0);

  // Var(su2_hat) = Var(su_term)/m ; Var(se2_hat) via weighted subject sizes
  double var_su = sample_var(fit.su_term) / std::max(1, m);

  arma::vec se_contrib = fit.se_term;                 // per-subject num_i / n_i
  // weights ~ n_i / n_total  -> sum w^2 factor
  double n_total = (double)ydiff.n_elem;
  double w2sum = 0.0;
  for (int i=0;i<m;++i) {
    double wi = ((double)S.rows[i].size()) / std::max(1.0, n_total);
    w2sum += wi*wi;
  }
  double var_se = sample_var(se_contrib) * w2sum;

  double cov_su_se = 0.0;
  if (use_cov_su_se) {
    // build subject-used vectors (exclude empty subjects just in case)
    arma::vec A_used(m_used), B_used(m_used);
    for (int i = 0, k = 0; i < m; ++i) {
      if (!S.rows[i].empty()) {
        A_used[k] = fit.su_term[i];   // A_i = E[u_i^2 | y]
        B_used[k] = fit.se_term[i];   // B_i = num_i / n_i
        ++k;
      }
    }
    // Cov( su_hat , se_hat ) approx (1/m_used) * Cov(A_i, B_i)
    cov_su_se = sample_cov(A_used, B_used) / std::max(1, m_used);
  }

  // Var(V) and Var(sd) via delta
  double var_V = var_su + var_se + 2.0 * cov_su_se;
  double var_sd = (V_loa > 0.0 ? var_V / (4.0 * V_loa) : 0.0);

  // ---- FIX: 'two' inside, 'z' outside ----
  double var_Lpm = var_mu0 + (two * two) * var_sd;

  const double se_bias  = std::sqrt(std::max(0.0, var_mu0));
  const double se_Lpm   = std::sqrt(std::max(0.0, var_Lpm));

  const double bias_lwr = mu0 - z * se_bias;
  const double bias_upr = mu0 + z * se_bias;
  const double loa_lower_lwr = loa_lower - z * se_Lpm;
  const double loa_lower_upr = loa_lower + z * se_Lpm;
  const double loa_upper_lwr = loa_upper - z * se_Lpm;
  const double loa_upper_upr = loa_upper + z * se_Lpm;

  return List::create(
    _["n_pairs"]     = (int)P.d.size(),
    _["n_subjects"]  = m,
    _["pairs_mean"]  = Rcpp::NumericVector(P.mean.begin(), P.mean.end()),
    _["pairs_diff"]  = Rcpp::NumericVector(P.d.begin(),   P.d.end()),
    // estimates
    _["bias_mu0"]    = mu0,
    _["bias_se"]     = se_bias,
    _["bias_lwr"]    = bias_lwr,
    _["bias_upr"]    = bias_upr,
    _["beta_intercept"] = beta0,
    _["beta_slope"]  = (include_slope ? beta1 : NA_REAL),
    // variance components
    _["sigma2_subject"] = su2,
    _["sigma2_resid"]   = se2,
    // LoA
    _["sd_loa"]      = sd_loa,
    _["loa_lower"]   = loa_lower,
    _["loa_upper"]   = loa_upper,
    _["loa_lower_lwr"] = loa_lower_lwr,
    _["loa_lower_upr"] = loa_lower_upr,
    _["loa_upper_lwr"] = loa_upper_lwr,
    _["loa_upper_upr"] = loa_upper_upr,
    // AR(1)
    _["use_ar1"]     = use_ar1,
    _["ar1_rho"]     = (use_ar1 ? rho_used : NA_REAL),
    _["ar1_estimated"] = (use_ar1 ? ar1_estimated : false),
    // misc
    _["conf_level"]  = conf_level,
    _["converged"]   = fit.converged,
    _["iter"]        = fit.iter,
    _["warn"]        = fit.warn
  );
}

