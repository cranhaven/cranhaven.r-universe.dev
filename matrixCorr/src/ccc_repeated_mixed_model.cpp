// Thiago de Paula Oliveira
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <cstdlib>

#ifdef _OPENMP
#include <omp.h>
#if defined(__unix__) || defined(__APPLE__)
#include <dlfcn.h>
#endif
#endif

#include "matrixCorr_detail.h"

using namespace Rcpp;
using namespace arma;

// ---- use helpers from matrixCorr_detail ----
using matrixCorr_detail::linalg::inv_sympd_safe;
using matrixCorr_detail::linalg::solve_sympd_safe;
using matrixCorr_detail::linalg::logdet_spd_safe;
using matrixCorr_detail::timeseries::ar1::kappa_T;
using matrixCorr_detail::timeseries::ar1::make_Cinv_by_method;
using matrixCorr_detail::indexing::BySubject;
using matrixCorr_detail::indexing::reindex;
using matrixCorr_detail::indexing::group_by_subject;
using matrixCorr_detail::design::build_U_base;
using matrixCorr_detail::design::gram_UtU;
using matrixCorr_detail::design::accumulate_Ut_vec;
using matrixCorr_detail::design::add_U_times;
using matrixCorr_detail::linalg::rows_take_to;
using matrixCorr_detail::moments::sample_var;
using matrixCorr_detail::timeseries::kappas::clamp01;
using matrixCorr_detail::timeseries::kappas::kappa_e_equal_ar1;
using matrixCorr_detail::timeseries::kappas::kappa_g_weighted;
using matrixCorr_detail::timeseries::kappas::kappa_e_weighted_ar1;

// =========================
// ==  helpers (thread guard)
// =========================
#ifdef _OPENMP
namespace detail_blas_guard {
struct BLASThreadGuard {
  int saved_openblas = -1;
  int saved_mkl      = -1;
  using OB_get_t = int  (*)();
  using OB_set_t = void (*)(int);
  using MKL_get_t = int  (*)();
  using MKL_set_t = void (*)(int);
  using MKL_dyn_t = void (*)(int);
  OB_get_t  ob_get  = nullptr;
  OB_set_t  ob_set  = nullptr;
  OB_set_t  ob_set_local = nullptr;
  MKL_get_t mkl_get = nullptr;
  MKL_set_t mkl_set_local = nullptr;
  MKL_dyn_t mkl_set_dynamic = nullptr;

  explicit BLASThreadGuard(int one = 1) {
#if defined(__APPLE__)
    setenv("VECLIB_MAXIMUM_THREADS", "1", 1);
    setenv("ACCELERATE_MAX_THREADS", "1", 1);
#endif
#if defined(__unix__) || defined(__APPLE__)
    ob_get       = reinterpret_cast<OB_get_t>( dlsym(RTLD_DEFAULT, "openblas_get_num_threads") );
    ob_set       = reinterpret_cast<OB_set_t>( dlsym(RTLD_DEFAULT, "openblas_set_num_threads") );
    ob_set_local = reinterpret_cast<OB_set_t>( dlsym(RTLD_DEFAULT, "openblas_set_num_threads_local") );
    mkl_get = reinterpret_cast<MKL_get_t>( dlsym(RTLD_DEFAULT, "mkl_get_max_threads") );
    if (!mkl_get)
      mkl_get = reinterpret_cast<MKL_get_t>( dlsym(RTLD_DEFAULT, "MKL_Get_Max_Threads") );
    mkl_set_local = reinterpret_cast<MKL_set_t>( dlsym(RTLD_DEFAULT, "mkl_set_num_threads_local") );
    if (!mkl_set_local)
      mkl_set_local = reinterpret_cast<MKL_set_t>( dlsym(RTLD_DEFAULT, "MKL_Set_Num_Threads_Local") );
    mkl_set_dynamic = reinterpret_cast<MKL_dyn_t>( dlsym(RTLD_DEFAULT, "mkl_set_dynamic") );
#endif
    if (ob_get)   saved_openblas = ob_get();
    if (mkl_get)  saved_mkl      = mkl_get();
    if (ob_set_local)      ob_set_local(one);
    else if (ob_set)       ob_set(one);
    if (mkl_set_local)     mkl_set_local(one);
    if (mkl_set_dynamic)   mkl_set_dynamic(0);
  }
  ~BLASThreadGuard() {
    if (saved_openblas > 0) {
      if (ob_set_local) ob_set_local(saved_openblas);
      else if (ob_set)  ob_set(saved_openblas);
    }
    if (saved_mkl > 0 && mkl_set_local) mkl_set_local(saved_mkl);
    if (mkl_set_dynamic) mkl_set_dynamic(1);
  }
};
inline void harden_omp_runtime_once() {
  omp_set_dynamic(0);
#if defined(_OPENMP) && (_OPENMP >= 201307)
  omp_set_max_active_levels(1);
#else
  omp_set_nested(0);
#endif
}
} // namespace detail_blas_guard
#endif // _OPENMP

// -------- local structs for caching --------
struct Cache {
  arma::mat UtU;   // r x r
  arma::vec Uty;   // r
  arma::mat Utx;   // r x p
  arma::vec Xty;   // p
  arma::mat XtX;   // p x p
  int n_i;
};

struct PrecompGen {
  int n_i = 0;
  std::vector<int> rows_ord;     // subject's rows, ordered by time
  std::vector<int> tim_ord;      // ordered time codes
  std::vector<int> met_ord;      // ordered method codes (may be -1)
  arma::mat X_i;                 // n_i x p
  arma::vec y_i;                 // n_i
  arma::mat Ueff;                // n_i x r_eff
  arma::mat Cinv;                // n_i x n_i (I if iid)
  // Precomputed with Cinv (no se scaling):
  arma::mat XTCX;   // p x p
  arma::vec XTCy;   // p
  arma::mat UTCX;   // r_eff x p
  arma::vec UTCy;   // r_eff
  arma::mat UCU;    // r_eff x r_eff
};

// Build per-subject blocks once (AR1 and/or extra Z)
static std::vector<PrecompGen>
  precompute_general_blocks(const arma::mat& X,
                            const arma::vec& y,
                            const BySubject& S,
                            const arma::mat& Z, bool has_extra, int qZ,
                            int nm_re, int nt_re, int nm_full, int nt_full,
                            bool use_ar1, double ar1_rho,
                            double /*eps*/) {
    const int m = static_cast<int>(S.rows.size());
    const int p = X.n_cols;
    const int r_base = 1 + (nm_re>0?nm_re:0) + (nt_re>0?nt_re:0);
    const int r_eff  = r_base + (has_extra ? qZ : 0);
    std::vector<PrecompGen> out(m);

#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
    for (int i=0; i<m; ++i) {
      const auto& rows_i = S.rows[i];
      const auto& met_i  = S.met[i];
      const auto& tim_i  = S.tim[i];
      const int n_i = static_cast<int>(rows_i.size());
      if (n_i == 0) continue;

      // order by time (NA last), stable within NA
      std::vector<int> ord(n_i);
      std::iota(ord.begin(), ord.end(), 0);
      std::stable_sort(ord.begin(), ord.end(), [&](int a, int b){
        int ta = tim_i[a], tb = tim_i[b];
        if (ta < 0 && tb < 0) return a < b;
        if (ta < 0) return false;
        if (tb < 0) return true;
        return ta < tb;
      });

      PrecompGen P;
      P.n_i = n_i;
      P.rows_ord.resize(n_i);
      P.tim_ord.resize(n_i);
      P.met_ord.assign(n_i, -1);
      P.X_i.set_size(n_i, p);
      P.y_i.set_size(n_i);

      for (int k = 0; k < n_i; ++k) {
        const int g = rows_i[ ord[k] ];
        P.rows_ord[k] = g;
        P.X_i.row(k) = X.row(g);
        P.y_i[k]     = y[g];
        P.tim_ord[k] = nt_full>0 ? tim_i[ ord[k] ] : -1;
        if (nm_full>0) P.met_ord[k] = met_i[ ord[k] ];
      }

      // Ueff = [1 | method dummies (nm_re) | time dummies (nt_re) | Zi?]
      arma::mat Ubase;
      build_U_base(P.met_ord, P.tim_ord, nm_re, nt_re, Ubase);
      if (has_extra) {
        arma::mat Zi;
        rows_take_to(Z, rows_i, Zi);
        // reorder rows to 'ord'
        arma::uvec ord_u = arma::conv_to<arma::uvec>::from(ord);
        Zi = Zi.rows(ord_u);
        P.Ueff.set_size(n_i, r_eff);
        P.Ueff.cols(0, r_base-1) = Ubase;
        P.Ueff.cols(r_base, r_eff-1) = Zi;
      } else {
        P.Ueff = std::move(Ubase);
      }

      // Cinv (AR1 or I), independent of se
      P.Cinv.zeros(n_i, n_i);
      if (use_ar1 && nt_full > 0) {
        make_Cinv_by_method(P.tim_ord, P.met_ord, nm_full, ar1_rho, P.Cinv);
      } else {
        P.Cinv.eye(n_i, n_i);
      }

      // Precompute with Cinv
      arma::mat CX = P.Cinv * P.X_i;
      P.XTCX = P.X_i.t() * CX;
      P.XTCy = P.X_i.t() * (P.Cinv * P.y_i);

      arma::mat CU = P.Cinv * P.Ueff;
      P.UTCX = P.Ueff.t() * (P.Cinv * P.X_i);      // or P.Ueff.t()*CX
      P.UTCy = P.Ueff.t() * (P.Cinv * P.y_i);
      P.UCU  = P.Ueff.t() * CU;

      out[i] = std::move(P);
    }
    return out;
  }

// =================== main entry ===================
// [[Rcpp::export]]
Rcpp::List ccc_vc_cpp(
    Rcpp::NumericMatrix Xr,
    Rcpp::NumericVector yr,
    Rcpp::IntegerVector subject,
    Rcpp::IntegerVector method,
    Rcpp::IntegerVector time,
    int nm, int nt,
    int max_iter = 200,
    double tol = 1e-6,
    double conf_level = 0.95,
    int ci_mode = 2,
    Rcpp::Nullable<Rcpp::NumericMatrix> Lr = R_NilValue,
    Rcpp::Nullable<Rcpp::NumericMatrix> auxDr = R_NilValue,
    Rcpp::Nullable<Rcpp::NumericMatrix> Zr = R_NilValue,
    bool use_ar1 = false,
    double ar1_rho = 0.0,
    bool include_subj_method = true,
    bool include_subj_time = true,
    double sb_zero_tol = 1e-10,
    bool eval_single_visit = false,
    Rcpp::Nullable<Rcpp::NumericVector> time_weights = R_NilValue
) {
#ifdef _OPENMP
#ifndef MATRIXCORR_NO_BLAS_GUARD
  detail_blas_guard::harden_omp_runtime_once();
  detail_blas_guard::BLASThreadGuard _guard_one_thread_blas(1);
#endif
#endif

  const int n = yr.size();
  if (Xr.nrow() != n) stop("nrow(X) must match length(y)");
  if (subject.size() != n) stop("length(subject) mismatch");
  if (method.size()  && method.size()!=n)  stop("length(method) mismatch");
  if (time.size()    && time.size()!=n)    stop("length(time) mismatch");

  arma::mat X(Xr.begin(), Xr.nrow(), Xr.ncol(), false);
  arma::vec y(yr.begin(), yr.size(), false);
  const int p = X.n_cols;

  // ---- parse time_weights
  std::vector<double> w_time;
  bool has_w_time = false;
  if (time_weights.isNotNull()) {
    Rcpp::NumericVector wR(time_weights.get());
    if (wR.size() > 0) {
      w_time.assign(wR.begin(), wR.end());
      // basic sanity: length must match nt when nt>=2 (we only use when >=2)
      if (nt >= 2 && static_cast<int>(w_time.size()) != nt) {
        Rcpp::stop("time_weights length (%d) must equal nt (%d) for this pair.",
                   static_cast<int>(w_time.size()), nt);
      }
      // normalise to sum=1 (R side already does it, but safe here)
      double s = 0.0;
      for (double v : w_time) {
        if (!std::isfinite(v) || v < 0.0) Rcpp::stop("time_weights must be finite and nonnegative.");
        s += v;
      }
      if (s <= 0.0) Rcpp::stop("time_weights sum must be positive.");
      for (double &v : w_time) v /= s;
      has_w_time = true;
    }
  }

  // Optional extra random effects Z
  arma::mat Z; int qZ = 0; bool has_extra = false;
  if (Zr.isNotNull()) {
    Rcpp::NumericMatrix Zrm = Zr.get();
    Z = arma::mat(Zrm.begin(), Zrm.nrow(), Zrm.ncol(), false);
    if ((int)Z.n_rows != n) stop("Zr must have n rows");
    qZ = (int)Z.n_cols;
    has_extra = (qZ > 0);
  }

  if (use_ar1) {
    if (!std::isfinite(ar1_rho)) stop("ar1_rho is NA/NaN.");
    if (std::fabs(ar1_rho) >= 0.999) stop("ar1_rho must be in (-0.999,0.999).");
    if (nt == 0) warning("use_ar1=TRUE but nt==0; AR(1) will be ignored.");
  }

  // subject indexing
  std::vector<int> subj_idx; int m = 0;
  reindex(subject, subj_idx, m, NA_INTEGER);
  BySubject S = group_by_subject(subj_idx, method, time, m);

  // Included random blocks
  const int nm_re = include_subj_method ? nm : 0;
  const int nt_re = include_subj_time ? nt : 0;

  // EM init
  double sa  = 1.0;
  double sab = (nm_re>0 ? 0.5 : 0.0);
  double sag = (nt_re>0 ? 0.5 : 0.0);
  double se  = 1.0;
  arma::vec tau2; if (has_extra) tau2 = arma::vec(qZ, arma::fill::value(0.5));
  const double eps = 1e-10;
  arma::vec beta(p, arma::fill::zeros);

  const int r = 1 + (nm_re>0?nm_re:0) + (nt_re>0?nt_re:0);
  const int r_eff = r + (has_extra ? qZ : 0);

  // ---------- Precompute invariants ----------
  // IID residual path cache
  std::vector<Cache> C_iid;
  if (!(use_ar1 || has_extra)) {
    C_iid.resize(m);
    for (int i=0; i<m; ++i) {
      const auto& rows = S.rows[i];
      const auto& met  = S.met[i];
      const auto& tim  = S.tim[i];
      const int   n_i  = (int)rows.size();
      C_iid[i].n_i = n_i;

      C_iid[i].UtU.set_size(r,r);
      gram_UtU(met, tim, n_i, nm_re, nt_re, C_iid[i].UtU);

      C_iid[i].Uty.set_size(r);
      accumulate_Ut_vec(rows, met, tim, nm_re, nt_re, [&](int idx){ return y[idx]; }, C_iid[i].Uty);

      C_iid[i].Utx.set_size(r, p);
      for (int k=0;k<p;++k) {
        arma::vec tmp(r, arma::fill::zeros);
        accumulate_Ut_vec(rows, met, tim, nm_re, nt_re, [&](int idx){ return X(idx,k); }, tmp);
        C_iid[i].Utx.col(k) = tmp;
      }

      C_iid[i].Xty.set_size(p);
      C_iid[i].XtX.zeros(p,p);
      for (int k=0; k<p; ++k) {
        double sxy = 0.0;
        for (int idx : rows) sxy += X(idx,k) * y[idx];
        C_iid[i].Xty[k] = sxy;
        for (int l=k; l<p; ++l) {
          double sxx = 0.0;
          for (int idx : rows) sxx += X(idx,k) * X(idx,l);
          C_iid[i].XtX(k,l) = sxx;
          if (l!=k) C_iid[i].XtX(l,k) = sxx;
        }
      }
    }
  }

  // General path cache: AR(1) and/or extra Z
  std::vector<PrecompGen> PG;
  if (use_ar1 || has_extra) {
    PG = precompute_general_blocks(X, y, S, Z, has_extra, qZ,
                                   nm_re, nt_re, nm, nt,
                                   use_ar1, ar1_rho, eps);
  }

  // delta-method accumulators
  std::vector<double> sa_term(m, 0.0);
  std::vector<double> sab_term(m, 0.0);
  std::vector<double> sag_term(m, 0.0);
  std::vector<double> se_term(m, 0.0);

  // ===================== EM iterations =====================
  for (int iter=0; iter<max_iter; ++iter) {

    // (1) Assemble XtViX, XtViy
    arma::mat XtViX(p,p,arma::fill::zeros);
    arma::vec XtViy(p, arma::fill::zeros);

    if (use_ar1 || has_extra) {
      const double inv_se = 1.0 / std::max(se, eps);

      arma::vec prior_prec(r_eff, fill::zeros);
      {
        int pos = 0;
        prior_prec[pos++] = 1.0 / std::max(sa,  eps);
        for (int l=0; l<nm_re; ++l) prior_prec[pos++] = 1.0 / std::max(sab, eps);
        for (int t=0; t<nt_re; ++t) prior_prec[pos++] = 1.0 / std::max(sag, eps);
        for (int j=0; j<qZ;    ++j) prior_prec[pos++] = 1.0 / std::max(tau2[j], eps);
      }

#ifdef _OPENMP
      int nthreads = omp_get_max_threads();
      std::vector<arma::mat> XtViX_tls(nthreads, arma::mat(p,p, arma::fill::zeros));
      std::vector<arma::vec> XtViy_tls(nthreads, arma::vec(p, arma::fill::zeros));
#pragma omp parallel for schedule(static)
      for (int i=0; i<m; ++i) {
        if (PG[i].n_i == 0) continue;
        int tid = omp_get_thread_num();
        arma::mat M(r_eff, r_eff, fill::zeros);
        M.diag() = prior_prec;
        M += inv_se * PG[i].UCU;

        arma::mat A(r_eff, 1+p);
        A.col(0)      = inv_se * PG[i].UTCy;
        A.cols(1, p)  = inv_se * PG[i].UTCX;
        arma::mat Zsol = solve_sympd_safe(M, A);

        arma::vec Z_y = Zsol.col(0);
        arma::mat Z_X = Zsol.cols(1, p);

        arma::mat XTRinvX = inv_se * PG[i].XTCX;
        arma::vec XTRinvY = inv_se * PG[i].XTCy;
        for (int k=0; k<p; ++k)
          XtViy_tls[tid][k] += XTRinvY[k] - dot(A.cols(1,p).col(k), Z_y);

        for (int k=0; k<p; ++k) for (int l=k; l<p; ++l) {
          double val = XTRinvX(k,l) - dot(A.cols(1,p).col(k), Z_X.col(l));
          XtViX_tls[tid](k,l) += val;
          if (l!=k) XtViX_tls[tid](l,k) += val;
        }
      }
      for (int t=0; t<nthreads; ++t) { XtViX += XtViX_tls[t]; XtViy += XtViy_tls[t]; }
#else
      for (int i=0; i<m; ++i) {
        if (PG[i].n_i == 0) continue;

        arma::mat M(r_eff, r_eff, fill::zeros);
        M.diag() = prior_prec;
        M += inv_se * PG[i].UCU;

        arma::mat A(r_eff, 1+p);
        A.col(0)      = inv_se * PG[i].UTCy;
        A.cols(1, p)  = inv_se * PG[i].UTCX;
        arma::mat Zsol = solve_sympd_safe(M, A);

        arma::vec Z_y = Zsol.col(0);
        arma::mat Z_X = Zsol.cols(1, p);

        arma::mat XTRinvX = inv_se * PG[i].XTCX;
        arma::vec XTRinvY = inv_se * PG[i].XTCy;

        for (int k=0; k<p; ++k)
          XtViy[k] += XTRinvY[k] - dot(A.cols(1,p).col(k), Z_y);

        for (int k=0; k<p; ++k) for (int l=k; l<p; ++l) {
          double val = XTRinvX(k,l) - dot(A.cols(1,p).col(k), Z_X.col(l));
          XtViX(k,l) += val; if (l!=k) XtViX(l,k) += val;
        }
      }
#endif

    } else {
      // IID path (uses cached sufficient stats)
      const double inv_se = 1.0 / std::max(se, eps);
#ifdef _OPENMP
      int nthreads = omp_get_max_threads();
      std::vector<arma::mat> XtViX_tls(nthreads, arma::mat(p,p, arma::fill::zeros));
      std::vector<arma::vec> XtViy_tls(nthreads, arma::vec(p, arma::fill::zeros));
#pragma omp parallel for schedule(static)
      for (int i=0; i<m; ++i) {
        const Cache& Ci = C_iid[i];
        if (Ci.n_i == 0) continue;
        int tid = omp_get_thread_num();

        arma::mat M(r,r,fill::zeros);
        M(0,0) = 1.0 / std::max(sa,  eps);
        int off = 1;
        if (nm_re>0) { for (int l=0; l<nm_re; ++l) M(off+l, off+l) = 1.0 / std::max(sab, eps); off += nm_re; }
        if (nt_re>0) { for (int t=0; t<nt_re; ++t) M(off+t, off+t) = 1.0 / std::max(sag, eps); }
        M += inv_se * Ci.UtU;

        arma::mat A(r, 1+p);
        A.col(0)    = Ci.Uty * inv_se;
        A.cols(1,p) = Ci.Utx * inv_se;
        arma::mat Zsol = solve_sympd_safe(M, A);

        arma::vec Z_y = Zsol.col(0);
        arma::mat Z_X = Zsol.cols(1,p);

        for (int k=0; k<p; ++k) {
          XtViy_tls[tid][k] += inv_se * (Ci.Xty[k] - dot(Ci.Utx.col(k), Z_y));
          for (int l=k; l<p; ++l) {
            double val = inv_se * (Ci.XtX(k,l) - dot(Ci.Utx.col(k), Z_X.col(l)));
            XtViX_tls[tid](k,l) += val;
            if (l!=k) XtViX_tls[tid](l,k) += val;
          }
        }
      }
      for (int t=0; t<nthreads; ++t) { XtViX += XtViX_tls[t]; XtViy += XtViy_tls[t]; }
#else
      for (int i=0; i<m; ++i) {
        const Cache& Ci = C_iid[i];
        if (Ci.n_i == 0) continue;

        arma::mat M(r,r,fill::zeros);
        M(0,0) = 1.0 / std::max(sa,  eps);
        int off = 1;
        if (nm_re>0) { for (int l=0; l<nm_re; ++l) M(off+l, off+l) = 1.0 / std::max(sab, eps); off += nm_re; }
        if (nt_re>0) { for (int t=0; t<nt_re; ++t) M(off+t, off+t) = 1.0 / std::max(sag, eps); }
        M += inv_se * Ci.UtU;

        arma::mat A(r, 1+p);
        A.col(0)    = Ci.Uty * inv_se;
        A.cols(1,p) = Ci.Utx * inv_se;
        arma::mat Zsol = solve_sympd_safe(M, A);

        arma::vec Z_y = Zsol.col(0);
        arma::mat Z_X = Zsol.cols(1,p);

        for (int k=0; k<p; ++k) {
          XtViy[k] += inv_se * (Ci.Xty[k] - dot(Ci.Utx.col(k), Z_y));
          for (int l=k; l<p; ++l) {
            double val = inv_se * (Ci.XtX(k,l) - dot(Ci.Utx.col(k), Z_X.col(l)));
            XtViX(k,l) += val; if (l!=k) XtViX(l,k) += val;
          }
        }
      }
#endif
    }

    // (2) GLS beta
    {
      arma::mat XtViX_inv;
      if (!inv_sympd_safe(XtViX_inv, XtViX)) {
        arma::mat XtViXj = XtViX;
        double base = 1.0;
        if (XtViX.n_rows > 0) {
          double tr = arma::trace(XtViX);
          if (std::isfinite(tr) && tr > 0.0) base = std::max(1.0, tr / XtViX.n_rows);
        }
        double lam = std::max(1e-12, 1e-8 * base);
        bool ok = false;
        for (int k=0; k<6 && !ok; ++k) {
          XtViXj = XtViX; XtViXj.diag() += lam;
          ok = arma::inv_sympd(XtViX_inv, XtViXj);
          lam *= 10.0;
        }
        if (!ok) XtViX_inv = arma::pinv(XtViX);
      }
      beta = XtViX_inv * XtViy;
    }

    // (3) M-step: update variances
    double sa_acc = 0.0, sab_acc = 0.0, sag_acc = 0.0;
    double se_sumsq = 0.0, se_trace = 0.0;
    arma::vec r_global = y - X * beta;
    arma::vec tau2_acc; if (has_extra) tau2_acc = arma::vec(qZ, arma::fill::zeros);

    if (use_ar1 || has_extra) {
      const double inv_se = 1.0 / std::max(se, eps);
      arma::vec prior_prec(r_eff, fill::zeros);
      {
        int pos = 0;
        prior_prec[pos++] = 1.0 / std::max(sa,  eps);
        for (int l=0; l<nm_re; ++l) prior_prec[pos++] = 1.0 / std::max(sab, eps);
        for (int t=0; t<nt_re; ++t) prior_prec[pos++] = 1.0 / std::max(sag, eps);
        for (int j=0; j<qZ;    ++j) prior_prec[pos++] = 1.0 / std::max(tau2[j], eps);
      }

#ifdef _OPENMP
      int nthreads2 = omp_get_max_threads();
      std::vector<double> sa_tls(nthreads2,0.0), sab_tls(nthreads2,0.0),
      sag_tls(nthreads2,0.0), ss_tls(nthreads2,0.0),
      tr_tls(nthreads2,0.0);
      std::vector<arma::vec> tau2_tls(nthreads2, arma::vec(qZ, arma::fill::zeros));
#pragma omp parallel for schedule(static)
      for (int i=0; i<m; ++i) {
        if (PG[i].n_i == 0) continue;
        int tid = omp_get_thread_num();

        arma::vec r_i(PG[i].n_i);
        for (int t=0; t<PG[i].n_i; ++t) r_i[t] = r_global[ PG[i].rows_ord[t] ];

        arma::mat M(r_eff, r_eff, fill::zeros);
        M.diag() = prior_prec;
        M += inv_se * PG[i].UCU;

        arma::vec Utr = inv_se * ( PG[i].Ueff.t() * (PG[i].Cinv * r_i) );
        arma::vec b_i = solve_sympd_safe(M, Utr);

        arma::mat Minv;
        inv_sympd_safe(Minv, M);

        arma::vec e = r_i - PG[i].Ueff * b_i;
        double quad = inv_se * arma::as_scalar(e.t() * (PG[i].Cinv * e));
        double trce = arma::trace(Minv * (inv_se * PG[i].UCU));

        ss_tls[tid] += quad;
        tr_tls[tid] += trce;
        se_term[i]   = se * (quad + trce) / std::max(1, PG[i].n_i);

        sa_tls[tid] += b_i[0]*b_i[0] + Minv(0,0);

        int pos = 1;
        if (nm_re>0) { for (int l=0; l<nm_re; ++l) { sab_tls[tid] += b_i[pos+l]*b_i[pos+l] + Minv(pos+l,pos+l); } pos += nm_re; }
        if (nt_re>0) { for (int t=0; t<nt_re; ++t) { sag_tls[tid] += b_i[pos+t]*b_i[pos+t] + Minv(pos+t,pos+t); } pos += nt_re; }
        if (has_extra) for (int j=0; j<qZ; ++j) tau2_tls[tid][j] += b_i[pos+j]*b_i[pos+j] + Minv(pos+j,pos+j);

        sa_term[i] = b_i[0]*b_i[0] + Minv(0,0);
        if (nm_re > 0) {
          double acc_m = 0.0;
          for (int l = 0; l < nm_re; ++l) acc_m += b_i[1 + l]*b_i[1 + l] + Minv(1 + l, 1 + l);
          sab_term[i] = acc_m / (double)nm_re;
        } else sab_term[i] = 0.0;
        if (nt_re > 0) {
          int base = 1 + (nm_re>0?nm_re:0);
          double acc_t = 0.0;
          for (int t = 0; t < nt_re; ++t) acc_t += b_i[base + t]*b_i[base + t] + Minv(base + t, base + t);
          sag_term[i] = acc_t / (double)nt_re;
        } else sag_term[i] = 0.0;
      }
      for (int t=0; t<nthreads2; ++t) {
        sa_acc  += sa_tls[t];
        sab_acc += sab_tls[t];
        sag_acc += sag_tls[t];
        se_sumsq+= ss_tls[t];
        se_trace+= tr_tls[t];
        if (has_extra) tau2_acc += tau2_tls[t];
      }
#else
      for (int i=0; i<m; ++i) {
        if (PG[i].n_i == 0) continue;

        arma::vec r_i(PG[i].n_i);
        for (int t=0; t<PG[i].n_i; ++t) r_i[t] = r_global[ PG[i].rows_ord[t] ];

        arma::mat M(r_eff, r_eff, fill::zeros);
        M.diag() = prior_prec;
        M += inv_se * PG[i].UCU;

        arma::vec Utr = inv_se * ( PG[i].Ueff.t() * (PG[i].Cinv * r_i) );
        arma::vec b_i = solve_sympd_safe(M, Utr);
        arma::mat Minv; inv_sympd_safe(Minv, M);

        arma::vec e = r_i - PG[i].Ueff * b_i;
        double quad = inv_se * arma::as_scalar(e.t() * (PG[i].Cinv * e));
        double trce = arma::trace(Minv * (inv_se * PG[i].UCU));
        se_sumsq += quad;
        se_trace += trce;
        se_term[i] = se * (quad + trce) / std::max(1, PG[i].n_i);

        sa_acc += b_i[0]*b_i[0] + Minv(0,0);
        int pos = 1;
        if (nm_re>0) { for (int l=0; l<nm_re; ++l) sab_acc += b_i[pos+l]*b_i[pos+l] + Minv(pos+l,pos+l); pos += nm_re; }
        if (nt_re>0) { for (int t=0; t<nt_re; ++t) sag_acc += b_i[pos+t]*b_i[pos+t] + Minv(pos+t,pos+t); pos += nt_re; }
        if (has_extra) for (int j=0; j<qZ; ++j) tau2_acc[j] += b_i[pos+j]*b_i[pos+j] + Minv(pos+j,pos+j);

        sa_term[i] = b_i[0]*b_i[0] + Minv(0,0);
        if (nm_re > 0) {
          double acc_m = 0.0;
          for (int l = 0; l < nm_re; ++l) acc_m += b_i[1 + l]*b_i[1 + l] + Minv(1 + l, 1 + l);
          sab_term[i] = acc_m / (double)nm_re;
        } else sab_term[i] = 0.0;
        if (nt_re > 0) {
          int base = 1 + (nm_re>0?nm_re:0);
          double acc_t = 0.0;
          for (int t = 0; t < nt_re; ++t) acc_t += b_i[base + t]*b_i[base + t] + Minv(base + t, base + t);
          sag_term[i] = acc_t / (double)nt_re;
        } else sag_term[i] = 0.0;
      }
#endif

    } else {
      // IID path
#ifdef _OPENMP
      int nthreads2 = omp_get_max_threads();
      std::vector<double> sa_tls(nthreads2,0.0), sab_tls(nthreads2,0.0),
      sag_tls(nthreads2,0.0), ss_tls(nthreads2,0.0),
      tr_tls(nthreads2,0.0);
#pragma omp parallel for schedule(static)
      for (int i=0; i<m; ++i) {
        const Cache& Ci = C_iid[i];
        if (Ci.n_i == 0) continue;
        int tid = omp_get_thread_num();

        arma::mat M(r,r, fill::zeros);
        M(0,0) = 1.0 / std::max(sa, eps);
        int off = 1;
        if (nm_re>0) { for (int l=0;l<nm_re;++l) M(off+l, off+l) = 1.0/std::max(sab,eps); off += nm_re; }
        if (nt_re>0) { for (int t=0;t<nt_re;++t) M(off+t, off+t) = 1.0/std::max(sag,eps); }
        M += (1.0/std::max(se,eps)) * Ci.UtU;

        arma::vec Utr = (Ci.Uty - Ci.Utx * beta) / std::max(se,eps);
        arma::vec b_i = solve_sympd_safe(M, Utr);
        arma::mat Minv; inv_sympd_safe(Minv, M);

        arma::vec r_i(Ci.n_i);
        for (int t=0; t<Ci.n_i; ++t) r_i[t] = r_global[ S.rows[i][t] ];

        arma::vec Ub(Ci.n_i, arma::fill::zeros);
        add_U_times(S.rows[i], S.met[i], S.tim[i], nm_re, nt_re, b_i, Ub);

        double ss = 0.0;
        for (int t=0; t<Ci.n_i; ++t) { double e = r_i[t] - Ub[t]; ss += e*e; }
        ss_tls[tid] += ss;
        double trce = arma::trace(Minv * Ci.UtU);
        tr_tls[tid] += trce;
        se_term[i] = (ss + trce) / std::max(1, Ci.n_i);

        sa_tls[tid] += b_i[0]*b_i[0] + Minv(0,0);
        int pos = 1;
        if (nm_re>0) { for (int l=0;l<nm_re;++l) sab_tls[tid] += b_i[pos+l]*b_i[pos+l] + Minv(pos+l,pos+l); pos += nm_re; }
        if (nt_re>0) { for (int t=0;t<nt_re;++t) sag_tls[tid] += b_i[pos+t]*b_i[pos+t] + Minv(pos+t, pos+t); }
      }
      for (int t=0;t<nthreads2;++t) {
        sa_acc+=sa_tls[t]; sab_acc+=sab_tls[t]; sag_acc+=sag_tls[t];
        se_sumsq+=ss_tls[t]; se_trace+=tr_tls[t];
      }
#else
      for (int i=0; i<m; ++i) {
        const Cache& Ci = C_iid[i];
        if (Ci.n_i == 0) continue;

        arma::mat M(r,r, fill::zeros);
        M(0,0) = 1.0 / std::max(sa, eps);
        int off = 1;
        if (nm_re>0) { for (int l=0;l<nm_re;++l) M(off+l, off+l) = 1.0/std::max(sab,eps); off += nm_re; }
        if (nt_re>0) { for (int t=0;t<nt_re;++t) M(off+t, off+t) = 1.0/std::max(sag,eps); }
        M += (1.0/std::max(se,eps)) * Ci.UtU;

        arma::vec Utr = (Ci.Uty - Ci.Utx * beta) / std::max(se,eps);
        arma::vec b_i = solve_sympd_safe(M, Utr);
        arma::mat Minv; inv_sympd_safe(Minv, M);

        arma::vec r_i(Ci.n_i);
        for (int t=0; t<Ci.n_i; ++t) r_i[t] = r_global[ S.rows[i][t] ];

        arma::vec Ub(Ci.n_i, arma::fill::zeros);
        add_U_times(S.rows[i], S.met[i], S.tim[i], nm_re, nt_re, b_i, Ub);

        double ss = 0.0; for (int t=0;t<Ci.n_i;++t) { double e = r_i[t] - Ub[t]; ss += e*e; }
        se_sumsq += ss;
        se_trace += arma::trace(Minv * Ci.UtU);

        sa_acc += b_i[0]*b_i[0] + Minv(0,0);
        int pos = 1;
        if (nm_re>0) { for (int l=0;l<nm_re;++l) sab_acc += b_i[pos+l]*b_i[pos+l] + Minv(pos+l, pos+l); pos += nm_re; }
        if (nt_re>0) { for (int t=0;t<nt_re;++t)  sag_acc += b_i[pos+t]*b_i[pos+t] + Minv(pos+t, pos+t); }
      }
#endif
    }

    // updates
    double sa_new   = std::max(sa_acc  / (double)m, eps);
    double sab_new  = (nm_re>0 ? std::max(sab_acc / (double)(m*nm_re), eps) : 0.0);
    double sag_new  = (nt_re>0 ? std::max(sag_acc / (double)(m*nt_re), eps) : 0.0);
    arma::vec tau2_new = tau2;
    if (has_extra) for (int j=0; j<qZ; ++j) tau2_new[j] = std::max(tau2_acc[j] / (double)m, eps);

    double se_new = std::max(
      (use_ar1 || has_extra)
      ? se * (se_sumsq + se_trace) / (double)n
    : (se_sumsq + se_trace) / (double)n,
    eps
    );

    double diff = std::fabs(sa_new - sa)
      + std::fabs(sab_new - sab)
      + std::fabs(sag_new - sag)
      + (has_extra ? arma::accu(arma::abs(tau2_new - tau2)) : 0.0)
      + std::fabs(se_new  - se);
      sa = sa_new; sab = sab_new; sag = sag_new; se = se_new; if (has_extra) tau2 = tau2_new;
      if (diff < tol) break;
  } // EM loop

  // ---------------- VarFix ----------------
  arma::mat XtViX_final(p,p,arma::fill::zeros);
  if (use_ar1 || has_extra) {
    const double inv_se = 1.0 / std::max(se, eps);
    arma::vec prior_prec(r_eff, fill::zeros);
    int pos = 0;
    prior_prec[pos++] = 1.0 / std::max(sa,  eps);
    for (int l=0; l<nm_re; ++l) prior_prec[pos++] = 1.0 / std::max(sab, eps);
    for (int t=0; t<nt_re; ++t) prior_prec[pos++] = 1.0 / std::max(sag, eps);
    for (int j=0; j<qZ;    ++j) prior_prec[pos++] = 1.0 / std::max(tau2[j], eps);

#ifdef _OPENMP
    int nthreads3 = omp_get_max_threads();
    std::vector<arma::mat> XtViX_tls2(nthreads3, arma::mat(p,p, arma::fill::zeros));
#pragma omp parallel for schedule(static)
    for (int i=0; i<m; ++i) {
      if (PG[i].n_i == 0) continue;
      int tid = omp_get_thread_num();
      arma::mat M(r_eff, r_eff, fill::zeros);
      M.diag() = prior_prec;
      M += inv_se * PG[i].UCU;

      arma::mat S_ux = inv_se * PG[i].UTCX;     // r_eff x p
      arma::mat Zx   = solve_sympd_safe(M, S_ux);
      arma::mat XTRinvX = inv_se * PG[i].XTCX;

      for (int k=0;k<p;++k) for (int l=k;l<p;++l) {
        double val = XTRinvX(k,l) - dot(S_ux.col(k), Zx.col(l));
        XtViX_tls2[tid](k,l) += val; if (l!=k) XtViX_tls2[tid](l,k) += val;
      }
    }
    for (int t=0; t<nthreads3; ++t) XtViX_final += XtViX_tls2[t];
#else
    for (int i=0; i<m; ++i) {
      if (PG[i].n_i == 0) continue;
      arma::mat M(r_eff, r_eff, fill::zeros);
      M.diag() = prior_prec;
      M += inv_se * PG[i].UCU;

      arma::mat S_ux = inv_se * PG[i].UTCX;
      arma::mat Zx   = solve_sympd_safe(M, S_ux);
      arma::mat XTRinvX = inv_se * PG[i].XTCX;

      for (int k=0;k<p;++k) for (int l=k;l<p;++l) {
        double val = XTRinvX(k,l) - dot(S_ux.col(k), Zx.col(l));
        XtViX_final(k,l) += val; if (l!=k) XtViX_final(l,k) += val;
      }
    }
#endif
  } else {
    const double inv_se_final = 1.0 / std::max(se, eps);
#ifdef _OPENMP
    int nthreads3 = omp_get_max_threads();
    std::vector<arma::mat> XtViX_tls2(nthreads3, arma::mat(p,p, arma::fill::zeros));
#pragma omp parallel for schedule(static)
    for (int i=0; i<m; ++i) {
      const Cache& Ci = C_iid[i];
      if (Ci.n_i == 0) continue;
      int tid = omp_get_thread_num();
      arma::mat M(r,r, fill::zeros);
      M(0,0) = 1.0 / std::max(sa, eps);
      int off = 1;
      if (nm_re>0) { for (int l=0;l<nm_re;++l) M(off+l, off+l) = 1.0/std::max(sab,eps); off += nm_re; }
      if (nt_re>0) { for (int t=0;t<nt_re;++t) M(off+t, off+t) = 1.0/std::max(sag,eps); }
      M += inv_se_final * Ci.UtU;
      arma::mat Zx = solve_sympd_safe(M, Ci.Utx * inv_se_final);
      for (int k=0;k<p;++k) for (int l=k;l<p;++l) {
        double val = inv_se_final * (Ci.XtX(k,l) - dot(Ci.Utx.col(k), Zx.col(l)));
        XtViX_tls2[tid](k,l) += val; if (l!=k) XtViX_tls2[tid](l,k) += val;
      }
    }
    for (int t=0; t<nthreads3; ++t) XtViX_final += XtViX_tls2[t];
#else
    for (int i=0; i<m; ++i) {
      const Cache& Ci = C_iid[i];
      if (Ci.n_i == 0) continue;
      arma::mat M(r,r, fill::zeros);
      M(0,0) = 1.0 / std::max(sa, eps);
      int off = 1;
      if (nm_re>0) { for (int l=0;l<nm_re;++l) M(off+l, off+l) = 1.0/std::max(sab,eps); off += nm_re; }
      if (nt_re>0) { for (int t=0;t<nt_re;++t) M(off+t, off+t) = 1.0/std::max(sag,eps); }
      M += inv_se_final * Ci.UtU;
      arma::mat Zx = solve_sympd_safe(M, Ci.Utx * inv_se_final);
      for (int k=0;k<p;++k) for (int l=k;l<p;++l) {
        double val = inv_se_final * (Ci.XtX(k,l) - dot(Ci.Utx.col(k), Zx.col(l)));
        XtViX_final(k,l) += val; if (l!=k) XtViX_final(l,k) += val;
      }
    }
#endif
  }

  arma::mat VarFix;
  if (!inv_sympd_safe(VarFix, XtViX_final)) Rcpp::stop("Failed to invert XtViX.");
  if (!VarFix.is_finite()) Rcpp::stop("VarFix is not finite");

  // -------- SB & CCC & CI --------
  double SB = 0.0, varSB = 0.0;
  if (nm > 0 && Lr.isNotNull() && auxDr.isNotNull()) {
    Rcpp::NumericMatrix Lrm = Rcpp::as<Rcpp::NumericMatrix>(Lr);
    Rcpp::NumericMatrix Drm = Rcpp::as<Rcpp::NumericMatrix>(auxDr);
    arma::mat L(Lrm.begin(), X.n_cols, Lrm.ncol(), false);
    arma::mat auxD(Drm.begin(), Drm.nrow(), Drm.ncol(), false);
    const double den = (double)nm * (double)(nm-1) * (double)std::max(nt,1);

    arma::vec difmed = L.t() * beta;
    arma::mat Afix   = L * auxD * L.t();
    double num = arma::as_scalar(difmed.t() * auxD * difmed) - arma::trace(Afix * VarFix);
    SB = std::max(num / den, 0.0);
    if (!std::isfinite(SB)) SB = 0.0;

    arma::mat AV = Afix * VarFix;
    double term1 = 2.0 * arma::trace(AV * AV);
    double term2 = 4.0 * arma::as_scalar(beta.t() * Afix * VarFix * Afix * beta);
    varSB = std::max((term1 + term2) / (den * den), 0.0);
    if (!std::isfinite(varSB) || varSB < 0.0) varSB = 0.0;
  }
  bool sb_fixed_zero = (!std::isfinite(SB) || SB <= sb_zero_tol);
  if (sb_fixed_zero) { SB = 0.0; varSB = 0.0; }

  // kappa factors for time-averaged CCC
  double kappa_g_bar = 1.0;
  double kappa_e_bar = 1.0;
  int    units = 0;

  if (!eval_single_visit) { // time-avg or weighted-avg
    if (nt > 0) {
      kappa_g_bar = 0.0;
      kappa_e_bar = 0.0;

      auto unit_kappas = [&](const std::vector<int>& times_obs)
        -> std::pair<double,double>
        {
          // unique observed time codes (0..nt-1), ignore negatives
          std::vector<int> tuniq;
          tuniq.reserve(times_obs.size());
          for (int t : times_obs) if (t >= 0) tuniq.push_back(t);
          if (tuniq.empty()) return {NAN, NAN};

          std::sort(tuniq.begin(), tuniq.end());
          tuniq.erase(std::unique(tuniq.begin(), tuniq.end()), tuniq.end());
          const int T = static_cast<int>(tuniq.size());

          if (!has_w_time) {
            const double kg = 1.0 / static_cast<double>(T);
            const double ke = use_ar1 ? kappa_e_equal_ar1(T, ar1_rho) : kg;
            return {kg, ke};
          }

          // build sub-weights for observed times and renormalize to 1
          std::vector<double> wsub(T, 0.0);
          double s = 0.0;
          for (int i = 0; i < T; ++i) { wsub[i] = w_time[tuniq[i]]; s += wsub[i]; }
          if (s <= 0.0) {
            const double kg = 1.0 / static_cast<double>(T);
            const double ke = use_ar1 ? kappa_e_equal_ar1(T, ar1_rho) : kg;
            return {kg, ke};
          }
          for (double &v : wsub) v /= s;

          const double kg = kappa_g_weighted(wsub);
          const double ke = use_ar1 ? kappa_e_weighted_ar1(wsub, ar1_rho) : kg;
          return {kg, ke};
        };

      for (int i = 0; i < m; ++i) {
        const auto& met_i = S.met[i];
        const auto& tim_i = S.tim[i];

        if (nm > 0) {
          for (int l = 0; l < nm; ++l) {
            std::vector<int> times_obs; times_obs.reserve(tim_i.size());
            for (size_t k = 0; k < tim_i.size(); ++k)
              if (met_i[k] == l && tim_i[k] >= 0) times_obs.push_back(tim_i[k]);

              auto kp = unit_kappas(times_obs);
              if (std::isfinite(kp.first) && std::isfinite(kp.second)) {
                kappa_g_bar += kp.first;
                kappa_e_bar += kp.second;
                ++units;
              }
          }
        } else { // no method factor: just per subject
          std::vector<int> times_obs; times_obs.reserve(tim_i.size());
          for (size_t k = 0; k < tim_i.size(); ++k)
            if (tim_i[k] >= 0) times_obs.push_back(tim_i[k]);

            auto kp = unit_kappas(times_obs);
            if (std::isfinite(kp.first) && std::isfinite(kp.second)) {
              kappa_g_bar += kp.first;
              kappa_e_bar += kp.second;
              ++units;
            }
        }
      }

      if (units > 0) {
        kappa_g_bar /= static_cast<double>(units);
        kappa_e_bar /= static_cast<double>(units);
      } else {
        kappa_g_bar = 1.0;
        kappa_e_bar = 1.0;
      }

      kappa_g_bar = clamp01(kappa_g_bar);
      kappa_e_bar = clamp01(kappa_e_bar);
    } else {
      // no time factor
      kappa_g_bar = 0.0;
      kappa_e_bar = 1.0;
    }
  }

  const double sab_eff = include_subj_method ? sab : 0.0;
  const double sag_eff = include_subj_time ? sag : 0.0;
  const double sag_bar = (nt > 0 ? kappa_g_bar * sag_eff : 0.0);
  const double se_bar  = kappa_e_bar * se;
  const double ccc = (sa + sag_bar) / (sa + sab_eff + sag_bar + SB + se_bar);

  // delta-method SE & CI
  double Nnum = sa + sag_bar;
  double Dden = sa + sab_eff + sag_bar + SB + se_bar;
  if (Dden < 1e-14) Dden = 1e-14;
  const double d_sa  = (sab_eff + SB + se_bar) / (Dden * Dden);
  const double d_sab = include_subj_method ? (-Nnum / (Dden * Dden)) : 0.0;
  const double d_sag = include_subj_time ? (kappa_g_bar * (sab_eff + SB + se_bar) / (Dden * Dden)) : 0.0;
  const double d_se  = -kappa_e_bar * Nnum / (Dden * Dden);
  const double d_SB  = sb_fixed_zero ? 0.0 : (-Nnum / (Dden * Dden));

  arma::mat Zdm;
  if (include_subj_method && include_subj_time) {
    Zdm.set_size(m, 3);
    for (int i = 0; i < m; ++i) { Zdm(i,0)=sa_term[i]; Zdm(i,1)=sab_term[i]; Zdm(i,2)=sag_term[i]; }
  } else if (include_subj_method && !include_subj_time) {
    Zdm.set_size(m, 2);
    for (int i = 0; i < m; ++i) { Zdm(i,0)=sa_term[i]; Zdm(i,1)=sab_term[i]; }
  } else if (!include_subj_method && include_subj_time) {
    Zdm.set_size(m, 2);
    for (int i = 0; i < m; ++i) { Zdm(i,0)=sa_term[i]; Zdm(i,1)=sag_term[i]; }
  } else {
    Zdm.set_size(m, 1);
    for (int i = 0; i < m; ++i) { Zdm(i,0)=sa_term[i]; }
  }
  arma::mat Sigma_vc;
  if (m < 2) {
    Sigma_vc.zeros(Zdm.n_cols, Zdm.n_cols);
  } else {
    if (Zdm.n_cols == 1u) {
      Sigma_vc.set_size(1,1);
      Sigma_vc(0,0) = sample_var(Zdm.col(0)) / (double)m;
    } else {
      Sigma_vc = arma::cov(Zdm) / (double)m;
    }
  }

  arma::vec se_vec(m);
  double n_total = 0.0;
  for (int i = 0; i < m; ++i) n_total += (double)(use_ar1||has_extra ? PG[i].n_i : C_iid[i].n_i);
  double w2sum = 0.0;
  for (int i = 0; i < m; ++i) {
    se_vec[i] = se_term[i];
    const int ni = (use_ar1||has_extra ? PG[i].n_i : C_iid[i].n_i);
    const double wi = ((double)ni) / std::max(1.0, n_total);
    w2sum += wi * wi;
  }
  double var_sehat = sample_var(se_vec) * w2sum;

  double var_ccc = 0.0;
  if (include_subj_method && include_subj_time) {
    arma::vec g(3); g[0]=d_sa; g[1]=d_sab; g[2]=d_sag;
    var_ccc += arma::as_scalar(g.t() * Sigma_vc * g);
  } else if (include_subj_method && !include_subj_time) {
    arma::vec g(2); g[0]=d_sa; g[1]=d_sab;
    var_ccc += arma::as_scalar(g.t() * Sigma_vc * g);
  } else if (!include_subj_method && include_subj_time) {
    arma::vec g(2); g[0]=d_sa; g[1]=d_sag;
    var_ccc += arma::as_scalar(g.t() * Sigma_vc * g);
  } else {
    var_ccc += d_sa * d_sa * Sigma_vc(0,0);
  }
  var_ccc += d_se * d_se * var_sehat;
  var_ccc += d_SB * d_SB * varSB;

  // Delta-method SE
  double se_ccc = std::sqrt(std::max(0.0, var_ccc));
  const double alpha = 1.0 - std::min(std::max(conf_level, 0.0), 1.0);
  const double z = R::qnorm(1.0 - 0.5 * alpha, 0.0, 1.0, 1, 0);

  // raw Wald CI (truncated)
  double lwr_raw = std::min(1.0, std::max(0.0, ccc - z * se_ccc));
  double upr_raw = std::min(1.0, std::max(0.0, ccc + z * se_ccc));

  // logit-stabilised CI
  const double eps_c = 1e-12;
  double c_mid = std::min(1.0 - eps_c, std::max(eps_c, ccc));
  double phi   = std::log(c_mid / (1.0 - c_mid));
  double se_phi = (se_ccc > 0.0) ? (se_ccc / std::max(eps_c, c_mid * (1.0 - c_mid))) : 0.0;
  double lo_phi = phi - z * se_phi;
  double hi_phi = phi + z * se_phi;
  auto expit = [](double x){ return 1.0 / (1.0 + std::exp(-x)); };
  double lwr_logit = expit(lo_phi);
  double upr_logit = expit(hi_phi);

  // auto rule to choose method
  int ci_mode_used = ci_mode;
  // 0=raw, 1=logit, 2=auto
  if (ci_mode == 2) {
    bool near_boundary = (ccc > 0.90 || ccc < 0.10);
    bool clipped_raw   = (lwr_raw <= eps_c || upr_raw >= 1.0 - eps_c);
    bool tight_near_bd = (se_ccc < 0.5 * std::min(ccc, 1.0 - ccc));
    if (near_boundary || clipped_raw || tight_near_bd) ci_mode_used = 1; else ci_mode_used = 0;
  }

  // select CI to report
  double lwr, upr;
  if (ci_mode_used == 1) { lwr = lwr_logit; upr = upr_logit; }
  else { lwr = lwr_raw; upr = upr_raw; }

  // -------- REML log-likelihood (uses precomp) --------
  const double two_pi = 2.0 * std::acos(-1.0);

  const double lg_sa  = std::log(std::max(sa,  eps));
  const double lg_sab = (nm_re>0 ? std::log(std::max(sab, eps)) : 0.0);
  const double lg_sag = (nt_re>0 ? std::log(std::max(sag, eps)) : 0.0);
  double sum_lg_tau = 0.0; if (has_extra) for (int j=0; j<qZ; ++j) sum_lg_tau += std::log(std::max(tau2[j], eps));

  double logdetG_one = 0.0;
  logdetG_one += lg_sa;
  if (nm_re>0) logdetG_one += nm_re * lg_sab;
  if (nt_re>0) logdetG_one += nt_re * lg_sag;
  logdetG_one += sum_lg_tau;

  double sum_logdetR  = 0.0;
  double sum_logdetM  = 0.0;
  arma::vec   XtViy_final(p, arma::fill::zeros);
  double      yTRVY_final  = 0.0;

  if (use_ar1 || has_extra) {
    const double inv_se = 1.0 / std::max(se, eps);

    arma::vec prior_prec(r_eff, fill::zeros);
    int pos2 = 0;
    prior_prec[pos2++] = 1.0 / std::max(sa,  eps);
    for (int l=0; l<nm_re; ++l) prior_prec[pos2++] = 1.0 / std::max(sab, eps);
    for (int t=0; t<nt_re; ++t) prior_prec[pos2++] = 1.0 / std::max(sag, eps);
    for (int j=0; j<qZ;    ++j) prior_prec[pos2++] = 1.0 / std::max(tau2[j], eps);

    const double lg_se = std::log(std::max(se, eps));
    for (int i=0; i<m; ++i) {
      if (PG[i].n_i == 0) continue;

      if (use_ar1 && nt > 0) {
        // R_i = se * C_i, and we already have Cinv_i = C_i^{-1} (by method)
        // log|R_i| = n_i*log(se) + log|C_i| = n_i*lg_se - log|Cinv_i|
        sum_logdetR += PG[i].n_i * lg_se - logdet_spd_safe(PG[i].Cinv);
      } else {
        sum_logdetR += PG[i].n_i * lg_se;
      }

      arma::mat M(r_eff, r_eff, fill::zeros);
      M.diag() = prior_prec;
      M += inv_se * PG[i].UCU;

      sum_logdetM += logdet_spd_safe(M);

      double yTRiny = inv_se * arma::as_scalar( PG[i].y_i.t() * (PG[i].Cinv * PG[i].y_i) );
      arma::vec S_uy = inv_se * PG[i].UTCy;
      arma::mat Minv; inv_sympd_safe(Minv, M);
      double corr = arma::as_scalar(S_uy.t() * (Minv * S_uy));
      yTRVY_final += (yTRiny - corr);

      arma::mat S_ux = inv_se * PG[i].UTCX;
      arma::vec tmpv = inv_se * PG[i].XTCy;
      XtViy_final += tmpv - S_ux.t() * (Minv * S_uy);
    }
  } else {
    for (int i=0; i<m; ++i) {
      const Cache& Ci = C_iid[i];
      if (Ci.n_i == 0) continue;

      sum_logdetR += Ci.n_i * std::log(std::max(se, eps));

      arma::mat M(r,r, fill::zeros);
      M(0,0) = 1.0 / std::max(sa,  eps);
      int off = 1;
      if (nm_re>0) { for (int l=0; l<nm_re; ++l) M(off+l, off+l) = 1.0/std::max(sab, eps); off += nm_re; }
      if (nt_re>0) { for (int t=0; t<nt_re; ++t) M(off+t, off+t) = 1.0/std::max(sag, eps); }
      M += (1.0/std::max(se, eps)) * Ci.UtU;

      sum_logdetM += logdet_spd_safe(M);

      arma::mat Minv; inv_sympd_safe(Minv, M);

      arma::vec S_uy = (1.0/std::max(se, eps)) * (Ci.Uty);

      double y2 = 0.0;
      for (int idx : S.rows[i]) { const double yi = y[idx]; y2 += yi * yi; }
      double yTRiny = (1.0/std::max(se, eps)) * y2;

      double corr = arma::as_scalar(S_uy.t() * (Minv * S_uy));
      yTRVY_final += (yTRiny - corr);

      arma::mat S_ux = (1.0/std::max(se, eps)) * Ci.Utx;
      arma::vec tmpv = (1.0/std::max(se, eps)) * Ci.Xty;
      XtViy_final += tmpv - S_ux.t() * (Minv * S_uy);
    }
  }

  double logdetXtViX = logdet_spd_safe(XtViX_final);
  double yPy = yTRVY_final - arma::as_scalar( XtViy_final.t() * (VarFix * XtViy_final) );
  double reml_loglik = -0.5 * (
    ((double)n - (double)p) * std::log(two_pi)
    + sum_logdetR
  + (double)m * logdetG_one
  + sum_logdetM
  + logdetXtViX
  + yPy
  );

  // AR(1) diagnostic (moments)
  double ar1_rho_mom  = NA_REAL;
  double ar1_pval     = NA_REAL;
  int    ar1_pairs    = 0;
  bool   ar1_recommend = false;
  if (!use_ar1 && nt > 0) {
    arma::mat Ubase, Ueff, Zi, M;
    arma::mat X_i;
    arma::vec y_i, r_i, Utr, b_i, e;
    std::vector<double> num_m(nm, 0.0), den1_m(nm, 0.0), den2_m(nm, 0.0);
    std::vector<int>    pairs_m(nm, 0);
    for (int i = 0; i < m; ++i) {
      const auto& rows_i = S.rows[i];
      const auto& met_i  = S.met[i];
      const auto& tim_i  = S.tim[i];
      const int   n_i    = (int)rows_i.size();
      if (n_i <= 1) continue;

      std::vector<int> ord(n_i);
      std::iota(ord.begin(), ord.end(), 0);
      std::stable_sort(ord.begin(), ord.end(), [&](int a, int b){
        int ta = tim_i[a], tb = tim_i[b];
        if (ta < 0 && tb < 0) return a < b;
        if (ta < 0) return false;
        if (tb < 0) return true;
        return ta < tb;
      });

      X_i.set_size(n_i, X.n_cols);
      y_i.set_size(n_i);
      for (int k = 0; k < n_i; ++k) {
        int g = rows_i[ ord[k] ];
        X_i.row(k) = X.row(g);
        y_i[k]     = y[g];
      }
      std::vector<int> tim_ord(n_i, -1), met_ord(n_i, -1);
      for (int k = 0; k < n_i; ++k) {
        int ok = ord[k];
        tim_ord[k] = tim_i[ ok ];
        met_ord[k] = (nm > 0 ? met_i[ ok ] : -1);
      }
      const int r_base = 1 + (include_subj_method ? nm : 0) + (include_subj_time ? nt : 0);
      Ubase.zeros(n_i, r_base);
      for (int t = 0; t < n_i; ++t) Ubase(t,0) = 1.0;
      int col = 1;
      if (include_subj_method) {
        for (int t = 0; t < n_i; ++t) if (met_ord[t] >= 0) Ubase(t, col + met_ord[t]) = 1.0;
        col += nm;
      }
      if (include_subj_time) {
        for (int t = 0; t < n_i; ++t) if (tim_ord[t] >= 0) Ubase(t, col + tim_ord[t]) = 1.0;
      }
      Ueff = Ubase;

      M.zeros(Ueff.n_cols, Ueff.n_cols);
      int off = 0;
      M(off,off) = 1.0 / std::max(sa, 1e-12); off += 1;
      if (include_subj_method) { for (int l = 0; l < nm; ++l) M(off+l, off+l) = 1.0 / std::max(sab, 1e-12); off += nm; }
      if (include_subj_time)   { for (int t = 0; t < nt; ++t) M(off+t, off+t) = 1.0 / std::max(sag, 1e-12); off += nt; }
      M += (1.0 / std::max(se, 1e-12)) * (Ueff.t() * Ueff);

      r_i = y_i - X_i * beta;
      Utr = (1.0 / std::max(se, 1e-12)) * (Ueff.t() * r_i);
      b_i = solve_sympd_safe(M, Utr);
      e   = r_i - Ueff * b_i;

      for (int l = 0; l < nm; ++l) {
        std::vector<std::pair<int,int>> idx;
        idx.reserve(n_i);
        for (int k = 0; k < n_i; ++k)
          if (met_ord[k] == l && tim_ord[k] >= 0) idx.emplace_back(tim_ord[k], k);
          if ((int)idx.size() <= 1) continue;
          std::sort(idx.begin(), idx.end());
          for (size_t t = 0; t + 1 < idx.size(); ++t) {
            double a = e[ idx[t].second     ];
            double b = e[ idx[t+1].second   ];
            num_m[l]  += a * b;
            den1_m[l] += a * a;
            den2_m[l] += b * b;
            pairs_m[l] += 1;
          }
      }
    }
    double num_pool = 0.0, den1_pool = 0.0, den2_pool = 0.0; int pairs_pool = 0;
    for (int l = 0; l < nm; ++l) {
      if (pairs_m[l] >= 3 && den1_m[l] > 0.0 && den2_m[l] > 0.0) {
        num_pool   += num_m[l];
        den1_pool  += den1_m[l];
        den2_pool  += den2_m[l];
        pairs_pool += pairs_m[l];
      }
    }
    if (pairs_pool >= 3 && den1_pool > 0.0 && den2_pool > 0.0) {
      const double rho_pool = num_pool / std::sqrt(den1_pool * den2_pool);
      const double zt = rho_pool * std::sqrt((double)pairs_pool);
      const double pval = 2.0 * R::pnorm(-std::fabs(zt), 0.0, 1.0, 1, 0);
      const double sag_share = (include_subj_time ? sag / std::max(1e-12, sag + se) : 0.0);
      const double thr   = (sag_share > 0.25 ? 0.20 : 0.10);
      const double p_thr = (sag_share > 0.25 ? 0.01 : 0.05);
      ar1_rho_mom  = rho_pool;
      ar1_pval     = pval;
      ar1_pairs    = pairs_pool;
      ar1_recommend = (std::fabs(rho_pool) >= thr && pval < p_thr);
    } else {
      ar1_rho_mom = NA_REAL; ar1_pval = NA_REAL; ar1_pairs = 0; ar1_recommend = false;
    }
  }

  SEXP sigma2_extra = R_NilValue;
  if (has_extra) sigma2_extra = Rcpp::NumericVector(tau2.begin(), tau2.end());

  return Rcpp::List::create(
    _["sigma2_subject"]        = sa,
    _["sigma2_subject_method"] = sab,
    _["sigma2_subject_time"]   = sag,
    _["sigma2_error"]          = se,
    _["sigma2_extra"]          = sigma2_extra,
    _["SB"]                    = SB,
    _["beta"]                  = beta,
    _["varFix"]                = VarFix,
    _["ccc"]                   = ccc,
    _["lwr"]                   = lwr,
    _["upr"]                   = upr,
    _["lwr_raw"]               = lwr_raw,
    _["upr_raw"]               = upr_raw,
    _["lwr_logit"]             = lwr_logit,
    _["upr_logit"]             = upr_logit,
    _["ci_mode_input"]         = ci_mode,       // 0,1,2
    _["ci_mode_used"]          = ci_mode_used,  // 0 or 1
    _["ci_mode_label_used"]    = (ci_mode_used==1 ? "logit" : "raw"),
    _["se_ccc"]                = se_ccc,
    _["conf_level"]            = conf_level,
    _["reml_loglik"]           = reml_loglik,
    _["ar1_rho_lag1"]          = ar1_rho_mom,
    _["ar1_pairs"]             = ar1_pairs,
    _["ar1_pval"]              = ar1_pval,
    _["use_ar1"]               = ar1_recommend,
    _["include_subj_method"]   = include_subj_method,
    _["include_subj_time"]     = include_subj_time,
    _["sb_fixed_zero"]         = sb_fixed_zero
  );
}
