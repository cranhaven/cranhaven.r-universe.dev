#include <Rcpp.h>
#include <cfloat>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize ("fp-contract=off")
#endif

using namespace Rcpp;

static inline double ly_gamma_rsum_result(long double s) {
  if (s > DBL_MAX) return R_PosInf;
  if (s < -DBL_MAX) return R_NegInf;
  return (double)s;
}

// Returns only finite when FALSE; information is NULL when covariance is NULL.
// [[Rcpp::export(rng = false)]]
List ly_gamma_terms_cpp(NumericMatrix V, IntegerVector ei, IntegerVector em, NumericMatrix vb,
                        IntegerVector dn, Nullable<NumericVector> covariance,
                        NumericVector log_sum, NumericVector eta) {
  const R_xlen_t nl = log_sum.size();
  for (R_xlen_t k = 0; k < nl; ++k)
    if (!R_FINITE(log_sum[k])) return List::create(_["finite"] = false);
  const int n = V.nrow(), q = V.ncol(), K = vb.nrow(), qq = q * q, nk = dn.size();
  const R_xlen_t E = ei.size(), ne = eta.size();
  if (em.size() != E || vb.ncol() != q) stop("subscript out of bounds");
  NumericMatrix U(n, q);
  double *u = U.begin();
  const double *v = V.begin(), *m = vb.begin();
  for (R_xlen_t r = 0; r < E; ++r) {
    if (ei[r] == NA_INTEGER || em[r] == NA_INTEGER) stop("subscript out of bounds");
    const int i = ei[r] - 1, t = em[r] - 1;
    if (i < 0 || i >= n || t < 0 || t >= K || i >= ne) stop("subscript out of bounds");
    for (int a = 0; a < q; ++a) {
      const R_xlen_t ia = i + (R_xlen_t)a * n;
      u[ia] = (u[ia] + v[ia]) - m[t + (R_xlen_t)a * K];
    }
  }
  if (nk != nl) stop("subscript out of bounds");
  long double s1 = 0.0, s2 = 0.0;
  for (int k = 0; k < nk; ++k) {
    const double p = (dn[k] == NA_INTEGER ? NA_REAL : (double)dn[k]) * log_sum[k];
    s1 += p;
  }
  for (R_xlen_t r = 0; r < E; ++r) s2 += eta[ei[r] - 1];
  const double objective = ly_gamma_rsum_result(s1) - ly_gamma_rsum_result(s2);
  if (covariance.isNull())
    return List::create(_["finite"] = true, _["U"] = U, _["information"] = R_NilValue,
                        _["objective"] = objective);
  NumericVector cov(covariance.get());
  if (cov.size() < (R_xlen_t)qq * nk) stop("subscript out of bounds");
  NumericMatrix information(q, q);
  double *info = information.begin();
  const double *c = cov.begin();
  for (int k = 0; k < nk; ++k) {
    const double w = dn[k] == NA_INTEGER ? NA_REAL : (double)dn[k];
    const double *ck = c + (R_xlen_t)k * qq;
    for (int ij = 0; ij < qq; ++ij) info[ij] = info[ij] + w * ck[ij];
  }
  return List::create(_["finite"] = true, _["U"] = U, _["information"] = information,
                      _["objective"] = objective);
}
