#include <Rcpp.h>
#include <algorithm>
#include <vector>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;


namespace {

typedef long double accumulator;

const int block_columns = 8;

void r_cumsum(const double *v, int len, bool reverse, double *r) {
  accumulator s = 0.0;
  for (int t = 0; t < len; ++t) {
    const int k = reverse ? len - 1 - t : t;
    s += v[k];
    r[k] = (double)s;
  }
  if (!ISNAN((double)s)) return;
  bool nan = false, na = false;
  for (int t = 0; t < len; ++t) {
    const int k = reverse ? len - 1 - t : t;
    if (!nan) nan = ISNAN(v[k]);
    if (nan && !na) na = R_IsNA(v[k]);
    if (nan) r[k] = na ? NA_REAL : R_NaN;
  }
}

double r_sum_value(accumulator s) {
  if (s > DBL_MAX) return R_PosInf;
  if (s < -DBL_MAX) return R_NegInf;
  return (double)s;
}

bool check_window(int K, const IntegerVector &valid, const IntegerVector &lri, const IntegerVector &fri, int n) {
  const int nv = valid.size();
  if (K < 0) stop("Risk-window grid size must be nonnegative.");
  if (lri.size() != nv || fri.size() != nv) stop("Risk-window index vectors differ in length.");
  bool entry = false;
  for (int r = 0; r < nv; ++r) {
    if (valid[r] == NA_INTEGER || valid[r] < 1 || valid[r] > n || lri[r] == NA_INTEGER || lri[r] < 1 ||
        lri[r] > K || fri[r] == NA_INTEGER || fri[r] < 1 || fri[r] > K + 1)
      stop("Risk-window index out of range.");
    if (fri[r] != 1) entry = true;
  }
  return entry;
}

void check_events(const IntegerVector &eti, const IntegerVector &esi, int K, int n) {
  if (esi.size() != eti.size()) stop("Stage-1 event index vectors differ in length.");
  for (R_xlen_t e = 0; e < eti.size(); ++e)
    if (eti[e] == NA_INTEGER || eti[e] < 1 || eti[e] > K || esi[e] == NA_INTEGER || esi[e] < 1 || esi[e] > n)
      stop("Stage-1 event index out of range.");
}

// W: nv x m row-major weights; S: K x m row-major risk-window sums.
class RiskWindow {
 public:
  RiskWindow(int K, const IntegerVector &valid, const IntegerVector &lri, const IntegerVector &fri, int n)
      : K_(K), nv_(valid.size()), lri_(lri.begin()), fri_(fri.begin()), entry_(check_window(K, valid, lri, fri, n)) {}

  void sums(const double *W, int m, double *S) {
    out_.assign((size_t)K_ * m, 0.0);
    for (int r = 0; r < nv_; ++r) {
      double *o = &out_[(size_t)(lri_[r] - 1) * m];
      const double *w = W + (size_t)r * m;
      for (int c = 0; c < m; ++c) o[c] += w[c];
    }
    std::vector<accumulator> ao(m, 0.0), al(m, 0.0);
    if (entry_) {
      late_.assign((size_t)(K_ + 1) * m, 0.0);
      for (int r = 0; r < nv_; ++r) {
        double *l = &late_[(size_t)(fri_[r] - 1) * m];
        const double *w = W + (size_t)r * m;
        for (int c = 0; c < m; ++c) l[c] += w[c];
      }
      for (int c = 0; c < m; ++c) al[c] += late_[(size_t)K_ * m + c];
      for (int k = K_ - 1; k >= 0; --k) {
        const double *o = &out_[(size_t)k * m], *l = &late_[(size_t)k * m];
        double *s = S + (size_t)k * m;
        for (int c = 0; c < m; ++c) {
          ao[c] += o[c];
          s[c] = (double)ao[c] - (double)al[c];
          al[c] += l[c];
        }
      }
    } else {
      for (int k = K_ - 1; k >= 0; --k) {
        const double *o = &out_[(size_t)k * m];
        double *s = S + (size_t)k * m;
        for (int c = 0; c < m; ++c) {
          ao[c] += o[c];
          s[c] = (double)ao[c];
        }
      }
    }
    for (int c = 0; c < m; ++c)
      if (ISNAN((double)ao[c]) || ISNAN((double)al[c])) exact_column(W + c, m, S + c);
  }

 private:
  void exact_column(const double *w, int m, double *s) {
    std::vector<double> out(K_, 0.0), late(K_ + 1, 0.0), ro(K_), rl(K_ + 1);
    for (int r = 0; r < nv_; ++r) out[lri_[r] - 1] += w[(size_t)r * m];
    for (int r = 0; r < nv_; ++r) late[fri_[r] - 1] += w[(size_t)r * m];
    r_cumsum(out.data(), K_, true, ro.data());
    r_cumsum(late.data(), K_ + 1, true, rl.data());
    for (int k = 0; k < K_; ++k) s[(size_t)k * m] = ro[k] - rl[k + 1];
  }

  int K_, nv_;
  const int *lri_, *fri_;
  bool entry_;
  std::vector<double> out_, late_;
};

}  // namespace

// [[Rcpp::export(rng = false)]]
List cimehr_s1_score_hess(NumericVector rs, NumericMatrix X, IntegerVector valid, IntegerVector lri,
                          IntegerVector fri, int K, IntegerVector dN, IntegerVector eti, IntegerVector esi) {
  const int n = X.nrow(), p = X.ncol(), nv = valid.size();
  const R_xlen_t N = eti.size();
  if (rs.size() != n || dN.size() != K) stop("Stage-1 kernel inputs have inconsistent lengths.");
  RiskWindow window(K, valid, lri, fri, n);
  check_events(eti, esi, K, n);
  const double *x = X.begin();
  std::vector<int> pa, pb;
  for (int a = 0; a < p; ++a)
    for (int b = a; b < p; ++b) {
      pa.push_back(a);
      pb.push_back(b);
    }
  const int ncol = 1 + p + (int)pa.size();
  std::vector<double> rsx((size_t)nv * p);
  for (int r = 0; r < nv; ++r) {
    const int i = valid[r] - 1;
    for (int j = 0; j < p; ++j) rsx[(size_t)r * p + j] = rs[i] * x[i + (R_xlen_t)n * j];
  }
  NumericVector S0(K);
  NumericMatrix Xbar(K, p), H(p, p);
  double *xbar = Xbar.begin();
  std::vector<double> W, S;
  for (int start = 0; start < ncol; start += block_columns) {
    const int m = std::min(block_columns, ncol - start);
    W.resize((size_t)nv * m);
    S.resize((size_t)K * m);
    for (int r = 0; r < nv; ++r) {
      const int i = valid[r] - 1;
      const double *rx = &rsx[(size_t)r * p];
      double *w = &W[(size_t)r * m];
      for (int c = 0; c < m; ++c) {
        const int col = start + c;
        if (col == 0)
          w[c] = rs[i];
        else if (col <= p)
          w[c] = rx[col - 1];
        else
          w[c] = rx[pa[col - 1 - p]] * x[i + (R_xlen_t)n * pb[col - 1 - p]];
      }
    }
    window.sums(W.data(), m, S.data());
    for (int c = 0; c < m; ++c) {
      const int col = start + c;
      if (col == 0) {
        for (int k = 0; k < K; ++k) S0[k] = S[(size_t)k * m];
      } else if (col <= p) {
        double *xbj = xbar + (R_xlen_t)K * (col - 1);
        for (int k = 0; k < K; ++k) xbj[k] = S[(size_t)k * m + c] / S0[k];
      } else {
        const int a = pa[col - 1 - p], b = pb[col - 1 - p];
        const double *xba = xbar + (R_xlen_t)K * a, *xbb = xbar + (R_xlen_t)K * b;
        accumulator s = 0.0;
        for (int k = 0; k < K; ++k) {
          const double ratio = S[(size_t)k * m + c] / S0[k];
          const double cross = xba[k] * xbb[k];
          const double diff = ratio - cross;
          const double term = (double)dN[k] * diff;
          s += term;
        }
        const double h = -r_sum_value(s);
        H(a, b) = h;
        H(b, a) = h;
      }
    }
  }
  NumericVector score(p);
  for (int j = 0; j < p; ++j) {
    const double *xj = x + (R_xlen_t)n * j, *xbj = xbar + (R_xlen_t)K * j;
    accumulator s = 0.0;
    for (R_xlen_t e = 0; e < N; ++e) {
      const double d = xj[esi[e] - 1] - xbj[eti[e] - 1];
      s += d;
    }
    score[j] = (double)s;
  }
  return List::create(_["S0"] = S0, _["Xbar"] = Xbar, _["score"] = score, _["hessian"] = H);
}

// [[Rcpp::export(rng = false)]]
List cimehr_s1_subject_parts(NumericMatrix X, NumericMatrix Xbar, NumericVector dLambda, IntegerVector valid,
                             IntegerVector lri, IntegerVector fri, IntegerVector eti, IntegerVector esi) {
  const int n = X.nrow(), p = X.ncol(), K = Xbar.nrow(), nv = valid.size();
  const R_xlen_t N = eti.size();
  if (Xbar.ncol() != p || dLambda.size() != K) stop("Stage-1 subject inputs have inconsistent dimensions.");
  check_window(K, valid, lri, fri, n);
  check_events(eti, esi, K, n);
  NumericMatrix evt(n, p), AC(n, p);
  std::vector<double> prod(K), cum(K);
  for (int j = 0; j < p; ++j) {
    const double *xj = X.begin() + (R_xlen_t)n * j, *xbj = Xbar.begin() + (R_xlen_t)K * j;
    double *ev = evt.begin() + (R_xlen_t)n * j, *ac = AC.begin() + (R_xlen_t)n * j;
    for (R_xlen_t e = 0; e < N; ++e) {
      const double d = xj[esi[e] - 1] - xbj[eti[e] - 1];
      ev[esi[e] - 1] += d;
    }
    for (int k = 0; k < K; ++k) prod[k] = xbj[k] * dLambda[k];
    r_cumsum(prod.data(), K, false, cum.data());
    for (int r = 0; r < nv; ++r) {
      const double hi = cum[lri[r] - 1], lo = fri[r] > 1 ? cum[fri[r] - 2] : 0.0;
      ac[valid[r] - 1] = hi - lo;
    }
  }
  return List::create(_["evt_full"] = evt, _["A_C"] = AC);
}

// Risk-set contribution to apply_nu(D): event_part - risk_part.
// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_s1_apply_nu(NumericVector rs, NumericMatrix D, IntegerVector valid, IntegerVector lri,
                                 IntegerVector fri, NumericVector S0, NumericVector dLambda, IntegerVector eti,
                                 IntegerVector esi) {
  const int n = D.nrow(), q = D.ncol(), K = S0.size(), nv = valid.size();
  const R_xlen_t N = eti.size();
  if (rs.size() != n || dLambda.size() != K) stop("Stage-1 nu-chain inputs have inconsistent lengths.");
  RiskWindow window(K, valid, lri, fri, n);
  check_events(eti, esi, K, n);
  NumericMatrix ans(n, q);
  const double *d = D.begin();
  std::vector<double> W, V, ep, rp(n), prod(K), prefix(K);
  for (int start = 0; start < q; start += block_columns) {
    const int m = std::min(block_columns, q - start);
    W.resize((size_t)nv * m);
    V.resize((size_t)K * m);
    for (int r = 0; r < nv; ++r) {
      const int i = valid[r] - 1;
      for (int c = 0; c < m; ++c) W[(size_t)r * m + c] = rs[i] * d[i + (R_xlen_t)n * (start + c)];
    }
    window.sums(W.data(), m, V.data());
    for (int k = 0; k < K; ++k)
      for (int c = 0; c < m; ++c) V[(size_t)k * m + c] = V[(size_t)k * m + c] / S0[k];
    ep.assign((size_t)n * m, 0.0);
    for (R_xlen_t e = 0; e < N; ++e) {
      double *o = &ep[(size_t)(esi[e] - 1) * m];
      const double *v = &V[(size_t)(eti[e] - 1) * m];
      for (int c = 0; c < m; ++c) o[c] += v[c];
    }
    for (int c = 0; c < m; ++c) {
      for (int k = 0; k < K; ++k) prod[k] = V[(size_t)k * m + c] * dLambda[k];
      r_cumsum(prod.data(), K, false, prefix.data());
      std::fill(rp.begin(), rp.end(), 0.0);
      for (int r = 0; r < nv; ++r) {
        const double hi = prefix[lri[r] - 1], lo = fri[r] > 1 ? prefix[fri[r] - 2] : 0.0;
        const double width = hi - lo;
        rp[valid[r] - 1] = rs[valid[r] - 1] * width;
      }
      double *a = ans.begin() + (R_xlen_t)n * (start + c);
      for (int i = 0; i < n; ++i) a[i] = ep[(size_t)i * m + c] - rp[i];
    }
  }
  return ans;
}
