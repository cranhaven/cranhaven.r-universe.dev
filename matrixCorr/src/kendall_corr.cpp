// kendall_corr.cpp
// Thiago de Paula Oliveira
#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <vector>
#include "matrixCorr_detail.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

using namespace matrixCorr_detail;
using matrixCorr_detail::order_stats::insertion_sort_range;
using matrixCorr_detail::order_stats::getMs_ll;
using matrixCorr_detail::order_stats::inv_count_inplace;
using matrixCorr_detail::order_stats::tau_two_vectors_fast;

// ---------------------------- Matrix version ---------------------------------
// [[Rcpp::export]]
Rcpp::NumericMatrix kendall_matrix_cpp(Rcpp::NumericMatrix mat){
  using namespace matrixCorr_detail;
  using namespace matrixCorr_detail::order_stats;

  const int n = mat.nrow();
  const int p = mat.ncol();

  // --- Scalar fast path (p == 2): raw doubles, no discretisation
  if (p == 2) {
    const double* x = &mat(0, 0);
    const double* y = &mat(0, 1);
    const double tau = tau_two_vectors_fast(x, y, n);
    Rcpp::NumericMatrix out(2, 2);
    out(0,0) = out(1,1) = 1.0;
    out(0,1) = out(1,0) = tau;
    return out;
  }

  // --- Discretise once per column for p >= 3 (matrix path)
  const double scale = 1e8;
  std::vector< std::vector<long long> > cols(p, std::vector<long long>(n));
  for (int j = 0; j < p; ++j) {
    const double* cj = &mat(0, j);
    for (int i = 0; i < n; ++i)
      cols[j][i] = static_cast<long long>(std::floor(cj[i] * scale));
  }

  Rcpp::NumericMatrix out(p, p);
  for (int j = 0; j < p; ++j) out(j,j) = 1.0;

  // --- Avoid OpenMP overhead when p is tiny
#if defined(_OPENMP)
  if (p >= 3) {
#pragma omp parallel for schedule(static)
    for (int i = 0; i < p - 1; ++i) {
      // Precompute ord for column i (indices sorted by x_i)
      std::vector<int> ord(n);
      std::iota(ord.begin(), ord.end(), 0);
      std::sort(ord.begin(), ord.end(),
                [&](int a, int b){ return cols[i][a] < cols[i][b]; });

      // Precompute x-run boundaries for column i
      std::vector<std::pair<int,int>> xruns;
      xruns.reserve(64);
      for (int s = 0; s < n; ) {
        int e = s + 1;
        const long long xi = cols[i][ord[s]];
        while (e < n && cols[i][ord[e]] == xi) ++e;
        xruns.emplace_back(s, e);
        s = e;
      }

      auto tau_from_ord = [&](const std::vector<long long>& y) -> double {
        thread_local std::vector<long long> ybuf, mrg;
        if ((int)ybuf.capacity() < n) { ybuf.reserve(n); mrg.reserve(n); }
        ybuf.resize(n);
        mrg.resize(n);

        for (int k = 0; k < n; ++k) ybuf[k] = y[ord[k]];

        long long m1 = 0, s_acc = 0;
        for (const auto& run : xruns) {
          const int s = run.first, e = run.second, L = e - s;
          if (L > 1) {
            m1 += 1LL * L * (L - 1) / 2;
            if (L <= 32) insertion_sort_range(ybuf.data(), s, e);
            else         std::sort(ybuf.begin() + s, ybuf.begin() + e);
            s_acc += getMs_ll(ybuf.data() + s, L);
          }
        }

        const long long inv = inv_count_inplace<long long>(ybuf.data(), mrg.data(), n);
        const long long m2  = getMs_ll(ybuf.data(), n);

        const long long n0 = 1LL * n * (n - 1) / 2LL;
        const double    S   = double(n0) - double(m1) - double(m2)
          - 2.0 * double(inv) + double(s_acc);
        const double    den1 = double(n0 - m1);
        const double    den2 = double(n0 - m2);
        if (den1 <= 0.0 || den2 <= 0.0) return NA_REAL;
        return S / std::sqrt(den1 * den2);
      };

      for (int j = i + 1; j < p; ++j) {
        const double tau = tau_from_ord(cols[j]);
        out(i,j) = out(j,i) = tau;
      }
    }
  } else
#endif
{
  // single-threaded path
  for (int i = 0; i < p - 1; ++i) {
    std::vector<int> ord(n);
    std::iota(ord.begin(), ord.end(), 0);
    std::sort(ord.begin(), ord.end(),
              [&](int a, int b){ return cols[i][a] < cols[i][b]; });

    std::vector<std::pair<int,int>> xruns;
    xruns.reserve(64);
    for (int s = 0; s < n; ) {
      int e = s + 1;
      const long long xi = cols[i][ord[s]];
      while (e < n && cols[i][ord[e]] == xi) ++e;
      xruns.emplace_back(s, e);
      s = e;
    }

    thread_local std::vector<long long> ybuf, mrg;

    auto tau_from_ord = [&](const std::vector<long long>& y) -> double {
      if ((int)ybuf.capacity() < n) { ybuf.reserve(n); mrg.reserve(n); }
      ybuf.resize(n);
      mrg.resize(n);
      for (int k = 0; k < n; ++k) ybuf[k] = y[ord[k]];

      long long m1 = 0, s_acc = 0;
      for (const auto& run : xruns) {
        const int s = run.first, e = run.second, L = e - s;
        if (L > 1) {
          m1 += 1LL * L * (L - 1) / 2;
          if (L <= 32) insertion_sort_range(ybuf.data(), s, e);
          else         std::sort(ybuf.begin() + s, ybuf.begin() + e);
          s_acc += getMs_ll(ybuf.data() + s, L);
        }
      }

      const long long inv = inv_count_inplace<long long>(ybuf.data(), mrg.data(), n);
      const long long m2  = getMs_ll(ybuf.data(), n);

      const long long n0 = 1LL * n * (n - 1) / 2LL;
      const double    S   = double(n0) - double(m1) - double(m2)
        - 2.0 * double(inv) + double(s_acc);
      const double    den1 = double(n0 - m1);
      const double    den2 = double(n0 - m2);
      if (den1 <= 0.0 || den2 <= 0.0) return NA_REAL;
      return S / std::sqrt(den1 * den2);
    };

    for (int j = i + 1; j < p; ++j) {
      const double tau = tau_from_ord(cols[j]);
      out(i,j) = out(j,i) = tau;
    }
  }
}

  return out;
}

// ---------------------------- Two-vector wrapper ------------------------------
// [[Rcpp::export]]
double kendall_tau2_cpp(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  const R_xlen_t n = x.size();
  if (n != y.size() || n < 2) return NA_REAL;
  const double* px = x.begin();
  const double* py = y.begin();
  return matrixCorr_detail::order_stats::tau_two_vectors_fast(px, py, static_cast<int>(n));
}


// [[Rcpp::export]]
double kendall_tau2_from_mat_cpp(Rcpp::NumericMatrix mat) {
  if (mat.ncol() != 2 || mat.nrow() < 2) return NA_REAL;
  const int n = mat.nrow();
  const double* x = &mat(0, 0);
  const double* y = &mat(0, 1);
  return matrixCorr_detail::order_stats::tau_two_vectors_fast(x, y, n);
}
