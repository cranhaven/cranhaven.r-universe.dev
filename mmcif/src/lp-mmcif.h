#ifndef LP_MMCIF_H
#define LP_MMCIF_H

#include <R_ext/RS.h> // for F77_NAME and F77_CALL

#include <algorithm>
#include <numeric>
#include <vector>
#include <string>
#include <stdexcept>
#include "simple-mem-stack.h"

extern "C" {
  void F77_NAME(dgetrf)
    (int const * m, int const *n, double * A, int const * lda,
     int * ipiv, int *info);

  void F77_NAME(dgetrs)
    (char const * trans, int const * n, int const * nrhs, double const * A,
     int const * lda, int const * ipiv, double * B, int const *ldb, int * info,
     size_t);

  void F77_NAME(dgeqp3)
    (int const *m, int const *n, double *A, int const *lda,
     int *jpvt, double *tau, double *work, int const *lwork, int *info);

  void F77_NAME(dormqr)
    (char const *side, char const *trans, int const *m, int const *n,
     int const *k, double const *A, int const *lda, double const *tau,
     double *C, int const *ldc, double *work, int *lwork, int *info,
     size_t, size_t);
}

namespace lp_mmcif {

/// general solver
class general_lsolver {
  int n;
  double * A;
  std::vector<int> ipiv = std::vector<int>(static_cast<size_t>(n));

public:
  /// returns the required amount of memory to pass
  static int n_wk_mem(int const n){ return n * n; }

  /**
   * the working memory should be kept allocated as long as the object is
   * needed
   */
  general_lsolver(int const n, double const *Ain, int lda, double *wk_mem):
  n{n}, A{wk_mem} {
    if(n < 1)
      return;

    for(int j = 0; j < n; ++j)
      std::copy(Ain + j * lda, Ain + j * lda + n, A + j * n);

    int info{};
    F77_CALL(dgetrf)(&n, &n, A, &n, ipiv.data(), &info);
    if(info != 0)
      throw std::runtime_error
        ("dgetrf failed with code " + std::to_string(info));
  }

  general_lsolver(int const n, double const *Ain, double *wk_mem):
    general_lsolver(n, Ain, n, wk_mem) { }

  /// solves the linear equations overwriting the input
  void operator()
  (double *res, int const nrhs, bool const do_trans) const {
    if(n < 1)
      return;

    char const trans{do_trans ? 'T' : 'N'};
    int info{};
    F77_CALL(dgetrs)
      (&trans, &n, &nrhs, A, &n, ipiv.data(), res, &n, &info, 1);
    if(info != 0)
      throw std::runtime_error
      ("dgetrs failed with code " + std::to_string(info));
  }

  /// solves the linear equations
  void operator()
    (double const *org, double *res, int const nrhs,
     bool const do_trans) const {
    if(n < 1)
      return;
    std::copy(org, org + n * nrhs, res);

    (*this)(res, nrhs, do_trans);
  }
};

/**
 * simple matrix vector product y <- y + Xx where y is a k matrix and X is
 * k x n matrix.
 */
inline void mat_vec
(double * __restrict__ y, double const * __restrict__ X,
 double const * __restrict__ x, unsigned const k, unsigned const n){
  for(unsigned j = 0; j < n; ++j)
    for(unsigned i = 0; i < k; ++i)
      y[i] += *X++ * x[j];
}

/// copies the transpose of a sub-part of a matrix
inline void copy_transpose
  (double const *X, size_t const nrow_X, size_t const nrow_cp,
   size_t const ncol_cp, double * __restrict__ res){
  for(size_t j = 0; j < ncol_cp; ++j)
    for(size_t i = 0; i < nrow_cp; ++i)
      res[j + i * ncol_cp] = X[i + j * nrow_X];
}

/// performs the update X <- X + scalar * A.B or X <- X + scalar * A.B^T
inline void mat_mat_prod
  (double * __restrict__ X, size_t const nrow_X, double const *A,
   size_t const odim_A, double const *B, size_t const odim_B, size_t const cdim,
   double const scalar, bool const trans_B){
  if(trans_B){
    for(size_t k = 0; k < cdim; ++k)
      for(size_t j = 0; j < odim_B; ++j)
        for(size_t i = 0; i < odim_A; ++i)
          X[i + j * nrow_X] += scalar * A[i + k * odim_A] * B[j + k * odim_B];

  } else {
    for(size_t j = 0; j < odim_B; ++j)
      for(size_t k = 0; k < cdim; ++k)
        for(size_t i = 0; i < odim_A; ++i)
          X[i + j * nrow_X] += scalar * A[i + k * odim_A] * B[k + j * cdim];

  }
}

/**
 * Given a n x n matrix symmetric matrix S, the derivatives w.r.t.
 * Z = S[k1:k2, l1:l2]S[l1:l2, l1:l2]^(-1), dZ, and the derivatives w.r.t.
 * dS, this function updates dS backpropagating dZ.
 */
inline void backprop_cond_mean
  (double const *dZ, double const *S, double * __restrict__ dS,
   size_t const k1, size_t const k2, size_t const l1, size_t const l2,
   size_t const n, ghqCpp::simple_mem_stack<double> &mem){
  size_t const n_k{k2 - k1 + 1},
               n_l{l2 - l1 + 1};

  double * inv_mat_mem = mem.get(general_lsolver::n_wk_mem(n_l));
  general_lsolver solver(n_l, S + l1 * (1 + n), n, inv_mat_mem);

  double * dSk_trans{mem.get(n_k * n_l)};
  copy_transpose(dZ, n_k, n_k, n_l, dSk_trans);

  solver(dSk_trans, n_k, false);
  std::for_each(dSk_trans, dSk_trans + n_k * n_l, [](double &x) { x /= 2; });

  for(size_t k = 0; k < n_k; ++k)
    for(size_t l = 0; l < n_l; ++l){
      dS[l + l1 + (k + k1) * n] += dSk_trans[l + k * n_l];
      dS[k + k1 + (l + l1) * n] += dSk_trans[l + k * n_l];
    }

  double * Sk_part{mem.get(n_k * n_l)};
  for(size_t k = 0; k < n_k; ++k)
    std::copy(S + l1 + (k + k1) * n, S + n_l + l1 + (k + k1) * n,
              Sk_part + n_l * k);
  solver(Sk_part, n_k, false);

  double * outer_prod{mem.get(n_l * n_l)};
  std::fill(outer_prod, outer_prod + n_l * n_l, 0);

  mat_mat_prod(outer_prod, n_l, Sk_part  , n_l, dSk_trans, n_l, n_k, -1, true);

  for(size_t lj = 0; lj < n_l; ++lj)
    for(size_t li = 0; li < n_l; ++li)
      dS[li + l1 + (lj + l1) * n] +=
        outer_prod[li + lj * n_l] + outer_prod[lj + li * n_l];
}

/**
 * Given a n x n matrix symmetric matrix S, the derivatives w.r.t.
 * Z = S[k1:k2, k1:k2] - S[k1:k2, l1:l2]S[l1:l2, l1:l2]^(-1)S[l1:l2, k1:k2], dZ,
 * and the derivatives w.r.t. dS, this function updates dS backpropagating dZ.
 */
inline void backprop_cond_vcov
  (double const *dZ, double const *S, double * __restrict__ dS,
   size_t const k1, size_t const k2, size_t const l1, size_t const l2,
   size_t const n, ghqCpp::simple_mem_stack<double> &mem){
  size_t const n_k{k2 - k1 + 1},
               n_l{l2 - l1 + 1};

  for(size_t kj = 0; kj < n_k; ++kj)
    for(size_t ki = 0; ki < n_k; ++ki)
      dS[ki + k1 + (kj  + k1) * n] +=
        dZ[ki + n_k * kj];

  double * inv_mat_mem = mem.get(general_lsolver::n_wk_mem(n_l));
  general_lsolver solver(n_l, S + l1 * (1 + n), n, inv_mat_mem);

  double * Sk_part{mem.get(n_k * n_l)};
  for(size_t k = 0; k < n_k; ++k)
    std::copy
    (S + l1 + (k + k1) * n, S + n_l + l1 + (k + k1) * n, Sk_part + n_l * k);
  solver(Sk_part, n_k, false);

  double * dZ_Sk_part{mem.get(n_k * n_l)};
  std::fill(dZ_Sk_part, dZ_Sk_part + n_k * n_l, 0);
  mat_mat_prod(dZ_Sk_part, n_k, dZ, n_k, Sk_part, n_l, n_k, 1, true);

  for(size_t k = 0; k < n_k; ++k)
    for(size_t l = 0; l < n_l; ++l){
      dS[l + l1 + (k + k1) * n] -= dZ_Sk_part[k + l * n_k];
      dS[k + k1 + (l + l1) * n] -= dZ_Sk_part[k + l * n_k];
    }

  mat_mat_prod(dS + l1 * (1 + n), n, Sk_part, n_l, dZ_Sk_part, n_l,
               n_k, 1, false);
}

/**
 * Given a n x n matrix symmetric matrix S, the derivatives w.r.t.
 * Z = (V + S^(-1))^(-1), dZ, M = (V + S^(-1))^(-1)
 * and the derivatives w.r.t. dS, this function updates dS backpropagating dZ.
 */
inline void backprop_cond_vcov_rev
  (double const *dZ, double const *S, double const *M, double * __restrict__ dS,
   size_t const n, ghqCpp::simple_mem_stack<double> &mem){
  double * inv_mat_mem = mem.get(general_lsolver::n_wk_mem(n));
  general_lsolver solver(n, S, inv_mat_mem);

  double * lhs{mem.get(n * n)};
  std::copy(M, M + n * n, lhs);
  solver(lhs, n, false);

  double * middle{mem.get(n * n)};
  std::fill(middle, middle + n * n, 0);

  mat_mat_prod(middle, n, lhs, n, dZ, n, n, 1, false);
  mat_mat_prod(dS, n, middle, n, lhs, n, n, 1, true);
}

} // namespace lp_mmcif

#endif
