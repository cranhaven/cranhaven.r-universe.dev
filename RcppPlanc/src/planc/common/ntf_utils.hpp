#pragma once
/* Copyright 2017 Ramakrishnan Kannan */

#include "ncpfactors.hpp"
#include "tensor.hpp"

/**
 * Returns the khatri-rao product between two matrices.
 * @param[in] A is of size m x k matrix
 * @param[in]  B is of size n x k
 * @param[out] Returns C of size mn x k
 */
void khatrirao(const arma::mat &i_A, const arma::mat &i_B, arma::mat *o_C) {
  assert(i_A.n_cols == i_B.n_cols);
  arma::vec acol = arma::zeros<arma::vec>(i_A.n_rows);
  arma::vec bcol = arma::zeros<arma::vec>(i_B.n_rows);
  for (unsigned int i = 0; i < i_A.n_cols; i++) {
    acol = i_A.col(i);
    bcol = i_B.col(i);
    for (unsigned int j = 0; j < acol.n_rows; j++) {
      (*o_C)(arma::span(j * bcol.n_rows, (j + 1) * bcol.n_rows - 1), i) =
          acol(j) * bcol;
    }
  }
}
/**
 * Returns the kronecker product between two vectors
 * @param[in] acol column vector of size m
 * @param[in] bcol is column vector of size n
 * @param[out] output vector of size mn
 */
inline void kronecker(const arma::vec &i_acol, const arma::vec &i_bcol, arma::vec *o_c) {
  for (unsigned int j = 0; j < i_acol.n_rows; j++) {
    (*o_c).rows(arma::span(j * i_bcol.n_rows, (j + 1) * i_bcol.n_rows - 1)) =
        i_acol(j) * i_bcol;
  }
}
/**
 * Return the mttkrp of mode i_n of tensor X. That is., determine
 * the KRP leaving out i_n and multiply with mode i_n factor
 * @param[in] mode i_n
 * @param[in] Tensor X
 * @param[in] NCPFactors
 * @param[out] MTTKRP of factor mode i_n
 */

void mttkrp(const int i_n, const planc::Tensor &X, planc::NCPFactors &i_F,
            arma::mat *o_mttkrp) {
  arma::mat krp = i_F.krp_leave_out_one(i_n);
  X.mttkrp(i_n, krp, o_mttkrp);
}
