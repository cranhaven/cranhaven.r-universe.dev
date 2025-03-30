//
// Created by andrew on 3/21/2025.
//

#include "nnls_lib.inl"

#define X(T) \
template arma::mat planc::nnlslib<T, double>::runbppnnls(const arma::mat &C, const T &B, const int &ncores);
#include "../nmf/nmf_types.inc"
#undef X
template arma::mat planc::nnlslib<arma::mat>::bppnnls_prod(const arma::mat &CtC, const arma::mat &CtB, const int &ncores);
