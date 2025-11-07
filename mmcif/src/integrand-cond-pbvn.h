#ifndef INTEGRAND_COND_PBVN_H
#define INTEGRAND_COND_PBVN_H

#include "ghq.h"

namespace ghqCpp {

/**
 * Computes
 *
 *   g(U) = Phi^{(2)}(0, eta + V.U, Psi)
 *
 * where Phi^{(2)} is the CDF of a bivariate normal distribution over the
 * negative orthant with the passed conditional mean and variance.
 *
 * The derivatives are computed w.r.t. eta, V and Psi.
 */
template<bool comp_grad = false>
class cond_pbvn final : public ghq_problem {
  arma::vec const &eta;
  arma::mat const &Psi;
  arma::mat const &V;

  size_t const v_n_vars = V.n_cols,
               v_n_out  = comp_grad ? 7 + V.n_elem : 1;

public:
  cond_pbvn(arma::vec const &eta, arma::mat const &Psi, arma::mat const &V);

  size_t n_vars() const { return v_n_vars; }
  size_t n_out() const { return v_n_out; }

  void eval
    (double const *points, size_t const n_points, double * __restrict__ outs,
     simple_mem_stack<double> &mem) const;

  double log_integrand
    (double const *point, simple_mem_stack<double> &mem) const;

  double log_integrand_grad
    (double const *point, double * __restrict__ grad,
     simple_mem_stack<double> &mem) const;

  void log_integrand_hess
    (double const *point, double *hess,
     simple_mem_stack<double> &mem) const;
};

} // namespace ghqCpp

#endif
