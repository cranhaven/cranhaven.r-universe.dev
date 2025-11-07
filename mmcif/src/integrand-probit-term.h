#ifndef INTEGRAND_PROBIT_TERM_H
#define INTEGRAND_PROBIT_TERM_H

#include "ghq.h"

namespace ghqCpp {

/**
 * computes the likelihood of single mixed probit term. That is the probability
 *
 *   g(u) = Phi((eta + z^T.u) / s)
 *
 * where u ~ N(0, Sigma). The function can easily be extended to more than one
 * probit factor if needed. The gradient is with respect to the scalar eta,
 * the scalar s and the vector z.
 */
template<bool comp_grad = false>
class mixed_probit_term final : public ghq_problem {
  double const s, eta;
  arma::vec const &z;

  size_t const v_n_vars = z.n_elem,
                v_n_out{comp_grad ? 3 + z.n_elem: 1};

public:
  mixed_probit_term(double const s, double const eta, arma::vec const &z);

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
