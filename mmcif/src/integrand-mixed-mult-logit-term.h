#ifndef INTEGRAND_MIXED_MULT_LOGIT_TERM_H
#define INTEGRAND_MIXED_MULT_LOGIT_TERM_H

#include "ghq.h"

namespace ghqCpp {

/**
 * computes the likelihood of a mixed multinomial term for a cluster. That is,
 * if there K groups, the probability that outcome i is in level k > 0 given the
 * random effects is
 *
 *   exp(eta[i, k - 1] + u[k - 1]) /
 *     (1 + sum_(j = 1)^K exp(eta[i, j - 1] + u[j - 1]))
 *
 *  and the probability of the reference level, k = 0, is
 *
 *    1 / (1 + sum_(j = 1)^K exp(eta[i, j - 1] + u[j - 1]))
 *
 *  If the gradient is required, the first output is the integral and the next
 *  elements are the gradients w.r.t. the eta matrix.
 */
template<bool comp_grad = false>
class mixed_mult_logit_term final : public ghq_problem {
  /**
   * the fixed offsets in the linear predictor stored
   * (K - 1) x <number of outcomes> matrix
   */
  arma::mat const &eta;
  // the category of each outcome. It is zero indexed
  arma::uvec const &which_category;

  size_t const v_n_vars = eta.n_rows,
               v_n_out{comp_grad ? 1 + eta.n_rows * eta.n_cols: 1};

public:
  mixed_mult_logit_term
    (arma::mat const &eta, arma::uvec const &which_category);

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

