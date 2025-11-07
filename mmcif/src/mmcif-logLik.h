#ifndef MMCIF_LOGLIK_H
#define MMCIF_LOGLIK_H

#include "ghq.h"
#include "simple-mem-stack.h"
#include "param-indexer.h"
#include <array>

/// information about a given individual
struct mmcif_data {
  /// covariates for the trajectory
  double const * cov_trajectory;
  /// derivatives of x w.r.t. time
  double const * d_cov_trajectory;
  /// covariates for the risks
  double const * cov_risk;
  /// is the probability of the trajectory in (0, 1) (finite) or is it always 1
  bool has_finite_trajectory_prob;
  /**
   * the flag for the cause (zero-based). Censoring is indicated by letting this
   * be the number of causes.
   */
  unsigned cause;
  /**
   * covariates for the trajectory for possibly delayed entry. Use a nullptr
   * if there is no delayed entry
   */
  double const * cov_trajectory_delayed;

  bool has_delayed_entry() const {
    return cov_trajectory_delayed;
  }

  mmcif_data to_delayed(param_indexer const &indexer) const {
    return {
      cov_trajectory_delayed, nullptr,
      cov_risk, true, static_cast<unsigned>(indexer.n_causes()),
      nullptr
    };
  }

  mmcif_data without_delayed() const {
    mmcif_data cp = *this;
    cp.cov_trajectory_delayed = nullptr;
    return cp;
  }
};

/**
 * computes the log composite likelihood term of a given pair. The parameters
 * should be stored with the full covariance matrix.
 */
double mmcif_logLik
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   ghqCpp::simple_mem_stack<double> &mem, ghqCpp::ghq_data const &dat);

/**
 * computes the log of the marginal bivariate cumulative incidence function.
 * The last element indicates if the density should be taken in for either of
 * the outcomes.
 */
double mmcif_log_mcif
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   ghqCpp::simple_mem_stack<double> &mem, ghqCpp::ghq_data const &dat,
   std::array<bool, 2> const &derivs);

/**
 * overload of singletons.
 */
double mmcif_logLik
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem,
   ghqCpp::ghq_data const &dat);

/**
 * computes the log composite likelihood term of a given pair and the gradient.
 */
double mmcif_logLik_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   ghqCpp::simple_mem_stack<double> &mem, ghqCpp::ghq_data const &dat);

/**
 * overload for singletons.
 */
double mmcif_logLik_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem,
   ghqCpp::ghq_data const &dat);

/**
 * overload for singletons
 */
double mmcif_log_mcif
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem,
   ghqCpp::ghq_data const &dat, bool const deriv);

/// computes the log likelihood of the model without the random effects.
template<bool with_risk>
double mcif_logLik
  (double const * __restrict__ par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem);

/// computes the gradient of mcif_logLik
template<bool with_risk>
double mcif_logLik_grad
  (double const * __restrict__ par, double * __restrict__ grad,
   param_indexer const &indexer, mmcif_data const &obs,
   ghqCpp::simple_mem_stack<double> &mem);

#endif
