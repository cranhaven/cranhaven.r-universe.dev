#include "integrand-probit-term.h"
#include "ghq-lp-utils.h"
#include <algorithm>
#include "pnorm.h"
#include "dnorm.h"

namespace ghqCpp {

template<bool comp_grad>
mixed_probit_term<comp_grad>::mixed_probit_term
  (double const s, double const eta, arma::vec const &z):
  s{s}, eta{eta}, z{z} { }

template<bool comp_grad>
void mixed_probit_term<comp_grad>::eval
  (double const *points, size_t const n_points, double * __restrict__ outs,
   simple_mem_stack<double> &mem) const {
  // compute the linear predictor
  double * const __restrict__ lps{mem.get(n_points)};
  std::fill(lps, lps + n_points, eta);
  for(size_t j = 0; j < n_vars(); ++j)
    for(size_t i = 0; i < n_points; ++i)
      lps[i] += points[i + j * n_points] * z[j];

  std::for_each(lps, lps + n_points, [&](double &x) { x /= s; });

  // set the output
  for(size_t i = 0; i < n_points; ++i, ++outs)
    *outs = pnorm_std(lps[i], 1, 0);

  if constexpr (comp_grad){
    double * const __restrict__ d_etas{outs};

    // the derivative w.r.t. eta and s
    for(size_t i = 0; i < n_points; ++i){
      d_etas[i] = std::exp(dnrm_log(lps[i])) / s;
      outs[i + n_points] = -d_etas[i] * lps[i];
    }
    outs += 2 * n_points;

    // the derivatives w.r.t. z
    for(size_t j = 0; j < n_vars(); ++j)
      for(size_t i = 0; i < n_points; ++i)
        outs[i + j * n_points] = d_etas[i] * points[i + j * n_points];
  }
}

template<bool comp_grad>
double mixed_probit_term<comp_grad>::log_integrand
  (double const *point, simple_mem_stack<double> &mem) const {
  double lp{eta};
  for(size_t i = 0; i < n_vars(); ++i)
    lp += point[i] * z[i];
  lp /= s;
  return pnorm_std(lp, 1, 1);
}

template<bool comp_grad>
double mixed_probit_term<comp_grad>::log_integrand_grad
  (double const *point, double * __restrict__ grad,
   simple_mem_stack<double> &mem) const {
  double lp{eta};
  for(size_t i = 0; i < n_vars(); ++i)
    lp += point[i] * z[i];
  lp /= s;
  double const log_pnrm{pnorm_std(lp, 1, 1)},
               log_dnrm{dnrm_log(lp)},
               d_lp{std::exp(log_dnrm - log_pnrm)};

  for(size_t i = 0; i < n_vars(); ++i)
    grad[i] = z[i] * d_lp / s;

  return log_pnrm;
}

template<bool comp_grad>
void mixed_probit_term<comp_grad>::log_integrand_hess
  (double const *point, double *hess,
   simple_mem_stack<double> &mem) const {
  double lp{eta};
  for(size_t i = 0; i < n_vars(); ++i)
    lp += point[i] * z[i];
  lp /= s;

  double const log_pnrm{pnorm_std(lp, 1, 1)},
               log_dnrm{dnrm_log(lp)},
                  ratio{std::exp(log_dnrm - log_pnrm)},
                d_lp_sq{-(lp * ratio + ratio * ratio)};

  for(size_t j = 0; j < n_vars(); ++j)
    for(size_t i = 0; i < n_vars(); ++i)
      hess[i + j * n_vars()] = z[i] * z[j] * d_lp_sq;

  std::for_each(hess, hess + n_vars() * n_vars(),
                [&](double &x){ x /= s * s; });
}

template class mixed_probit_term<false>;
template class mixed_probit_term<true>;

} // namespace ghqCpp
