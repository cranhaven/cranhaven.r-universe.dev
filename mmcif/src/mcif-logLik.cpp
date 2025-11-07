#include "mmcif-logLik.h"
#include "dnorm.h"
#include "pnorm.h"
#include "mmcif-misc.h"
#include <algorithm>

using namespace ghqCpp;

namespace {

class mcif_comp_helper {
  param_indexer const &indexer;
  double const * const par;

public:
  mcif_comp_helper(param_indexer const &indexer, double const *par):
  indexer{indexer}, par{par} { }

  bool is_censored(mmcif_data const &obs){
    return obs.cause == indexer.n_causes();
  }

  double comp_lp_traject
  (mmcif_data const &obs, unsigned const cause){
    double const *cov_trajectory
      {obs.cov_trajectory + indexer.cov_traject(cause)};
    return -std::inner_product
      (cov_trajectory, cov_trajectory + indexer.n_cov_traject(),
       par + indexer.traject(cause), 0.);
  }

  double comp_lp_traject(mmcif_data const &obs){
    return comp_lp_traject(obs, obs.cause);
  }

  void comp_lp_traject_backprop
    (double const grad_lp_traject, mmcif_data const &obs, unsigned const cause,
     double * __restrict__ grad){
    double const *cov_trajectory
      {obs.cov_trajectory + indexer.cov_traject(cause)};
    grad += indexer.traject(cause);
    auto const n_cov_traject = indexer.n_cov_traject();
    for(size_t i = 0; i < n_cov_traject; ++i)
      grad[i] -= cov_trajectory[i] * grad_lp_traject;
  }

  void comp_lp_traject_backprop
    (double const grad_lp_traject, mmcif_data const &obs,
     double * __restrict__ grad){
    comp_lp_traject_backprop(grad_lp_traject, obs, obs.cause, grad);
  }

  double comp_d_lp_traject
  (mmcif_data const &obs, unsigned const cause){
    double const * d_cov_trajectory
      {obs.d_cov_trajectory + indexer.cov_traject(cause)};
    return -std::inner_product
      (d_cov_trajectory, d_cov_trajectory + indexer.n_cov_traject(),
       par + indexer.traject(cause), 0.);
  }

  double comp_d_lp_traject(mmcif_data const &obs){
    return comp_d_lp_traject(obs, obs.cause);
  }

  void comp_d_lp_traject_backprop
    (double const grad_lp_traject, mmcif_data const &obs,
     double * __restrict__ grad){
    auto const cause = obs.cause;
    grad += indexer.traject(cause);
    auto const n_cov_traject = indexer.n_cov_traject();
    double const *d_cov_trajectory
      {obs.d_cov_trajectory + indexer.cov_traject(cause)};
    for(size_t i = 0; i < n_cov_traject; ++i)
      grad[i] -= d_cov_trajectory[i] * grad_lp_traject;
  }

  void fill_logit_offsets
  (double *eta, mmcif_data const &obs){
    for(size_t j = 0; j < indexer.n_causes(); ++j)
      eta[j] = std::inner_product
      (obs.cov_risk, obs.cov_risk + indexer.n_cov_risk(),
       par + indexer.risk(j), 0.);
  }

  void fill_logit_offsets_backprop
    (double const * d_logit_offsets, mmcif_data const & obs,
     double * __restrict__ grad){
    auto const n_cov_risk = indexer.n_cov_risk();
    auto const n_causes = indexer.n_causes();
    double const * cov_risk = obs.cov_risk;

    for(size_t j = 0; j < n_causes; ++j){
      auto grad_j = grad + indexer.risk(j);
      for(size_t i = 0; i < n_cov_risk; ++i)
        grad_j[i] += cov_risk[i] * d_logit_offsets[j];
    }
  }
};

} // namespace


template<bool with_risk>
double mcif_logLik
  (double const * __restrict__ par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem) {
  if(obs.has_delayed_entry()){
    double const delayed_term
      {mcif_logLik<with_risk>(par, indexer, obs.to_delayed(indexer), mem)};

    return
      mcif_logLik<with_risk>(par, indexer, obs.without_delayed(), mem)
      - delayed_term;
  }

  mcif_comp_helper helper{indexer, par};

  bool const is_censored{helper.is_censored(obs)};
  if(is_censored){
    if constexpr(!with_risk)
      return 0;

    auto const n_causes = indexer.n_causes();
    double * const logit_offsets{mem.get(2 * n_causes)},
           * const exp_logits{logit_offsets + n_causes};
    helper.fill_logit_offsets(logit_offsets, obs);

    std::transform(logit_offsets, logit_offsets + n_causes,
                   exp_logits, [](double const x) { return std::exp(x); });

    double const denom
      {std::accumulate(exp_logits, exp_logits + n_causes, 1.)};

    if(!obs.has_finite_trajectory_prob)
      return -std::log(denom);

    double likelihood{1};
    for(size_t cause = 0; cause < n_causes; ++cause){
      double const lp_traject{helper.comp_lp_traject(obs, cause)};
      likelihood -= ghqCpp::pnorm_std(lp_traject, 1, 0) *
        exp_logits[cause] / denom;
    }

    return std::log(likelihood);
  }

  double const lp_traject{helper.comp_lp_traject(obs)},
             d_lp_traject{helper.comp_d_lp_traject(obs)};

  double out{std::log(d_lp_traject) + ghqCpp::dnrm_log(lp_traject)};

  if constexpr(!with_risk)
    return out;

  auto const n_causes = indexer.n_causes();
  double * const logit_offsets{mem.get(n_causes)};
  helper.fill_logit_offsets(logit_offsets, obs);

  double const denom
    {
      std::accumulate(
          logit_offsets, logit_offsets + n_causes, 1.,
          [](double const sum, double const v){ return sum + std::exp(v); })
    };

  return out + logit_offsets[obs.cause] - std::log(denom);
}

template<bool with_risk>
double mcif_logLik_grad
  (double const * __restrict__ par, double * __restrict__ grad,
   param_indexer const &indexer, mmcif_data const &obs,
   ghqCpp::simple_mem_stack<double> &mem) {
  if(obs.has_delayed_entry()){
    double * const gr_delayed{mem.get(indexer.n_par_wo_vcov())};
    auto gr_marker = mem.set_mark_raii();
    std::fill(gr_delayed, gr_delayed + indexer.n_par_wo_vcov(), 0);

    double const delayed_term
      {mcif_logLik_grad<with_risk>
        (par, gr_delayed, indexer, obs.to_delayed(indexer), mem)};

    double const out
      {mcif_logLik_grad<with_risk>
        (par, grad, indexer, obs.without_delayed(), mem) - delayed_term};
    for(size_t i = 0; i < indexer.n_par_wo_vcov(); ++i)
      grad[i] -= gr_delayed[i];

    return out;
  }

  mcif_comp_helper helper{indexer, par};

  bool const is_censored{helper.is_censored(obs)};
  if(is_censored){
    if constexpr(!with_risk)
      return 0;

    auto const n_causes = indexer.n_causes();
    double * const logit_offsets{mem.get(3 * n_causes)},
           * const exp_logits{logit_offsets + n_causes},
           * const d_logit_offsets{exp_logits + n_causes};

    helper.fill_logit_offsets(logit_offsets, obs);

    std::transform(logit_offsets, logit_offsets + n_causes,
                   exp_logits, [](double const x) { return std::exp(x); });
    std::fill(d_logit_offsets, d_logit_offsets + n_causes, 0);

    double const denom{std::accumulate(exp_logits, exp_logits + n_causes, 1.)};

    if(!obs.has_finite_trajectory_prob){
      for(size_t cause = 0; cause < n_causes; ++cause)
        d_logit_offsets[cause] -= exp_logits[cause] / denom;

      helper.fill_logit_offsets_backprop(d_logit_offsets, obs, grad);
      return -std::log(denom);
    }

    double likelihood{1};

    double * const grads_lp_traject{mem.get(n_causes)};

    for(size_t cause = 0; cause < n_causes; ++cause){
      double const lp_traject{helper.comp_lp_traject(obs, cause)};
      double const pnrm{ghqCpp::pnorm_std(lp_traject, 1, 0)},
                   term{pnrm * exp_logits[cause] / denom};

      likelihood -= term;

      for(size_t denom_cause = 0; denom_cause < n_causes; ++denom_cause){
        if(denom_cause == cause)
          d_logit_offsets[denom_cause] -=
            term * (denom - exp_logits[denom_cause]) / denom;
        else
          d_logit_offsets[denom_cause] +=
            term * exp_logits[denom_cause] / denom;
      }

      grads_lp_traject[cause] =
        -std::exp(ghqCpp::dnrm_log(lp_traject)) * exp_logits[cause] / denom;

    }

    for(size_t cause = 0; cause < n_causes; ++cause){
      d_logit_offsets[cause] /= likelihood;
      helper.comp_lp_traject_backprop
        (grads_lp_traject[cause] / likelihood, obs, cause, grad);
    }

    helper.fill_logit_offsets_backprop(d_logit_offsets, obs, grad);

    return std::log(likelihood);
  }

  double const lp_traject{helper.comp_lp_traject(obs)},
             d_lp_traject{helper.comp_d_lp_traject(obs)};

  double out{std::log(d_lp_traject) + ghqCpp::dnrm_log(lp_traject)};

  helper.comp_d_lp_traject_backprop(1/d_lp_traject, obs, grad);
  helper.comp_lp_traject_backprop(-lp_traject, obs, grad);

  if constexpr(!with_risk)
    return out;

  auto const n_causes = indexer.n_causes();
  double * const logit_offsets{mem.get(3 * n_causes)},
         * const exp_logits{logit_offsets + n_causes},
         * const d_logit_offsets{exp_logits + n_causes};
  helper.fill_logit_offsets(logit_offsets, obs);

  std::transform(logit_offsets, logit_offsets + n_causes,
                 exp_logits, [](double const x) { return std::exp(x); });

  double const denom{std::accumulate(exp_logits, exp_logits + n_causes, 1.)};

  for(size_t cause = 0; cause < n_causes; ++cause)
    d_logit_offsets[cause] = -exp_logits[cause] / denom;
  d_logit_offsets[obs.cause] += 1;

  helper.fill_logit_offsets_backprop(d_logit_offsets, obs, grad);

  return out + logit_offsets[obs.cause] - std::log(denom);
}

template double mcif_logLik<false>
  (double const * __restrict__ par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem);
template double mcif_logLik<true>
  (double const * __restrict__ par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem);

template double mcif_logLik_grad<false>
  (double const * __restrict__ par, double * __restrict__ grad,
   param_indexer const &indexer, mmcif_data const &obs,
   ghqCpp::simple_mem_stack<double> &mem);
template double mcif_logLik_grad<true>
  (double const * __restrict__ par, double * __restrict__ grad,
   param_indexer const &indexer, mmcif_data const &obs,
   ghqCpp::simple_mem_stack<double> &mem);
