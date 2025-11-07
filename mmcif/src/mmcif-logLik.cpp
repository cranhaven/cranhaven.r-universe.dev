#include "lp-mmcif.h"
#include "mmcif-logLik.h"
#include "integrand-mixed-mult-logit-term.h"
#include "integrand-probit-term.h"
#include "integrand-cond-pbvn.h"
#include <numeric>
#include <array>
#include "dnorm.h"
#include "mmcif-misc.h"

using namespace ghqCpp;

namespace {
constexpr size_t ghq_target_size{100};
constexpr double adaptive_rel_eps{1e-6};

class mmcif_comp_helper {
  param_indexer const &indexer;
  double const * const par;
  simple_mem_stack<double> &mem;

public:
  mmcif_comp_helper(param_indexer const &indexer, double const *par,
                    simple_mem_stack<double> &mem):
    indexer{indexer}, par{par}, mem{mem} { }

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

  void backprop_lp_traject
  (double const d_lp, mmcif_data const &obs, unsigned const cause,
   double * __restrict__ gr){
    double const * cov_trajectory
      {obs.cov_trajectory + indexer.cov_traject(cause)};
    for(size_t i = 0; i < indexer.n_cov_traject(); ++i)
      gr[indexer.traject(cause) + i] -= d_lp * cov_trajectory[i];
  }

  void backprop_lp_traject
  (double const d_lp, mmcif_data const &obs, double * __restrict__ gr){
    backprop_lp_traject(d_lp, obs, obs.cause, gr);
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

  void backprop_d_lp_traject
  (double const d_d_lp, mmcif_data const &obs, unsigned const cause,
   double * __restrict__ gr){
    double const *d_cov_trajectory
      {obs.d_cov_trajectory + indexer.cov_traject(cause)};
    for(size_t i = 0; i < indexer.n_cov_traject(); ++i)
      gr[indexer.traject(cause) + i] -= d_d_lp * d_cov_trajectory[i];
  }

  void backprop_d_lp_traject
  (double const d_d_lp, mmcif_data const &obs, double * __restrict__ gr){
    backprop_d_lp_traject(d_d_lp, obs, obs.cause, gr);
  }

  void fill_vcov(arma::mat &vcov){
    auto const vcov_dim = 2 * indexer.n_causes();
    vcov = mat_no_alloc(vcov_dim, vcov_dim, mem);
    std::copy(par + indexer.vcov(), par + indexer.n_par<false>(),
              vcov.begin());
  }

  void fill_vcov_rng_traject(arma::mat &vcov_sub, arma::mat const &vcov){
    auto const n_causes = indexer.n_causes();
    vcov_sub = mat_no_alloc(n_causes, n_causes, mem);
    vcov_sub = vcov.submat(0, 0, n_causes - 1, n_causes - 1);
  }

  double comp_trajector_cond_dens_obs_one
  (double const lp_traject, unsigned const cause){
    auto const n_causes = indexer.n_causes();
    double const *vcov{par + indexer.vcov()};
    auto const idx = cause + n_causes;
    double var{1 + vcov[idx + idx * 2 * n_causes]};
    return ghqCpp::dnrm_log(lp_traject, std::sqrt(var));
  }

  /**
   * computes the density along the derivative w.r.t. the first argument and
   * the variance
   */
  std::array<double, 3> comp_trajector_cond_dens_obs_one_w_grads
    (double const lp_traject, unsigned const cause){
    auto const n_causes = indexer.n_causes();
    double const *vcov{par + indexer.vcov()};
    auto const idx = cause + n_causes;
    double var{1 + vcov[idx + idx * 2 * n_causes]};
    return {
      ghqCpp::dnrm_log(lp_traject, std::sqrt(var)),
      - lp_traject / var,
      (lp_traject * lp_traject - var) / (2 * var * var) };
  }

  void fill_logit_offsets
  (double *eta, mmcif_data const &obs){
    for(size_t j = 0; j < indexer.n_causes(); ++j)
      eta[j] = std::inner_product
        (obs.cov_risk, obs.cov_risk + indexer.n_cov_risk(),
         par + indexer.risk(j), 0.);
  }

  void backprop_logit_offsets
  (double const * d_eta, mmcif_data const &obs, double * __restrict__ gr){
    for(size_t j = 0; j < indexer.n_causes(); ++j)
      for(size_t i = 0; i < indexer.n_cov_risk(); ++i)
        gr[indexer.risk(j) + i] += d_eta[j] * obs.cov_risk[i];
  }

  void fill_cond_vcov_one_obs(arma::mat &res, unsigned const cause){
    auto const n_causes = indexer.n_causes();
    auto const dim = 2 * n_causes;

    arma::mat Sigma
      (const_cast<double*>(par + indexer.vcov()), dim, dim, false);
    arma::mat Sigma_inv{mat_no_alloc(dim, dim, mem)};
    Sigma_inv = arma::inv_sympd(Sigma);

    auto const idx = cause + n_causes;
    Sigma_inv(idx, idx) += 1;

    res = mat_no_alloc(dim, dim, mem);
    res = arma::inv_sympd(Sigma_inv);
  }
};

double mmcif_univariate_mcif
  (double const * par, param_indexer const &indexer, mmcif_data const &obs,
   unsigned const cause, simple_mem_stack<double> &mem, ghq_data const &dat,
   arma::mat const &vcov, arma::mat const &vcov_sub,
   arma::mat const &logit_offsets){
  mmcif_comp_helper helper{indexer, par, mem};
  auto const n_causes = indexer.n_causes();

  arma::uvec const which_cat{cause == n_causes ? 0 : cause + 1};
  if(!obs.has_finite_trajectory_prob){
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
    rescale_problem<false> prob_use(vcov_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);
    double res{};
    ghq(&res, dat, prob, mem, ghq_target_size);
    return res;
  }

  size_t const idx_traject{n_causes + cause};
  arma::vec vcov_col = vcov.col(idx_traject).subvec(0, n_causes - 1);
  arma::vec rng_coefs = arma::solve(vcov_sub, vcov_col,
                                    arma::solve_opts::likely_sympd);
  double const var_cond
  {1 + vcov(idx_traject, idx_traject) - arma::dot(vcov_col, rng_coefs)};
  rng_coefs *= -1;
  double const lp_traject{helper.comp_lp_traject(obs, cause)};

  mixed_probit_term<false> probit_term
    (std::sqrt(var_cond), lp_traject, rng_coefs);
  mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);

  combined_problem comp_prob({&probit_term, &mult_term});
  rescale_problem<false> prob_use(vcov_sub, comp_prob);
  adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

  double res{};
  ghq(&res, dat, prob, mem, ghq_target_size);

  return res;
}

/// computes the log likelihood when both outcomes are observed
double mmcif_logLik_both_obs
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  mmcif_comp_helper helper{indexer, par, mem};

  double const lp_traject[]
    { helper.comp_lp_traject(obs1), helper.comp_lp_traject(obs2) };
  double const d_lp_traject[]
    { helper.comp_d_lp_traject(obs1), helper.comp_d_lp_traject(obs2) };

  double out{std::log(d_lp_traject[0]) + std::log(d_lp_traject[1])};

  auto const n_causes = indexer.n_causes();
  auto const idx_cause_1 = obs1.cause + n_causes;
  auto const idx_cause_2 = obs2.cause + n_causes;

  arma::mat vcov_cond_dens{mat_no_alloc(2, 2, mem)};
  arma::mat V{mat_no_alloc(2, 2 * n_causes, mem)};
  V.zeros();
  V(0, idx_cause_1) = 1;
  V(1, idx_cause_2) = 1;

  arma::mat vcov;
  helper.fill_vcov(vcov);

  arma::vec const dens_point{lp_traject[0], lp_traject[1]};

  {
    vcov_cond_dens = V * vcov * V.t();
    vcov_cond_dens.diag() += 1;
    out += log_dmvn(dens_point, vcov_cond_dens, mem);
  }

  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov_cond_inv{mat_no_alloc(2 * n_causes, 2 * n_causes, mem)};
  arma::mat vcov_cond    {mat_no_alloc(2 * n_causes, 2 * n_causes, mem)};
  vcov_cond_inv = arma::inv_sympd(vcov);
  vcov_cond_inv += V.t() * V;
  vcov_cond = arma::inv_sympd(vcov_cond_inv);

  arma::mat vcov_cond_sub{mat_no_alloc(n_causes, n_causes, mem)};
  vcov_cond_sub = vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1);

  arma::vec cond_mean = vcov_cond * V.t() * dens_point;
  cond_mean = cond_mean.subvec(0, n_causes - 1);

  arma::uvec const which_cat{obs1.cause + 1, obs2.cause + 1};

  auto mem_mark = mem.set_mark_raii();
  mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
  rescale_shift_problem<false>
    prob_use(vcov_cond_sub, cond_mean, mult_term);

  adaptive_problem prob(prob_use, mem, adaptive_rel_eps);
  double res{};
  ghq(&res, dat, prob, mem, ghq_target_size);
  out += std::log(res);

  return out;
}

/**
 * computes the log likelihood when the first outcome is observed but the
 * second is not
 */
double mmcif_logLik_one_obs
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  mmcif_comp_helper helper{indexer, par, mem};

  double const lp_traject1{helper.comp_lp_traject(obs1)};
  double const d_lp_traject1{helper.comp_d_lp_traject(obs1)};
  auto const cause1 = obs1.cause;

  double out{std::log(d_lp_traject1)};
  out += helper.comp_trajector_cond_dens_obs_one(lp_traject1, cause1);

  auto const n_causes = indexer.n_causes();
  auto const cause = obs1.cause;
  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov_cond;
  helper.fill_cond_vcov_one_obs(vcov_cond, cause);

  arma::vec cond_mean;
  cond_mean = vcov_cond.col(cause + n_causes) * lp_traject1;

  arma::mat const vcov_cond_sub
    {vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1)};
  arma::vec const cond_mean_sub{cond_mean.subvec(0, n_causes - 1)};

  if(!obs2.has_finite_trajectory_prob){
    auto mem_mark = mem.set_mark_raii();

    arma::uvec which_cat{cause + 1, 0};
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
    rescale_shift_problem<false> prob_use
      (vcov_cond_sub, cond_mean_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double integral_term{};
    ghq(&integral_term, dat, prob, mem, ghq_target_size);
    out += std::log(integral_term);
    return out;
  }

  auto mem_mark = mem.set_mark_raii();
  double integral_term{};
  {
    arma::mat logit_offset_obs1 = logit_offsets.col(0);
    arma::uvec which_cat{cause + 1};

    mixed_mult_logit_term<false> mult_term(logit_offset_obs1, which_cat);
    rescale_shift_problem<false> prob_use
      (vcov_cond_sub, cond_mean_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);
    ghq(&integral_term, dat, prob, mem, ghq_target_size);
  }

  arma::uvec which_cat{cause + 1, 0};
  arma::vec rng_coefs, vcov_col;
  for(size_t cause_2 = 0; cause_2 < n_causes; ++cause_2){
    which_cat[1] = cause_2 + 1;

    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);

    size_t const idx_traject{n_causes + cause_2};
    vcov_col = vcov_cond.col(idx_traject).subvec(0, n_causes - 1);

    rng_coefs = arma::solve
      (vcov_cond_sub, vcov_col, arma::solve_opts::likely_sympd);
    double const var_cond
      {1 + vcov_cond(idx_traject, idx_traject)
        - arma::dot(vcov_col, rng_coefs)};

    double const lp_traject2{helper.comp_lp_traject(obs2, cause_2)};
    double const shift_prob
      {lp_traject2 - cond_mean[idx_traject]
        + arma::dot(rng_coefs, cond_mean_sub)};

    rng_coefs *= -1;

    mixed_probit_term<false> probit_term
      (std::sqrt(var_cond), shift_prob, rng_coefs);
    combined_problem comp_prob({&probit_term, &mult_term});

    rescale_shift_problem<false> prob_use
      (vcov_cond_sub, cond_mean_sub, comp_prob);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double res{};
    ghq(&res, dat, prob, mem, ghq_target_size);
    integral_term -= res;
  }

  out += std::log(integral_term);
  return out;
}

/// computes the log likelihood when both outcomes are censored
double mmcif_logLik_both_cens
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  if(obs2.has_finite_trajectory_prob && !obs1.has_finite_trajectory_prob)
    return mmcif_logLik_both_cens(par, indexer, obs2, obs1, mem, dat);

  mmcif_comp_helper helper{indexer, par, mem};
  auto const n_causes = indexer.n_causes();

  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov, vcov_sub;
  helper.fill_vcov(vcov);
  helper.fill_vcov_rng_traject(vcov_sub, vcov);

  if(!obs1.has_finite_trajectory_prob){
    arma::uvec which_cat{0, 0};
    auto mem_marker = mem.set_mark_raii();
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
    rescale_problem<false> prob_use(vcov_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double integral{};
    ghq(&integral, dat, prob, mem, ghq_target_size);

    return std::log(integral);

  }
  else if(!obs2.has_finite_trajectory_prob){
    double integral{};
    auto mem_marker = mem.set_mark_raii();

    {
      arma::mat const logit_offsets_2 = logit_offsets.col(1);
      arma::uvec const which_cat{0};
      mixed_mult_logit_term<false> mult_term(logit_offsets_2, which_cat);
      rescale_problem<false> prob_use(vcov_sub, mult_term);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      ghq(&integral, dat, prob, mem, ghq_target_size);
    }

    arma::uvec which_cat{0, 0};
    arma::vec rng_coefs, vcov_col;
    for(size_t cause_1 = 0; cause_1 < n_causes; ++cause_1){
      which_cat[0] = cause_1 + 1;

      size_t const idx_traject{n_causes + cause_1};
      vcov_col = vcov.col(idx_traject).subvec(0, n_causes - 1);

      rng_coefs = arma::solve
        (vcov_sub, vcov_col, arma::solve_opts::likely_sympd);
      double const var_cond
        {1 + vcov(idx_traject, idx_traject) - arma::dot(vcov_col, rng_coefs)};

      double const lp_traject1{helper.comp_lp_traject(obs1, cause_1)};
      rng_coefs *= -1;

      mixed_probit_term<false> probit_term
        (std::sqrt(var_cond), lp_traject1, rng_coefs);
      mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
      combined_problem comp_prob({&probit_term, &mult_term});

      rescale_problem<false> prob_use(vcov_sub, comp_prob);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double res{};
      ghq(&res, dat, prob, mem, ghq_target_size);
      integral -= res;
    }

    return std::log(integral);
  }

  auto comp_main_terms = [&](mmcif_data const &obs){
    return std::exp(mmcif_logLik(par, indexer, obs, mem, dat)) - 1;
  };

  double integral{1};
  auto mem_marker1 = mem.set_mark_raii();
  integral += comp_main_terms(obs1);
  integral += comp_main_terms(obs2);

  // handle the interaction terms
  arma::mat V{mat_no_alloc(2, n_causes, mem)};
  arma::uvec which_cat(2);
  arma::vec dens_point(vec_no_alloc(2, mem));

  arma::mat pbvn_vcov{mat_no_alloc(2, 2, mem)};
  arma::mat rng_coefs{mat_no_alloc(2, n_causes, mem)};

  auto const vcov_dim = 2 * n_causes;
  arma::mat const cond_mean_loadings =
    arma::solve(vcov_sub,
                vcov.submat(0, n_causes, n_causes - 1, vcov_dim - 1),
                arma::solve_opts::likely_sympd).t();
  arma::mat const vcov_rng_cond_traject =
    vcov.submat(n_causes, n_causes, vcov_dim - 1, vcov_dim - 1)
    - vcov.submat(n_causes, 0, vcov_dim - 1, n_causes - 1) *
      cond_mean_loadings.t();

  auto mem_marker2 = mem.set_mark_raii();

  for(size_t cause_1 = 0; cause_1 < n_causes; ++cause_1){
    which_cat[0] = cause_1 + 1;
    dens_point[0] = -helper.comp_lp_traject(obs1, cause_1);

    for(size_t cause_2 = 0; cause_2 < n_causes; ++cause_2){
      which_cat[1] = cause_2 + 1;

      V.zeros();
      V(0, cause_1) = 1;
      V(1, cause_2) = 1;
      dens_point[1] = -helper.comp_lp_traject(obs2, cause_2);

      pbvn_vcov = V * vcov_rng_cond_traject * V.t();
      pbvn_vcov.diag() += 1;
      rng_coefs = V * cond_mean_loadings;

      cond_pbvn<false> pbvn_term(dens_point, pbvn_vcov, rng_coefs);
      mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
      combined_problem comp_prob({&pbvn_term, &mult_term});

      rescale_problem<false> prob_use(vcov_sub, comp_prob);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double res{};
      ghq(&res, dat, prob, mem, ghq_target_size);
      integral += res;
    }
  }

  return std::log(integral);
}

double mmcif_logLik_both_obs_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  mmcif_comp_helper helper{indexer, par, mem};

  double const lp_traject[]
    { helper.comp_lp_traject(obs1), helper.comp_lp_traject(obs2) };
  double const d_lp_traject[]
    { helper.comp_d_lp_traject(obs1), helper.comp_d_lp_traject(obs2) };

  double out{std::log(d_lp_traject[0]) + std::log(d_lp_traject[1])};

  helper.backprop_d_lp_traject(1/d_lp_traject[0], obs1, gr);
  helper.backprop_d_lp_traject(1/d_lp_traject[1], obs2, gr);

  auto const n_causes = indexer.n_causes();
  auto const idx_cause_1 = obs1.cause + n_causes;
  auto const idx_cause_2 = obs2.cause + n_causes;

  arma::mat vcov_cond_dens{mat_no_alloc(2, 2, mem)};
  arma::mat V{mat_no_alloc(2, 2 * n_causes, mem)};
  V.zeros();
  V(0, idx_cause_1) = 1;
  V(1, idx_cause_2) = 1;

  arma::mat vcov;
  helper.fill_vcov(vcov);

  arma::vec const dens_point{lp_traject[0], lp_traject[1]};
  arma::vec::fixed<2> gr_lp_traject{0, 0};

  arma::mat d_Sig(gr + indexer.vcov(), 2 * n_causes, 2 * n_causes, false, true);
  {
    vcov_cond_dens = V * vcov * V.t();
    vcov_cond_dens.diag() += 1;
    auto dmvn_res = log_dmvn_grad(dens_point, vcov_cond_dens, mem);
    out += dmvn_res.value;

    gr_lp_traject[0] = dmvn_res.d_x[0];
    gr_lp_traject[1] = dmvn_res.d_x[1];
    d_Sig += V.t() * dmvn_res.d_Sigma * V;
  }

  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov_cond_inv{mat_no_alloc(2 * n_causes, 2 * n_causes, mem)};
  arma::mat vcov_cond    {mat_no_alloc(2 * n_causes, 2 * n_causes, mem)};
  vcov_cond_inv = arma::inv_sympd(vcov);
  vcov_cond_inv += V.t() * V;
  vcov_cond = arma::inv_sympd(vcov_cond_inv);

  arma::mat vcov_cond_sub{mat_no_alloc(n_causes, n_causes, mem)};
  vcov_cond_sub = vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1);

  arma::vec cond_mean = vcov_cond * V.t() * dens_point;
  cond_mean = cond_mean.subvec(0, n_causes - 1);

  arma::uvec const which_cat{obs1.cause + 1, obs2.cause + 1};

  auto mem_mark = mem.set_mark_raii();
  mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
  rescale_shift_problem<true>
    prob_use(vcov_cond_sub, cond_mean, mult_term);
  adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

  double * ghq_res{mem.get(prob_use.n_out())};
  auto ghq_marker = mem.set_mark_raii();
  ghq(ghq_res, dat, prob, mem, ghq_target_size);

  double const integral{ghq_res[0]};
  out += std::log(integral);

  std::for_each(ghq_res + 1, ghq_res + prob_use.n_out(),
                [&](double &x){ x /= integral; });
  double * d_logit_offsets{ghq_res + 1};
  arma::vec d_cond_mean(d_logit_offsets + 2 * n_causes, n_causes, false, true);
  double * d_vcov_cond_sub{d_cond_mean.end()};

  helper.backprop_logit_offsets(d_logit_offsets           , obs1, gr);
  helper.backprop_logit_offsets(d_logit_offsets + n_causes, obs2, gr);

  gr_lp_traject += V * vcov_cond.cols(0, n_causes - 1) * d_cond_mean;

  helper.backprop_lp_traject(gr_lp_traject[0], obs1, gr);
  helper.backprop_lp_traject(gr_lp_traject[1], obs2, gr);

  arma::mat &d_vcov_cond = vcov_cond_inv;
  d_vcov_cond.zeros();

  arma::mat const mean_terms = (d_cond_mean / 2) * dens_point.t() * V;
  d_vcov_cond.rows(0, n_causes - 1) += mean_terms;
  d_vcov_cond.cols(0, n_causes - 1) += mean_terms.t();

  d_vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1) +=
    arma::mat(d_vcov_cond_sub, n_causes, n_causes, false);

  lp_mmcif::backprop_cond_vcov_rev
    (d_vcov_cond.memptr(), par + indexer.vcov(), vcov_cond.memptr(),
     d_Sig.memptr(), 2 * n_causes, mem);

  return out;
}

double mmcif_logLik_one_obs_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  mmcif_comp_helper helper{indexer, par, mem};

  double const lp_traject1{helper.comp_lp_traject(obs1)};
  double const d_lp_traject1{helper.comp_d_lp_traject(obs1)};
  auto const cause = obs1.cause;

  double out{std::log(d_lp_traject1)};
  helper.backprop_d_lp_traject(1/d_lp_traject1, obs1, gr);

  auto norm_dens_term = helper.comp_trajector_cond_dens_obs_one_w_grads
    (lp_traject1, cause);
  out += norm_dens_term[0];
  double gr_lp_traject1{norm_dens_term[1]};

  auto const n_causes = indexer.n_causes();
  auto const idx_cause = cause + n_causes;
  gr[indexer.vcov() + idx_cause + idx_cause * 2 * n_causes] += norm_dens_term[2];

  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov_cond;
  helper.fill_cond_vcov_one_obs(vcov_cond, cause);

  arma::vec cond_mean;
  cond_mean = vcov_cond.col(idx_cause) * lp_traject1;

  arma::mat const vcov_cond_sub
    {vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1)};
  arma::vec const cond_mean_sub{cond_mean.subvec(0, n_causes - 1)};

  if(!obs2.has_finite_trajectory_prob){
    auto mem_mark = mem.set_mark_raii();

    arma::uvec which_cat{cause + 1, 0};
    mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
    rescale_shift_problem<true> prob_use
      (vcov_cond_sub, cond_mean_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double * ghq_res{mem.get(prob_use.n_out())};
    auto ghq_mark = mem.set_mark_raii();
    ghq(ghq_res, dat, prob, mem, ghq_target_size);

    double const integral{ghq_res[0]};
    out += std::log(integral);
    std::for_each(ghq_res + 1, ghq_res + prob_use.n_out(),
                  [&](double &x){ x /= integral; });

    double * d_logit_offsets{ghq_res + 1};
    arma::vec d_cond_mean_sub(d_logit_offsets + 2 * n_causes, n_causes, false,
                              true);
    arma::mat d_vcov_cond_sub(d_cond_mean_sub.end(), n_causes, n_causes, false,
                              true);

    helper.backprop_logit_offsets(d_logit_offsets           , obs1, gr);
    helper.backprop_logit_offsets(d_logit_offsets + n_causes, obs2, gr);

    gr_lp_traject1 += arma::dot
      (d_cond_mean_sub,
       vcov_cond.col(idx_cause).subvec(0, n_causes - 1));
    helper.backprop_lp_traject(gr_lp_traject1, obs1, gr);

    arma::mat d_vcov_cond(mat_no_alloc(2 * n_causes, 2 * n_causes, mem));
    d_vcov_cond.zeros();

    d_cond_mean_sub *= lp_traject1 / 2;
    d_vcov_cond.submat(0, idx_cause, n_causes - 1, idx_cause) +=
      d_cond_mean_sub;
    d_vcov_cond.submat(idx_cause, 0, idx_cause, n_causes - 1) +=
      d_cond_mean_sub.t();

    d_vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_cond_sub;

    lp_mmcif::backprop_cond_vcov_rev
      (d_vcov_cond.memptr(), par + indexer.vcov(), vcov_cond.memptr(),
       gr + indexer.vcov(), 2 * n_causes, mem);

    return out;
  }

  helper.backprop_lp_traject(gr_lp_traject1, obs1, gr);

  double * const gr_integral{mem.get(indexer.n_par<false>())};
  std::fill(gr_integral, gr_integral + indexer.n_par<false>(), 0);
  auto mem_mark = mem.set_mark_raii();
  double integral_term{};
  {
    arma::mat logit_offset_obs1 = logit_offsets.col(0);
    arma::uvec which_cat{cause + 1};

    mixed_mult_logit_term<true> mult_term(logit_offset_obs1, which_cat);
    rescale_shift_problem<true> prob_use
      (vcov_cond_sub, cond_mean_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double * ghq_res{mem.get(prob_use.n_out())};
    auto ghq_marker = mem.set_mark_raii();
    ghq(ghq_res, dat, prob, mem, ghq_target_size);

    integral_term += ghq_res[0];

    double * d_logit_offsets{ghq_res + 1};
    arma::vec d_cond_mean_sub(d_logit_offsets + n_causes, n_causes, false,
                              true);
    arma::mat d_vcov_cond_sub(d_cond_mean_sub.end(), n_causes, n_causes, false,
                              true);

    helper.backprop_logit_offsets(d_logit_offsets, obs1, gr_integral);

    helper.backprop_lp_traject
      (arma::dot
         (d_cond_mean_sub,
          vcov_cond.col(idx_cause).subvec(0, n_causes - 1)),
       obs1, gr_integral);

    arma::mat d_vcov_cond(mat_no_alloc(2 * n_causes, 2 * n_causes, mem));
    d_vcov_cond.zeros();

    d_cond_mean_sub *= lp_traject1 / 2;
    d_vcov_cond.col(idx_cause).subvec(0, n_causes - 1) +=
      d_cond_mean_sub;
    d_vcov_cond.row(idx_cause).subvec(0, n_causes - 1) +=
      d_cond_mean_sub.t();

    d_vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_cond_sub;

    lp_mmcif::backprop_cond_vcov_rev
      (d_vcov_cond.memptr(), par + indexer.vcov(), vcov_cond.memptr(),
       gr_integral + indexer.vcov(), 2 * n_causes, mem);
  }

  std::for_each(gr_integral, gr_integral + indexer.n_par<false>(),
                [](double &x){ x *= -1; });

  arma::uvec which_cat{cause + 1, 0};
  arma::vec rng_coefs, vcov_col;
  for(size_t cause_2 = 0; cause_2 < n_causes; ++cause_2){
    which_cat[1] = cause_2 + 1;

    mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);

    size_t const idx_traject{n_causes + cause_2};
    vcov_col = vcov_cond.col(idx_traject).subvec(0, n_causes - 1);

    rng_coefs = arma::solve
      (vcov_cond_sub, vcov_col, arma::solve_opts::likely_sympd);
    double const var_cond
      {1 + vcov_cond(idx_traject, idx_traject)
        - arma::dot(vcov_col, rng_coefs)};

    double const lp_traject2{helper.comp_lp_traject(obs2, cause_2)};
    double const shift_prob
      {lp_traject2 - cond_mean[idx_traject]
        + arma::dot(rng_coefs, cond_mean_sub)};

    rng_coefs *= -1;

    mixed_probit_term<true> probit_term
      (std::sqrt(var_cond), shift_prob, rng_coefs);
    combined_problem comp_prob({&probit_term, &mult_term});

    rescale_shift_problem<true> prob_use
      (vcov_cond_sub, cond_mean_sub, comp_prob);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double *ghq_res{mem.get(prob_use.n_out())};
    auto ghq_marker = mem.set_mark_raii();
    ghq(ghq_res, dat, prob, mem, ghq_target_size);

    integral_term -= ghq_res[0];

    double d_shift_prob{ghq_res[1]},
             d_var_cond{ghq_res[2] / (2 * std::sqrt(var_cond))};
    arma::vec d_rng_coefs(ghq_res + 3, n_causes, false, true);
    double * d_logit_offset{d_rng_coefs.end()};
    arma::vec d_cond_mean_sub(d_logit_offset + 2 * n_causes, n_causes, false,
                              true);
    arma::mat d_vcov_cond_sub(d_cond_mean_sub.end(), n_causes, n_causes, false,
                              true);

    d_rng_coefs *= -1;

    helper.backprop_lp_traject(d_shift_prob, obs2, cause_2, gr_integral);

    arma::vec d_cond_mean{vec_no_alloc(2 * n_causes, mem)};
    d_cond_mean.zeros();
    d_cond_mean[idx_traject] -= d_shift_prob;

    arma::mat d_vcov_cond(mat_no_alloc(2 * n_causes, 2 * n_causes, mem));
    d_vcov_cond.zeros();

    d_cond_mean.subvec(0, n_causes - 1) -= rng_coefs * d_shift_prob;
    d_rng_coefs += cond_mean_sub * d_shift_prob;

    lp_mmcif::backprop_cond_vcov
      (&d_var_cond, vcov_cond.memptr(), d_vcov_cond.memptr(),
       idx_traject, idx_traject, 0, n_causes - 1, 2 * n_causes, mem);

    lp_mmcif::backprop_cond_mean
      (d_rng_coefs.memptr(), vcov_cond.memptr(), d_vcov_cond.memptr(),
       idx_traject, idx_traject, 0, n_causes - 1, 2 * n_causes, mem);

    helper.backprop_logit_offsets
      (d_logit_offset           , obs1, gr_integral);
    helper.backprop_logit_offsets
      (d_logit_offset + n_causes, obs2, gr_integral);

    d_cond_mean.subvec(0, n_causes - 1) += d_cond_mean_sub;
    helper.backprop_lp_traject
      (arma::dot(d_cond_mean, vcov_cond.col(idx_cause)),
       obs1, gr_integral);

    d_cond_mean *= lp_traject1 / 2;
    d_vcov_cond.col(idx_cause) += d_cond_mean;
    d_vcov_cond.row(idx_cause) += d_cond_mean.t();

    d_vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_cond_sub;

    lp_mmcif::backprop_cond_vcov_rev
      (d_vcov_cond.memptr(), par + indexer.vcov(), vcov_cond.memptr(),
       gr_integral + indexer.vcov(), 2 * n_causes, mem);
  }

  std::for_each(gr_integral, gr_integral + indexer.n_par<false>(),
                [&](double &x){ x /= integral_term; });
  for(size_t i = 0; i < indexer.n_par<false>(); ++i)
    gr[i] -= gr_integral[i];

  out += std::log(integral_term);
  return out;
}

double mmcif_logLik_both_cens_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  if(obs2.has_finite_trajectory_prob && !obs1.has_finite_trajectory_prob)
    return mmcif_logLik_both_cens_grad(par, gr, indexer, obs2, obs1, mem, dat);

  mmcif_comp_helper helper{indexer, par, mem};
  auto const n_causes = indexer.n_causes();

  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov, vcov_sub;
  helper.fill_vcov(vcov);
  helper.fill_vcov_rng_traject(vcov_sub, vcov);

  if(!obs1.has_finite_trajectory_prob){
    arma::uvec which_cat{0, 0};
    auto mem_marker = mem.set_mark_raii();
    mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
    rescale_problem<true> prob_use(vcov_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double *ghq_res{mem.get(prob_use.n_out())};
    auto ghq_marker = mem.set_mark_raii();
    ghq(ghq_res, dat, prob, mem, ghq_target_size);

    double const integral{ghq_res[0]};
    std::for_each(ghq_res + 1, ghq_res + prob_use.n_out(),
                  [&](double &x){ x /= integral; });

    double * d_logit_offsets{ghq_res + 1};
    arma::mat d_vcov_sub
      (d_logit_offsets + 2 * n_causes, n_causes, n_causes, false);

    helper.backprop_logit_offsets(d_logit_offsets           , obs1, gr);
    helper.backprop_logit_offsets(d_logit_offsets + n_causes, obs2, gr);

    arma::mat d_vcov(gr + indexer.vcov(), 2 * n_causes, 2 * n_causes, false,
                     true);
    d_vcov.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_sub;

    return std::log(integral);

  }
  else if(!obs2.has_finite_trajectory_prob){
    double integral{};
    double * new_gr_terms{mem.get(indexer.n_par<false>())};
    std::fill(new_gr_terms, new_gr_terms + indexer.n_par<false>(), 0);
    auto mem_marker = mem.set_mark_raii();

    arma::mat d_vcov_new_terms
      (new_gr_terms + indexer.vcov(), 2 * n_causes, 2 * n_causes, false);
    {
      arma::mat const logit_offsets_2 = logit_offsets.col(1);
      arma::uvec const which_cat{0};
      mixed_mult_logit_term<true> mult_term(logit_offsets_2, which_cat);
      rescale_problem<true> prob_use(vcov_sub, mult_term);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double * ghq_res{mem.get(prob.n_out())};
      auto ghq_marker = mem.set_mark_raii();
      ghq(ghq_res, dat, prob, mem, ghq_target_size);

      integral += ghq_res[0];

      double * d_logit_offsets_2{ghq_res + 1};
      arma::mat d_vcov_sub
        (d_logit_offsets_2 + n_causes, n_causes, n_causes, false);

      helper.backprop_logit_offsets(d_logit_offsets_2, obs2, new_gr_terms);
      d_vcov_new_terms.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_sub;
    }

    std::for_each(new_gr_terms, new_gr_terms + indexer.n_par<false>(),
                  [](double &x){ x *= -1; });

    arma::uvec which_cat{0, 0};
    arma::vec rng_coefs, vcov_col;
    for(size_t cause_1 = 0; cause_1 < n_causes; ++cause_1){
      which_cat[0] = cause_1 + 1;

      size_t const idx_traject{n_causes + cause_1};
      vcov_col = vcov.col(idx_traject).subvec(0, n_causes - 1);

      rng_coefs = arma::solve
        (vcov_sub, vcov_col, arma::solve_opts::likely_sympd);
      double const var_cond
        {1 + vcov(idx_traject, idx_traject) - arma::dot(vcov_col, rng_coefs)};

      double const lp_traject1{helper.comp_lp_traject(obs1, cause_1)};
      rng_coefs *= -1;

      mixed_probit_term<true> probit_term
        (std::sqrt(var_cond), lp_traject1, rng_coefs);
      mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
      combined_problem comp_prob({&probit_term, &mult_term});

      rescale_problem<true> prob_use(vcov_sub, comp_prob);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double * ghq_res{mem.get(prob.n_out())};
      auto ghq_marker = mem.set_mark_raii();
      ghq(ghq_res, dat, prob, mem, ghq_target_size);

      integral -= ghq_res[0];

      double const d_lp_traject1{ghq_res[1]};
      double const d_var_cond{ghq_res[2] / (2 * std::sqrt(var_cond))};
      arma::vec d_rng_coefs(ghq_res + 3, n_causes, false, true);
      double * d_logit_offsets{d_rng_coefs.end()};
      arma::mat d_vcov_sub
        (d_logit_offsets + 2 * n_causes, n_causes, n_causes, false, true);

      helper.backprop_lp_traject(d_lp_traject1, obs1, cause_1, new_gr_terms);

      lp_mmcif::backprop_cond_vcov
        (&d_var_cond, vcov.memptr(), d_vcov_new_terms.memptr(),
         idx_traject, idx_traject, 0, n_causes - 1, 2 * n_causes, mem);

      d_rng_coefs *= -1;
      lp_mmcif::backprop_cond_mean
        (d_rng_coefs.memptr(), vcov.memptr(), d_vcov_new_terms.memptr(),
         idx_traject, idx_traject, 0, n_causes - 1, 2 * n_causes, mem);

      helper.backprop_logit_offsets
        (d_logit_offsets           , obs1, new_gr_terms);
      helper.backprop_logit_offsets
        (d_logit_offsets + n_causes, obs2, new_gr_terms);

      d_vcov_new_terms.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_sub;
    }

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      gr[i] -= new_gr_terms[i] / integral;

    return std::log(integral);
  }

  double * new_gr_terms{mem.get(2 * indexer.n_par<false>())},
         * gr_inter{new_gr_terms + indexer.n_par<false>()};
  std::fill(new_gr_terms, new_gr_terms + indexer.n_par<false>(), 0);
  auto mem_marker1 = mem.set_mark_raii();

  auto comp_main_terms = [&](mmcif_data const &obs){
    std::fill(gr_inter, gr_inter + indexer.n_par<false>(), 0);
    double const integral
      {std::exp(mmcif_logLik_grad(par, gr_inter, indexer, obs, mem, dat))};
    for(size_t i = 0; i <  indexer.n_par<false>(); ++i)
      new_gr_terms[i] += gr_inter[i] * integral;
    return integral - 1;
  };

  double integral{1};
  integral += comp_main_terms(obs1);
  integral += comp_main_terms(obs2);

  // handle the interaction terms
  arma::mat V{mat_no_alloc(2, n_causes, mem)};
  arma::uvec which_cat(2);
  arma::vec dens_point(vec_no_alloc(2, mem));

  arma::mat pbvn_vcov{mat_no_alloc(2, 2, mem)};
  arma::mat rng_coefs{mat_no_alloc(2, n_causes, mem)};

  auto const vcov_dim = 2 * n_causes;
  arma::mat const cond_mean_loadings =
    arma::solve(vcov_sub,
                vcov.submat(0, n_causes, n_causes - 1, vcov_dim - 1),
                arma::solve_opts::likely_sympd).t();
  arma::mat const vcov_rng_cond_traject =
    vcov.submat(n_causes, n_causes, vcov_dim - 1, vcov_dim - 1)
    - vcov.submat(n_causes, 0, vcov_dim - 1, n_causes - 1) *
      cond_mean_loadings.t();

  arma::mat d_vcov
    (new_gr_terms + indexer.vcov(), 2 * n_causes, 2 * n_causes, false);
  arma::mat d_vcov_rng_cond_traject{mat_no_alloc(n_causes, n_causes, mem)},
            d_cond_mean_loadings{mat_no_alloc(n_causes, n_causes, mem)};

  auto mem_marker2 = mem.set_mark_raii();
  for(size_t cause_1 = 0; cause_1 < n_causes; ++cause_1){
    which_cat[0] = cause_1 + 1;
    dens_point[0] = -helper.comp_lp_traject(obs1, cause_1);

    for(size_t cause_2 = 0; cause_2 < n_causes; ++cause_2){
      which_cat[1] = cause_2 + 1;

      V.zeros();
      V(0, cause_1) = 1;
      V(1, cause_2) = 1;
      dens_point[1] = -helper.comp_lp_traject(obs2, cause_2);

      pbvn_vcov = V * vcov_rng_cond_traject * V.t();
      pbvn_vcov.diag() += 1;
      rng_coefs = V * cond_mean_loadings;

      cond_pbvn<true> pbvn_term(dens_point, pbvn_vcov, rng_coefs);
      mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
      combined_problem comp_prob({&pbvn_term, &mult_term});

      rescale_problem<true> prob_use(vcov_sub, comp_prob);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double *ghq_res{mem.get(prob_use.n_out())};
      auto loop_marker = mem.set_mark_raii();
      ghq(ghq_res, dat, prob, mem, ghq_target_size);

      integral += ghq_res[0];

      double * d_dens_point{ghq_res + 1};
      arma::mat d_rng_coefs(d_dens_point + 2, 2, n_causes, false, true),
                d_pbvn_vcov(d_rng_coefs.end(), 2, 2, false, true);
      double * d_logit_offsets{d_pbvn_vcov.end()};
      arma::mat d_vcov_sub
        (d_logit_offsets + 2 * n_causes, n_causes, n_causes, false, true);

      helper.backprop_lp_traject(-d_dens_point[0], obs1, cause_1, new_gr_terms);
      helper.backprop_lp_traject(-d_dens_point[1], obs2, cause_2, new_gr_terms);

      d_cond_mean_loadings = V.t() * d_rng_coefs;
      lp_mmcif::backprop_cond_mean
        (d_cond_mean_loadings.memptr(), vcov.memptr(), d_vcov.memptr(),
         n_causes, vcov_dim - 1, 0, n_causes - 1, 2 * n_causes, mem);

      d_vcov_rng_cond_traject = V.t() * d_pbvn_vcov * V;
      lp_mmcif::backprop_cond_vcov
        (d_vcov_rng_cond_traject.memptr(), vcov.memptr(), d_vcov.memptr(),
         n_causes, vcov_dim - 1, 0, n_causes - 1, 2 * n_causes, mem);

      helper.backprop_logit_offsets
        (d_logit_offsets           , obs1, new_gr_terms);
      helper.backprop_logit_offsets
        (d_logit_offsets + n_causes, obs2, new_gr_terms);

      d_vcov.submat(0, 0, n_causes - 1, n_causes - 1) += d_vcov_sub;
    }
  }

  for(size_t i = 0; i < indexer.n_par<false>(); ++i)
    gr[i] += new_gr_terms[i] / integral;

  return std::log(integral);
}
} // namespace

double mmcif_logLik
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs, simple_mem_stack<double> &mem,
   ghq_data const &dat){
  if(obs.has_delayed_entry()){
    double const delayed_term
      {mmcif_logLik(par, indexer, obs.to_delayed(indexer), mem, dat)};

    return mmcif_logLik(par, indexer, obs.without_delayed(), mem, dat) -
      delayed_term;
  }

  mmcif_comp_helper helper{indexer, par, mem};

  bool const is_cens{helper.is_censored(obs)};
  if(is_cens){
    auto const n_causes = indexer.n_causes();
    arma::mat logit_offsets{mat_no_alloc(n_causes, 1, mem)};
    helper.fill_logit_offsets(logit_offsets.begin(), obs);

    arma::mat vcov, vcov_sub;
    helper.fill_vcov(vcov);
    helper.fill_vcov_rng_traject(vcov_sub, vcov);

    auto mem_mark = mem.set_mark_raii();
    if(!obs.has_finite_trajectory_prob){
      return std::log(
        mmcif_univariate_mcif
          (par, indexer, obs, n_causes, mem, dat, vcov, vcov_sub,
           logit_offsets));
    }

    double integral{1};
    for(size_t cause = 0; cause < n_causes; ++cause){
      integral -= mmcif_univariate_mcif
        (par, indexer, obs, cause, mem, dat, vcov, vcov_sub, logit_offsets);
    }

    return std::log(integral);
  }

  size_t const cause{obs.cause};
  double const lp_traject{helper.comp_lp_traject(obs, cause)};
  double const d_lp_traject{helper.comp_d_lp_traject(obs, cause)};

  double out{std::log(d_lp_traject)};
  out += helper.comp_trajector_cond_dens_obs_one(lp_traject, cause);

  auto const n_causes = indexer.n_causes();
  arma::mat logit_offsets{mat_no_alloc(n_causes, 1, mem)};
  helper.fill_logit_offsets(logit_offsets.begin(), obs);

  arma::mat vcov_cond;
  helper.fill_cond_vcov_one_obs(vcov_cond, cause);

  arma::vec cond_mean;
  cond_mean = vcov_cond.col(cause + n_causes) * lp_traject;
  cond_mean = cond_mean.subvec(0, n_causes - 1);

  arma::mat vcov_sub{mat_no_alloc(n_causes, n_causes, mem)};
  vcov_sub = vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1);

  arma::uvec which_cat{static_cast<arma::uword>(cause + 1)};

  auto mem_mark = mem.set_mark_raii();
  mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
  rescale_shift_problem<false> prob_use(vcov_sub, cond_mean, mult_term);
  adaptive_problem prob(prob_use, mem, adaptive_rel_eps);
  double res{};
  ghq(&res, dat, prob, mem, ghq_target_size);
  out += std::log(res);

  return out;
}

double mmcif_logLik
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   simple_mem_stack<double> &mem, ghq_data const &dat){
  if(obs1.has_delayed_entry() && obs2.has_delayed_entry()){
    double const delayed_term
      {mmcif_logLik
        (par, indexer, obs1.to_delayed(indexer),
         obs2.to_delayed(indexer), mem, dat)};

    return mmcif_logLik
      (par, indexer, obs1.without_delayed(), obs2.without_delayed(),
       mem, dat) - delayed_term;

  } else if(obs1.has_delayed_entry() || obs2.has_delayed_entry()){
    mmcif_data const &obs_w_delayed = obs1.has_delayed_entry() ? obs1 : obs2,
                     &obs_wo_delayed = obs1.has_delayed_entry() ? obs2 : obs1;

    double const delayed_term
      {mmcif_logLik(par, indexer, obs_w_delayed.to_delayed(indexer), mem, dat)};

    return mmcif_logLik
      (par, indexer, obs_w_delayed.without_delayed(), obs_wo_delayed,
       mem, dat) - delayed_term;

  }

  mmcif_comp_helper helper{indexer, par, mem};
  bool const is_cens1{helper.is_censored(obs1)},
             is_cens2{helper.is_censored(obs2)};

  if(is_cens1 && is_cens2)
    return mmcif_logLik_both_cens(par, indexer, obs1, obs2, mem, dat);
  else if(!is_cens1 && is_cens2)
    return mmcif_logLik_one_obs(par, indexer, obs1, obs2, mem, dat);
  else if(is_cens1 && !is_cens2)
    return mmcif_logLik_one_obs(par, indexer, obs2, obs1, mem, dat);
  return mmcif_logLik_both_obs(par, indexer, obs1, obs2, mem, dat);
}

double mmcif_logLik_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem,
   ghqCpp::ghq_data const &dat){
  if(obs.has_delayed_entry()){
    double * const gr_delayed{mem.get(indexer.n_par<false>())};
    auto gr_marker = mem.set_mark_raii();
    std::fill(gr_delayed, gr_delayed + indexer.n_par<false>(), 0);

    double const delayed_term
      {mmcif_logLik_grad
        (par, gr_delayed, indexer, obs.to_delayed(indexer), mem, dat)};

    double const out
      {mmcif_logLik_grad
        (par, gr, indexer, obs.without_delayed(), mem, dat) - delayed_term};
    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      gr[i] -= gr_delayed[i];

    return out;
  }

  mmcif_comp_helper helper{indexer, par, mem};

  bool const is_cens{helper.is_censored(obs)};
  if(is_cens){
    auto const n_causes = indexer.n_causes();
    arma::mat logit_offsets{mat_no_alloc(n_causes, 1, mem)};
    helper.fill_logit_offsets(logit_offsets.begin(), obs);
    arma::uvec which_cat(1);

    arma::mat vcov, vcov_sub;
    helper.fill_vcov(vcov);
    helper.fill_vcov_rng_traject(vcov_sub, vcov);

    if(!obs.has_finite_trajectory_prob){
      which_cat[0] = 0;

      mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
      rescale_problem<true> prob_use(vcov_sub, mult_term);
      auto mem_mark1 = mem.set_mark_raii();
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double * const ghq_res{mem.get(prob.n_out())};
      auto mem_mark2 = mem.set_mark_raii();
      ghq(ghq_res, dat, prob, mem, ghq_target_size);

      double const integrand{ghq_res[0]};
      double * d_logit_offset{ghq_res + 1},
             * d_vcov_sub{d_logit_offset + n_causes};
      std::for_each(ghq_res + 1, ghq_res + prob.n_out(),
                    [&](double &x) { x /= integrand; });

      helper.backprop_logit_offsets(d_logit_offset, obs, gr);

      double * gr_Sig{gr + indexer.vcov()};
      for(size_t j = 0; j < n_causes; ++j)
        for(size_t i = 0; i < n_causes; ++i)
          gr_Sig[i + j * 2 * n_causes] += d_vcov_sub[i + j * n_causes];

      return std::log(integrand);
    }

    double * const new_gr_terms{mem.get(indexer.n_par<false>())};
    std::fill(new_gr_terms, new_gr_terms + indexer.n_par<false>(), 0);
    auto mem_mark = mem.set_mark_raii();

    double integral{1};
    for(size_t cause = 0; cause < n_causes; ++cause){
      size_t const idx_traject{n_causes + cause};
      arma::vec vcov_col = vcov.col(idx_traject).subvec(0, n_causes - 1);
      arma::vec rng_coefs = arma::solve(vcov_sub, vcov_col,
                                        arma::solve_opts::likely_sympd);
      double const var_cond
        {1 + vcov(idx_traject, idx_traject) - arma::dot(vcov_col, rng_coefs)};
      rng_coefs *= -1;
      double const lp_traject{helper.comp_lp_traject(obs, cause)};

      mixed_probit_term<true> probit_term
        (std::sqrt(var_cond), lp_traject, rng_coefs);
      which_cat[0] = cause + 1;
      mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);

      combined_problem comp_prob({&probit_term, &mult_term});
      rescale_problem<true> prob_use(vcov_sub, comp_prob);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double * const ghq_res{mem.get(prob.n_out())};
      auto loop_marker = mem.set_mark_raii();
      ghq(ghq_res, dat, prob, mem, ghq_target_size);

      double d_lp_traject{ghq_res[1]},
             d_var_cond{ghq_res[2] / (2 * std::sqrt(var_cond))},
           * d_rng_coefs{ghq_res + 3},
           * d_logit_offset{d_rng_coefs + rng_coefs.n_elem},
           * d_vcov_sub{d_logit_offset + n_causes};

      helper.backprop_lp_traject(d_lp_traject, obs, cause, new_gr_terms);

      double * const gr_Sig{new_gr_terms + indexer.vcov()};
      lp_mmcif::backprop_cond_vcov
        (&d_var_cond, vcov.memptr(), gr_Sig,
         idx_traject, idx_traject, 0, n_causes - 1, 2 * n_causes, mem);

      std::for_each(d_rng_coefs, d_rng_coefs + rng_coefs.n_elem,
                    [](double &x){ x *= -1; });
      lp_mmcif::backprop_cond_mean
        (d_rng_coefs, vcov.memptr(), gr_Sig,
         idx_traject, idx_traject, 0, n_causes - 1, 2 * n_causes, mem);

      helper.backprop_logit_offsets(d_logit_offset, obs, new_gr_terms);

      for(size_t j = 0; j < n_causes; ++j)
        for(size_t i = 0; i < n_causes; ++i)
          gr_Sig[i + j * 2 * n_causes] += d_vcov_sub[i + j * n_causes];

      integral -= ghq_res[0];
    }

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      gr[i] -= new_gr_terms[i] / integral;

    return std::log(integral);
  }

  size_t const cause{obs.cause};
  double const lp_traject{helper.comp_lp_traject(obs, cause)};
  double const d_lp_traject{helper.comp_d_lp_traject(obs, cause)};

  double out{std::log(d_lp_traject)};
  auto const norm_dens_term =
    helper.comp_trajector_cond_dens_obs_one_w_grads(lp_traject, cause);
  out += norm_dens_term[0];

  helper.backprop_d_lp_traject(1/d_lp_traject, obs, cause, gr);
  auto const n_causes = indexer.n_causes();
  {
    auto const idx = cause + n_causes;
    gr[indexer.vcov() + idx + idx * 2 * n_causes] += norm_dens_term[2];
  }

  arma::mat logit_offsets{mat_no_alloc(n_causes, 1, mem)};
  helper.fill_logit_offsets(logit_offsets.begin(), obs);

  arma::mat vcov_cond;
  helper.fill_cond_vcov_one_obs(vcov_cond, cause);

  arma::vec cond_mean;
  cond_mean = vcov_cond.col(cause + n_causes) * lp_traject;
  cond_mean = cond_mean.subvec(0, n_causes - 1);

  arma::mat vcov_sub{mat_no_alloc(n_causes, n_causes, mem)};
  vcov_sub = vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1);

  arma::uvec which_cat{static_cast<arma::uword>(cause + 1)};

  auto mem_mark = mem.set_mark_raii();
  mixed_mult_logit_term<true> mult_term(logit_offsets, which_cat);
  rescale_shift_problem<true> prob_use(vcov_sub, cond_mean, mult_term);
  adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

  double * ghq_res{mem.get(prob.n_out())};
  auto ghq_mark = mem.set_mark_raii();
  ghq(ghq_res, dat, prob, mem, ghq_target_size);

  double const integrand{ghq_res[0]};
  out += std::log(integrand);
  std::for_each(ghq_res + 1, ghq_res + prob.n_out(),
                [&](double &x){ x /= integrand; });

  double const * d_logit_offsets{ghq_res + 1},
               * d_cond_mean{d_logit_offsets + n_causes},
               * d_vcov_sub{d_cond_mean + n_causes};

  helper.backprop_logit_offsets(d_logit_offsets, obs, gr);

  helper.backprop_lp_traject(
    norm_dens_term[1] + std::inner_product(
      d_cond_mean, d_cond_mean + n_causes, vcov_cond.colptr(cause + n_causes),
      0.),
    obs, cause, gr);

  arma::mat d_vcov_cond(mat_no_alloc(2 * n_causes, 2 * n_causes, mem));
  d_vcov_cond.zeros();

  for(size_t i = 0; i < n_causes; ++i){
    d_vcov_cond(i, cause + n_causes) = d_cond_mean[i] * lp_traject / 2;
    d_vcov_cond(cause + n_causes, i) = d_cond_mean[i] * lp_traject / 2;
  }

  for(size_t j = 0; j < n_causes; ++j)
    for(size_t i = 0; i < n_causes; ++i)
      d_vcov_cond(i, j) += d_vcov_sub[i + j * n_causes];

  lp_mmcif::backprop_cond_vcov_rev
    (d_vcov_cond.memptr(), par + indexer.vcov(), vcov_cond.memptr(),
     gr + indexer.vcov(), 2 * n_causes, mem);

  return out;
}

double mmcif_logLik_grad
  (double const * par, double * __restrict__ gr, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   ghqCpp::simple_mem_stack<double> &mem, ghqCpp::ghq_data const &dat){
  if(obs1.has_delayed_entry() && obs2.has_delayed_entry()){
    double * const gr_delayed{mem.get(indexer.n_par<false>())};
    auto gr_marker = mem.set_mark_raii();
    std::fill(gr_delayed, gr_delayed + indexer.n_par<false>(), 0);

    double const delayed_term
      {mmcif_logLik_grad
        (par, gr_delayed, indexer, obs1.to_delayed(indexer),
         obs2.to_delayed(indexer), mem, dat)};

    double const out
      {mmcif_logLik_grad
        (par, gr, indexer, obs1.without_delayed(), obs2.without_delayed(),
         mem, dat) - delayed_term};

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      gr[i] -= gr_delayed[i];

    return out;

  } else if(obs1.has_delayed_entry() || obs2.has_delayed_entry()){
    double * const gr_delayed{mem.get(indexer.n_par<false>())};
    auto gr_marker = mem.set_mark_raii();
    std::fill(gr_delayed, gr_delayed + indexer.n_par<false>(), 0);

    mmcif_data const &obs_w_delayed = obs1.has_delayed_entry() ? obs1 : obs2,
                     &obs_wo_delayed = obs1.has_delayed_entry() ? obs2 : obs1;

    double const delayed_term
      {mmcif_logLik_grad
        (par, gr_delayed, indexer, obs_w_delayed.to_delayed(indexer), mem,
         dat)};

    double const out
      {mmcif_logLik_grad
        (par, gr, indexer, obs_w_delayed.without_delayed(), obs_wo_delayed,
         mem, dat) - delayed_term};

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      gr[i] -= gr_delayed[i];

    return out;

  }

  mmcif_comp_helper helper{indexer, par, mem};
  bool const is_cens1{helper.is_censored(obs1)},
             is_cens2{helper.is_censored(obs2)};

  if(is_cens1 && is_cens2)
    return mmcif_logLik_both_cens_grad(par, gr, indexer, obs1, obs2, mem, dat);
  else if(!is_cens1 && is_cens2)
    return mmcif_logLik_one_obs_grad(par, gr, indexer, obs1, obs2, mem, dat);
  else if(is_cens1 && !is_cens2)
    return mmcif_logLik_one_obs_grad(par, gr, indexer, obs2, obs1, mem, dat);
  return mmcif_logLik_both_obs_grad(par, gr, indexer, obs1, obs2, mem, dat);
}

double mmcif_log_mcif
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs, ghqCpp::simple_mem_stack<double> &mem,
   ghqCpp::ghq_data const &dat, bool const deriv){
  if(obs.has_delayed_entry()){
    double const delayed_term
      {mmcif_logLik(par, indexer, obs.to_delayed(indexer), mem, dat)};

    return mmcif_log_mcif(par, indexer, obs.without_delayed(), mem, dat, deriv) -
      delayed_term;
  }

  mmcif_comp_helper helper{indexer, par, mem};

  bool const is_cens{helper.is_censored(obs)};
  if(is_cens){
    if(deriv)
      throw std::invalid_argument("deriv with censored observation");
    return mmcif_logLik(par, indexer, obs, mem, dat);
  } else if(deriv){
    return mmcif_logLik(par, indexer, obs, mem, dat);
  }

  arma::mat logit_offsets{mat_no_alloc(indexer.n_causes(), 1, mem)};
  helper.fill_logit_offsets(logit_offsets.begin(), obs);

  arma::mat vcov, vcov_sub;
  helper.fill_vcov(vcov);
  helper.fill_vcov_rng_traject(vcov_sub, vcov);

  auto mem_mark = mem.set_mark_raii();
  double const mcif
    {mmcif_univariate_mcif(par, indexer, obs, obs.cause, mem, dat,
                           vcov, vcov_sub, logit_offsets)};

  return std::log(mcif);
}

double mmcif_log_mcif
  (double const * par, param_indexer const &indexer,
   mmcif_data const &obs1, mmcif_data const &obs2,
   ghqCpp::simple_mem_stack<double> &mem, ghqCpp::ghq_data const &dat,
   std::array<bool, 2> const &derivs){
  if(obs1.has_delayed_entry() && obs2.has_delayed_entry()){
    double const delayed_term
      {mmcif_logLik
        (par, indexer, obs1.to_delayed(indexer),
         obs2.to_delayed(indexer), mem, dat)};

    return mmcif_log_mcif
      (par, indexer, obs1.without_delayed(), obs2.without_delayed(),
       mem, dat, derivs) - delayed_term;

  } else if(obs1.has_delayed_entry() || obs2.has_delayed_entry()){
    mmcif_data const &obs_w_delayed = obs1.has_delayed_entry() ? obs1 : obs2,
                    &obs_wo_delayed = obs1.has_delayed_entry() ? obs2 : obs1;

    double const delayed_term
      {mmcif_logLik(par, indexer, obs_w_delayed.to_delayed(indexer), mem, dat)};

    std::array<bool, 2> const new_deriv =
      obs1.has_delayed_entry()
        ? derivs
        : std::array<bool, 2>{ derivs[1], derivs[0] };

    return mmcif_log_mcif
      (par, indexer, obs_w_delayed.without_delayed(), obs_wo_delayed,
       mem, dat, new_deriv) - delayed_term;

  }

  mmcif_comp_helper helper{indexer, par, mem};
  bool const is_cens1{helper.is_censored(obs1)},
             is_cens2{helper.is_censored(obs2)};

  auto comp_swap = [&]{
    return mmcif_log_mcif
      (par, indexer, obs2, obs1, mem, dat, { derivs[1], derivs[0] });
  };

  if(derivs[1] && !derivs[0])
    return comp_swap();
  else if(derivs[1] == derivs[0] && is_cens2 && !is_cens1)
    return comp_swap();

  if((is_cens1 && derivs[0]) || (is_cens2 && derivs[1]))
    throw std::invalid_argument("deriv with censored observation");

  bool const obs_wo_deriv1{(!is_cens1 && !derivs[0])};
  bool const obs_wo_deriv2{(!is_cens2 && !derivs[1])};
  if(!obs_wo_deriv1 && !obs_wo_deriv2)
    return mmcif_logLik(par, indexer, obs1, obs2, mem, dat);

  // because of the swapping, there are only three scenarios left
  //  a. the first one is density and the second is an observed cif
  //  b. both are observed cifs
  //  c. the first one is censored and the second one is observed cif
  auto const n_causes = indexer.n_causes();
  if(derivs[0]){
    // TODO: more or less the same code as in mmcif_logLik_one_obs
    // TODO: refactor
    double const lp_traject1{helper.comp_lp_traject(obs1)};
    double const d_lp_traject1{helper.comp_d_lp_traject(obs1)};
    auto const cause1 = obs1.cause;
    auto const cause2 = obs2.cause;

    double out{std::log(d_lp_traject1)};
    out += helper.comp_trajector_cond_dens_obs_one(lp_traject1, cause1);

    arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
    helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
    helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

    arma::mat vcov_cond;
    helper.fill_cond_vcov_one_obs(vcov_cond, cause1);

    arma::vec cond_mean;
    cond_mean = vcov_cond.col(cause1 + n_causes) * lp_traject1;

    arma::mat const vcov_cond_sub
      {vcov_cond.submat(0, 0, n_causes - 1, n_causes - 1)};
    arma::vec const cond_mean_sub{cond_mean.subvec(0, n_causes - 1)};

    arma::uvec const which_cat{cause1 + 1, obs2.cause + 1};
    auto mem_mark = mem.set_mark_raii();
    if(!obs2.has_finite_trajectory_prob){
      mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
      rescale_shift_problem<false> prob_use
        (vcov_cond_sub, cond_mean_sub, mult_term);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double integral_term{};
      ghq(&integral_term, dat, prob, mem, ghq_target_size);
      out += std::log(integral_term);
      return out;
    }

    arma::vec rng_coefs, vcov_col;
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);

    size_t const idx_traject{n_causes + cause2};
    vcov_col = vcov_cond.col(idx_traject).subvec(0, n_causes - 1);

    rng_coefs = arma::solve
      (vcov_cond_sub, vcov_col, arma::solve_opts::likely_sympd);
    double const var_cond
    {1 + vcov_cond(idx_traject, idx_traject)
      - arma::dot(vcov_col, rng_coefs)};

    double const lp_traject2{helper.comp_lp_traject(obs2, cause2)};
    double const shift_prob
    {lp_traject2 - cond_mean[idx_traject]
      + arma::dot(rng_coefs, cond_mean_sub)};

    rng_coefs *= -1;

    mixed_probit_term<false> probit_term
      (std::sqrt(var_cond), shift_prob, rng_coefs);
    combined_problem comp_prob({&probit_term, &mult_term});

    rescale_shift_problem<false> prob_use
      (vcov_cond_sub, cond_mean_sub, comp_prob);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double integral_term{};
    ghq(&integral_term, dat, prob, mem, ghq_target_size);
    out += std::log(integral_term);
    return out;

  } else if(!is_cens1 && !is_cens2){
    // code more or less like in mmcif_logLik_both_cens
    // TODO: refactor
    if(!obs1.has_finite_trajectory_prob && !obs2.has_finite_trajectory_prob){
      arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
      helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
      helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

      arma::mat vcov, vcov_sub;
      helper.fill_vcov(vcov);
      helper.fill_vcov_rng_traject(vcov_sub, vcov);

      arma::uvec const which_cat{obs1.cause + 1, obs2.cause + 1};

      auto mem_marker = mem.set_mark_raii();
      mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
      rescale_problem<false> prob_use(vcov_sub, mult_term);
      adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

      double integral{};
      ghq(&integral, dat, prob, mem, ghq_target_size);

      return std::log(integral);
    }

    auto comp_order_one_none_finte =
      [&](mmcif_data const &obs1, mmcif_data const &obs2){
        arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
        helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
        helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

        arma::mat vcov, vcov_sub;
        helper.fill_vcov(vcov);
        helper.fill_vcov_rng_traject(vcov_sub, vcov);

        arma::uvec const which_cat{obs1.cause + 1, obs2.cause + 1};

        auto mem_marker = mem.set_mark_raii();
        arma::vec rng_coefs, vcov_col;

        size_t const idx_traject{n_causes + obs1.cause};
        vcov_col = vcov.col(idx_traject).subvec(0, n_causes - 1);

        rng_coefs = arma::solve
          (vcov_sub, vcov_col, arma::solve_opts::likely_sympd);
        double const var_cond
          {1 + vcov(idx_traject, idx_traject) - arma::dot(vcov_col, rng_coefs)};

        double const lp_traject1{helper.comp_lp_traject(obs1, obs1.cause)};
        rng_coefs *= -1;

        mixed_probit_term<false> probit_term
          (std::sqrt(var_cond), lp_traject1, rng_coefs);
        mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
        combined_problem comp_prob({&probit_term, &mult_term});

        rescale_problem<false> prob_use(vcov_sub, comp_prob);
        adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

        double integral{};
        ghq(&integral, dat, prob, mem, ghq_target_size);
        return std::log(integral);
    };

    if(!obs1.has_finite_trajectory_prob)
      return comp_order_one_none_finte(obs2, obs1);
    if(!obs2.has_finite_trajectory_prob)
      return comp_order_one_none_finte(obs1, obs2);

    arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
    helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
    helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

    arma::mat vcov, vcov_sub;
    helper.fill_vcov(vcov);
    helper.fill_vcov_rng_traject(vcov_sub, vcov);

    auto const cause_1 = obs1.cause;
    auto const cause_2 = obs2.cause;

    arma::mat V{mat_no_alloc(2, n_causes, mem)};
    arma::uvec const which_cat{cause_1 + 1, cause_2 + 1};
    arma::vec dens_point(vec_no_alloc(2, mem));

    arma::mat pbvn_vcov{mat_no_alloc(2, 2, mem)};
    arma::mat rng_coefs{mat_no_alloc(2, n_causes, mem)};

    auto const vcov_dim = 2 * n_causes;
    arma::mat const cond_mean_loadings =
      arma::solve(vcov_sub,
                  vcov.submat(0, n_causes, n_causes - 1, vcov_dim - 1),
                  arma::solve_opts::likely_sympd).t();
    arma::mat const vcov_rng_cond_traject =
      vcov.submat(n_causes, n_causes, vcov_dim - 1, vcov_dim - 1)
      - vcov.submat(n_causes, 0, vcov_dim - 1, n_causes - 1) *
        cond_mean_loadings.t();

    auto mem_marker = mem.set_mark_raii();
    dens_point[0] = -helper.comp_lp_traject(obs1, cause_1);

    V.zeros();
    V(0, cause_1) = 1;
    V(1, cause_2) = 1;
    dens_point[1] = -helper.comp_lp_traject(obs2, cause_2);

    pbvn_vcov = V * vcov_rng_cond_traject * V.t();
    pbvn_vcov.diag() += 1;
    rng_coefs = V * cond_mean_loadings;

    cond_pbvn<false> pbvn_term(dens_point, pbvn_vcov, rng_coefs);
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
    combined_problem comp_prob({&pbvn_term, &mult_term});

    rescale_problem<false> prob_use(vcov_sub, comp_prob);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double integral{};
    ghq(&integral, dat, prob, mem, ghq_target_size);
    return std::log(integral);
  }

  // code more or less like in mmcif_logLik_both_cens
  // TODO: refactor
  arma::mat logit_offsets{mat_no_alloc(n_causes, 2, mem)};
  helper.fill_logit_offsets(logit_offsets.colptr(0), obs1);
  helper.fill_logit_offsets(logit_offsets.colptr(1), obs2);

  arma::mat vcov, vcov_sub;
  helper.fill_vcov(vcov);
  helper.fill_vcov_rng_traject(vcov_sub, vcov);

  if(!obs1.has_finite_trajectory_prob && !obs2.has_finite_trajectory_prob){
    arma::uvec const which_cat{0, obs2.cause + 1};

    auto mem_marker = mem.set_mark_raii();
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
    rescale_problem<false> prob_use(vcov_sub, mult_term);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double integral{};
    ghq(&integral, dat, prob, mem, ghq_target_size);

    return std::log(integral);

  } else if(obs1.has_finite_trajectory_prob != obs2.has_finite_trajectory_prob)
    throw std::runtime_error("the case where one is censored and at the maximum follow-up and the other is a cummulative is not implemented");

  auto mem_marker1 = mem.set_mark_raii();
  double integral
    {std::exp(mmcif_log_mcif(par, indexer, obs2, mem, dat, derivs[1]))};

  // handle the interaction terms
  arma::mat V{mat_no_alloc(2, n_causes, mem)};
  arma::uvec which_cat(2);
  arma::vec dens_point(vec_no_alloc(2, mem));

  arma::mat pbvn_vcov{mat_no_alloc(2, 2, mem)};
  arma::mat rng_coefs{mat_no_alloc(2, n_causes, mem)};

  auto const vcov_dim = 2 * n_causes;
  arma::mat const cond_mean_loadings =
    arma::solve(vcov_sub,
                vcov.submat(0, n_causes, n_causes - 1, vcov_dim - 1),
                arma::solve_opts::likely_sympd).t();
  arma::mat const vcov_rng_cond_traject =
    vcov.submat(n_causes, n_causes, vcov_dim - 1, vcov_dim - 1)
    - vcov.submat(n_causes, 0, vcov_dim - 1, n_causes - 1) *
      cond_mean_loadings.t();

  auto const cause_2 = obs2.cause;
  which_cat[1] = cause_2 + 1;

  dens_point[1] = -helper.comp_lp_traject(obs2, cause_2);

  auto mem_marker2 = mem.set_mark_raii();
  for(size_t cause_1 = 0; cause_1 < n_causes; ++cause_1){
    which_cat[0] = cause_1 + 1;
    dens_point[0] = -helper.comp_lp_traject(obs1, cause_1);

    V.zeros();
    V(0, cause_1) = 1;
    V(1, cause_2) = 1;

    pbvn_vcov = V * vcov_rng_cond_traject * V.t();
    pbvn_vcov.diag() += 1;
    rng_coefs = V * cond_mean_loadings;

    cond_pbvn<false> pbvn_term(dens_point, pbvn_vcov, rng_coefs);
    mixed_mult_logit_term<false> mult_term(logit_offsets, which_cat);
    combined_problem comp_prob({&pbvn_term, &mult_term});

    rescale_problem<false> prob_use(vcov_sub, comp_prob);
    adaptive_problem prob(prob_use, mem, adaptive_rel_eps);

    double res{};
    ghq(&res, dat, prob, mem, ghq_target_size);
    integral -= res;
  }

  return std::log(integral);
}
