#include "pedigree-ll.h"

namespace {
arma::mat adjust_design_mat_sign
  (arma::mat const &X, arma::vec const &y){
  if(X.n_rows != y.n_elem)
    throw std::invalid_argument("adjust_design_mat_sign: y and X's dimension do not match");

  arma::mat out = X;
  for(arma::uword i = 0; i < X.n_rows; ++i)
    if(y[i] > 0)
      out.row(i) *= -1;

  return out;
}
} // namespace

namespace pedmod {
cache_mem<double> pedigree_ll_term::dmem;

void pedigree_ll_term::setup
  (double const * par, arma::vec &mu, arma::vec &lower, arma::vec &upper,
   arma::mat &sig, cache_mem<double> &mem){
  mu = arma::vec   (mem.get_mem(), n_members(), false),
    lower = arma::vec(mu.end()     , n_members(), false),
    upper = arma::vec(lower.end()  , n_members(), false);

  std::fill(lower.begin(), lower.end(),
            -std::numeric_limits<double>::infinity());
  upper.zeros();

  arma::vec beta(const_cast<double *>(par), n_fix_effect(), false, true);
  for(unsigned i = 0; i < n_members(); ++i)
    mu[i] = arma::dot(beta, X.row(i));

  sig = arma::mat(upper.end(), n_members(), n_members(), false);
}

pedigree_ll_term::pedigree_ll_term(arma::mat const &X_in, arma::vec const &y,
                                   std::vector<arma::mat> const &scale_mats,
                                   unsigned const max_threads,
                                   unsigned const max_n_sequences):
  X(adjust_design_mat_sign(X_in, y)),
  max_n_sequences(max_n_sequences),
  l_factor(([&](){
    // set cache
    cdf<likelihood               >::alloc_mem(y.n_elem, max_threads);
    cdf<pedigree_l_factor        >::alloc_mem(y.n_elem, max_threads);
    cdf<pedigree_l_factor_Hessian>::alloc_mem(y.n_elem, max_threads);

    arma::vec z(y.n_elem);
    for(arma::uword i = 0; i < y.n_elem; ++i)
      z[i] = y[i] > 0 ? 1 : -1;

    std::vector<arma::mat> out = scale_mats;
    for(auto &x : out){
      if(x.n_rows != z.n_elem or x.n_cols != z.n_elem)
        throw std::invalid_argument("pedigree_ll_term::pedigree_ll_term: invalid scale matrices");

      for(arma::uword j = 0; j < z.n_elem; ++j)
        for(arma::uword i = 0; i < j; ++i){
          double const mult = z[i] * z[j];
          x.at(i, j) *= mult;
          x.at(j, i) *= mult;
        }
    }

    return pedigree_l_factor(out, max_threads, X.t(), max_n_sequences);
  })()),
  h_factor(l_factor.scale_mats, max_threads, X.t(), max_n_sequences) {
  // checks
  if(l_factor.n_mem != n_members())
    throw std::invalid_argument("pedigree_ll_term::pedigree_ll_term: X and scale matrices dimension do not match");

  // set working memory
  likelihood::alloc_mem(n_members(), max_threads, max_n_sequences);
  dmem.set_n_mem(
    3 * n_members() + n_members() * n_members(), max_threads);
}

pedigree_ll_term::fn_res pedigree_ll_term::fn
  (double const * par, unsigned const maxvls, double const abs_eps,
   double const rel_eps, int minvls, bool const do_reorder,
   bool const use_aprx, bool &did_fail, cdf_methods const method,
   bool const use_tilting){
  did_fail = true;

  arma::vec mu, lower, upper;
  arma::mat sig;
  setup(par, mu, lower, upper, sig, dmem);
  l_factor.setup(sig, par + n_fix_effect(), 1., true);

  likelihood func;
  if(minvls < 0)
    minvls = std::min<int>(1000, 100 * n_members());
  auto const res = cdf<likelihood>(
    func, lower, upper, mu, sig, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, method, minvls, max_n_sequences);

  did_fail = res.inform > 0;

  double const log_likelihood = std::log(res.likelihood);

  // crude variance estimator based on the delta rule
  double const sig_est = res.abserr * 2 / 7 / res.likelihood,
    var_est = sig_est * sig_est;

  return { log_likelihood, var_est };
}

double pedigree_ll_term::gr
  (double const * par, double * d_par, double * var_est, unsigned const maxvls,
   double const abs_eps, double const rel_eps, int minvls,
   bool const do_reorder, bool const use_aprx, bool &did_fail,
   double const weight, cdf_methods const method, bool const use_tilting){
  did_fail = true;

  arma::vec mu, lower, upper;
  arma::mat sig;
  setup(par, mu, lower, upper, sig, dmem);
  l_factor.setup(sig, par + n_fix_effect(), 1., true);

  {
    likelihood lfunc;
    auto const norm_const = cdf<likelihood>(
      lfunc, lower, upper, mu, sig, do_reorder, use_aprx,
      use_tilting).approximate(
          maxvls, abs_eps, std::min(1., 10. * rel_eps), method, minvls,
          max_n_sequences);

    l_factor.setup(sig, par + n_fix_effect(), norm_const.likelihood, false);
  }

  if(minvls < 0)
    minvls = std::min<unsigned>(1000, 100 * n_members());
  auto const res = cdf<pedigree_l_factor>(
    l_factor, lower, upper, mu, sig, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, method, minvls,
        max_n_sequences);

  // derivatives for the slopes and the scale parameters
  for(unsigned i = 0; i < n_fix_effect() + n_scales(); ++i)
    d_par[i] += weight * res.derivs[i];

  // add variance terms to var_est. The first one for the log likelihood is an
  // application of the delta rule
  var_est[0] += weight * weight * res.sd_errs[0] * res.sd_errs[0] /
    (res.likelihood * res.likelihood);
  for(unsigned i = 1; i < n_fix_effect() + n_scales() + 1; ++i)
    var_est[i] += weight * weight * res.sd_errs[i] * res.sd_errs[i];

  did_fail = res.inform > 0;
  return weight * std::log(res.likelihood);
}

double pedigree_ll_term::hessian
  (double const * par, double * d_par, double *hess, double * var_est,
   unsigned const maxvls, double const abs_eps, double const rel_eps,
   int minvls, bool const do_reorder, bool const use_aprx, bool &did_fail,
   double const weight, cdf_methods const method, bool const use_tilting){
  did_fail = true;

  arma::vec mu, lower, upper;
  arma::mat sig;
  setup(par, mu, lower, upper, sig, dmem);

  h_factor.setup(sig, par + n_fix_effect(), 1.);

  {
    likelihood lfunc;
    auto const norm_const = cdf<likelihood>(
      lfunc, lower, upper, mu, sig, do_reorder, use_aprx,
      use_tilting).approximate(
          maxvls, abs_eps, std::min(1., 10. * rel_eps), method, minvls,
          max_n_sequences);

    h_factor.setup(sig, par + n_fix_effect(), norm_const.likelihood);
  }

  if(minvls < 0)
    minvls = std::min<unsigned>(1000, 100 * n_members());
  auto const res = cdf<pedigree_l_factor_Hessian>(
    h_factor, lower, upper, mu, sig, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, method, minvls,
        max_n_sequences);

  // derivatives for the slopes and the scale parameters
  unsigned const n_pars = n_fix_effect() + n_scales();
  for(unsigned i = 0; i < n_pars; ++i)
    d_par[i] += weight * res.gradient[i];

  // the hessian
  for(unsigned i = 0; i < n_pars * n_pars; ++i)
    hess[i] += weight * res.hessian[i];

  // add variance terms to var_est. The first one for the log likelihood is an
  // application of the delta rule
  var_est[0] += weight * weight * res.sd_errs[0] * res.sd_errs[0] /
    (res.likelihood * res.likelihood);
  for(unsigned i = 1; i < n_pars * (1 + n_pars) + 1; ++i)
    var_est[i] += weight * weight * res.sd_errs[i] * res.sd_errs[i];

  did_fail = res.inform > 0;
  return weight * std::log(res.likelihood);
}

pedigree_ll_term_loading::pedigree_ll_term_loading
  (arma::mat const &X_in, arma::mat const &Z, arma::vec const &y,
   std::vector<arma::mat> const &scale_mats, unsigned const max_threads,
   unsigned const max_n_sequences):
  X(adjust_design_mat_sign(X_in, y)),
  Z(Z),
  max_n_sequences(max_n_sequences),
  scale_mats
  {
    ([&]{
      arma::vec z(y.n_elem);
      for(arma::uword i = 0; i < y.n_elem; ++i)
        z[i] = y[i] > 0 ? 1 : -1;

      std::vector<arma::mat>  out = scale_mats;
      for(auto &x : out){
        if(x.n_rows != z.n_elem or x.n_cols != z.n_elem)
          throw std::invalid_argument("pedigree_ll_term_loading::pedigree_ll_term_loading: invalid scale matrices");

        for(arma::uword j = 0; j < z.n_elem; ++j)
          for(arma::uword i = 0; i < j; ++i){
            double const mult{z[i] * z[j]};
            x.at(i, j) *= mult;
            x.at(j, i) *= mult;
          }
      }

      return out;
    })()
  } {
  // checks
  for(auto &mat : scale_mats)
    if(mat.n_rows != n_members() || mat.n_cols != n_members())
      throw std::invalid_argument("pedigree_ll_term_loading: X and scale matrices dimension do not match");
  if(Z.n_cols < 1 || Z.n_rows != n_members())
    throw std::invalid_argument("pedigree_ll_term_loading: invalid Z");
  if(scale_mats.size() < 1)
    throw std::invalid_argument("No scale matrices where passed");

  // set working memory
  cdf<likelihood       >::alloc_mem(y.n_elem, max_threads);
  cdf<generic_l_factor>::alloc_mem(y.n_elem, max_threads);

  likelihood::alloc_mem(n_members(), max_threads, max_n_sequences);
  generic_l_factor::alloc_mem(n_members(), max_threads, max_n_sequences);
}

pedigree_ll_term_loading::fn_res pedigree_ll_term_loading::fn
  (double const * par, unsigned const maxvls, double const abs_eps,
   double const rel_eps, int minvls, bool const do_reorder,
   bool const use_aprx, bool &did_fail, cdf_methods const method,
   bool const use_tilting){
  did_fail = true;
  arma::vec lower(n_members()),
  upper(n_members());
  std::fill
    (lower.begin(), lower.end(), -std::numeric_limits<double>::infinity());
  upper.zeros();

  arma::vec beta(const_cast<double *>(par), n_fix_effect(), false, true);
  arma::vec const mu = X * beta;

  arma::mat thetas(beta.end(), n_scale_coefs(), n_scales(), false, true);
  arma::mat scale_params = Z * thetas;
  scale_params.for_each([](double &x) { x = std::exp(x); });

  arma::mat sig(n_members(), n_members(), arma::fill::zeros);
  sig.diag() += 1;

  arma::mat tmp;
  for(size_t i = 0; i < n_scales(); ++i){
    tmp = scale_mats[i];
    tmp.each_row() %= scale_params.col(i).t();
    tmp.each_col() %= scale_params.col(i);
    sig += tmp;
  }

  likelihood func;
  if(minvls < 0)
    minvls = std::min<int>(1000, 100 * n_members());
  auto const res = cdf<likelihood>(
    func, lower, upper, mu, sig, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, method, minvls, max_n_sequences);

  did_fail = res.inform > 0;

  double const log_likelihood = std::log(res.likelihood);

  // crude variance estimator based on the delta rule
  double const sig_est = res.abserr * 2 / 7 / res.likelihood,
    var_est = sig_est * sig_est;

  return { log_likelihood, var_est };
}

double pedigree_ll_term_loading::gr
  (double const * par, double * d_par, double * var_est, unsigned const maxvls,
   double const abs_eps, double const rel_eps, int minvls,
   bool const do_reorder, bool const use_aprx, bool &did_fail,
   double const weight, cdf_methods const method, bool const use_tilting){
  did_fail = true;
  arma::vec lower(n_members()),
  upper(n_members());
  std::fill
    (lower.begin(), lower.end(), -std::numeric_limits<double>::infinity());
  upper.zeros();

  arma::vec beta(const_cast<double *>(par), n_fix_effect(), false, true);
  arma::vec const mu = X * beta;

  arma::mat thetas
    (beta.end(), n_scale_coefs(), n_scales(), false);
  arma::mat scale_params = Z * thetas;
  scale_params.for_each([](double &x) { x = std::exp(x); });

  arma::mat sig(n_members(), n_members(), arma::fill::zeros);
  sig.diag() += 1;

  arma::mat tmp_mat;
  for(size_t i = 0; i < scale_mats.size(); ++i){
    tmp_mat = scale_mats[i];
    tmp_mat.each_row() %= scale_params.col(i).t();
    tmp_mat.each_col() %= scale_params.col(i);
    sig += tmp_mat;
  }

  if(minvls < 0)
    minvls = std::min<unsigned>(1000, 100 * n_members());

  double norm_const{1};
  {
    likelihood lfunc;
    norm_const = cdf<likelihood>(
      lfunc, lower, upper, mu, sig, do_reorder, use_aprx,
      use_tilting).approximate(
          maxvls, abs_eps, std::min(1., 10. * rel_eps), method, minvls,
          max_n_sequences).likelihood;
  }

  generic_l_factor l_factor(mu, sig, norm_const);
  auto res = cdf<generic_l_factor>(
    l_factor, lower, upper, mu, sig, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, method, minvls,
        max_n_sequences);

  // compute the derivatives
  res.derivs *= weight;
  arma::vec d_mu(res.derivs.begin(), n_members(), false, true);
  arma::mat d_Sig(d_mu.end(), n_members(), n_members(), false, true);

  arma::vec d_fixef(d_par, n_fix_effect(), false, true);
  arma::mat d_thetas(d_fixef.end(), n_scale_coefs(), n_scales(), false, true);

  d_fixef += X.t() * d_mu;

  arma::vec derivs_scales(n_members());
  for(size_t i = 0; i < n_scales(); ++i){
    derivs_scales.zeros();

    for(arma::uword j = 0; j < n_members(); ++j)
      for(arma::uword k = 0; k < n_members(); ++k)
        derivs_scales[j] +=
          2 * scale_params(k, i) *scale_mats[i](j, k) * d_Sig(k, j);

    derivs_scales %= scale_params.col(i);
    d_thetas.col(i) += Z.t() * derivs_scales;
  }

  // add variance terms to var_est. The first one for the log likelihood is an
  // application of the delta rule
  var_est[0] += weight * weight * res.sd_errs[0] * res.sd_errs[0] /
    (res.likelihood * res.likelihood);
  std::fill(var_est + 1, var_est + n_par() + 1,
            std::numeric_limits<double>::quiet_NaN());

  did_fail = res.inform > 0;
  return weight * std::log(res.likelihood);
}


}
