#ifndef PEDIGREE_LL_H
#define PEDIGREE_LL_H

#include "cdfaprx.h"
#include <stdexcept>
#include <limits>

namespace pedmod {

class pedigree_ll_term {
  /// design matrix for the fixed effects
  arma::mat const X;

  static cache_mem<double> dmem;

  unsigned const max_n_sequences;

  void setup
    (double const * par, arma::vec &mu, arma::vec &lower, arma::vec &upper,
     arma::mat &sig, cache_mem<double> &mem);

public:
  /// object to compute the likelihood factor
  pedigree_l_factor l_factor;
  /// object to compute the Hessian
  pedigree_l_factor_Hessian h_factor;

  unsigned n_members() const { return X.n_rows; }
  unsigned n_fix_effect() const { return X.n_cols; }
  size_t n_scales() const { return l_factor.scale_mats.size(); }
  size_t n_par() const { return n_fix_effect() + n_scales(); }

  pedigree_ll_term(arma::mat const &X_in, arma::vec const &y,
                   std::vector<arma::mat> const &scale_mats,
                   unsigned const max_threads,
                   unsigned const max_n_sequences);

  struct fn_res {
    double log_likelihood;
    double estimator_var;
  };

  /**
   * Approximates the log-likelihood term.
   */
  fn_res fn
    (double const * par, unsigned const maxvls, double const abs_eps,
     double const rel_eps, int minvls, bool const do_reorder,
     bool const use_aprx, bool &did_fail, cdf_methods const method,
     bool const use_tilting);

  /**
   * Approximates the log-likelihood term and the derivative.
   */
  double gr
    (double const * par, double * d_par, double * var_est, unsigned const maxvls,
     double const abs_eps, double const rel_eps, int minvls,
     bool const do_reorder, bool const use_aprx, bool &did_fail,
     double const weight, cdf_methods const method, bool const use_tilting);

  /**
   * Approximates the log-likelihood term, the gradient, and the Hessian
   */
  double hessian
    (double const * par, double * d_par, double *hess, double * var_est,
     unsigned const maxvls, double const abs_eps, double const rel_eps,
     int minvls, bool const do_reorder, bool const use_aprx, bool &did_fail,
     double const weight, cdf_methods const method, bool const use_tilting);
};

class pedigree_ll_term_loading {
  /// design matrix for the fixed effects
  arma::mat const X;
  /// design matrix for the coefficients in the scale parameters
  arma::mat const Z;

  unsigned const max_n_sequences;

public:
  /// the scale matrices for the term
  std::vector<arma::mat> const scale_mats;

  arma::uword n_members() const { return X.n_rows; }
  arma::uword n_scale_coefs() const { return Z.n_cols; }
  arma::uword n_fix_effect() const { return X.n_cols; }
  size_t n_scales() const { return scale_mats.size(); }

  size_t n_par() const {
    return n_fix_effect() + n_scales() * n_scale_coefs();
  }

  pedigree_ll_term_loading
    (arma::mat const &X_in, arma::mat const &Z, arma::vec const &y,
     std::vector<arma::mat> const &scale_mats, unsigned const max_threads,
     unsigned const max_n_sequences);

  struct fn_res {
    double log_likelihood;
    double estimator_var;
  };

  /**
   * Approximates the log-likelihood term.
   */
  fn_res fn
    (double const * par, unsigned const maxvls, double const abs_eps,
     double const rel_eps, int minvls, bool const do_reorder,
     bool const use_aprx, bool &did_fail, cdf_methods const method,
     bool const use_tilting);

  /**
   * Approximates the log-likelihood term and the derivative.
   */
  double gr
    (double const * par, double * d_par, double * var_est, unsigned const maxvls,
     double const abs_eps, double const rel_eps, int minvls,
     bool const do_reorder, bool const use_aprx, bool &did_fail,
     double const weight, cdf_methods const method, bool const use_tilting);
};

} // namespace pedmod

#endif
