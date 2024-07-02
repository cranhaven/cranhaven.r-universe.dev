#ifndef CDFAPRX_H
#define CDFAPRX_H

#include "arma-wrap.h"
#include <array>
#include <vector>
#include <limits>
#include <memory>
#include <cmath>
#include "pnorm.h"
#include "qnorm.h"
#include "config.h"
#include <algorithm>
#include "new-mvt.h"
#include "norm-cdf-approx.h"
#include <cmath>
#include <stdexcept>
#include "ped-mem.h"
#include "string"
#include "config.h"
#include "find-tilting-param.h"
#include <limits.h>
#include "qtnorm.h"

#include <R_ext/RS.h> // F77_NAME and F77_CALL

namespace pedmod {
extern "C"
{
  /**
   * @param N Dimension of the integral.
   * @param lower N-vector with lower bounds.
   * @param upper N-vector with upper bounds.
   * @param delta N-vector with mean.
   * @param correl N(N - 1)/2-dimensional vector with  upper triangle of the
   * correlation matrix.
   * @param infin N-dimensional vector indicating whether the bounds are
   * finite.
   * @param pivot not sure. Set it to true.
   * @param y N-dimensional vector with workig memory.
   * @param ND N unless there is double infinite regions.
   * @param A potentially permutated version of lower.
   * @param B potentially permutated version of upper.
   * @param DL potentially permutated version of delta.
   * @param cov N(N + 1)/2-dimensional vector with potentially permutated
   * Cholesky decomposition of correl.
   * @param infi potentially permutated version of infin.
   * @param inform non-zero if something went wrong.
   * @param idx N-dimensional vector with indices of applied permutation.
   * @param doscale logical for whether to scale the cholesky decomposition
   * to have ones in the diagonal.
   *
   * cov is scaled such that the diagonal entries are one. This implies that
   * it is __not__ the Cholesky decomposition of the correlation matrix.
   * A, B, and DL are scaled accordingly.
   */
  void F77_NAME(mvsort)(
      int const* /* N */, double const* /* lower */,
      double const* /* upper */, double const* /* delta */,
      double const* /* correl */, int const* /* infin */,
      double const* /* y */, int const* /* pivot */,
      int* /* ND */, double* /* A */, double* /* B */, double* /* DL */,
      double* /* cov */, int* /* infi */, int* /* inform */,
      int* /* idx */, int const* /* doscale */);

  void F77_NAME(dtpsv)
    (const char * /* uplo */, const char * /* trans */, const char * /* diag */,
     const int * /* n */, const double * /* ap */, double * /* x */,
     const int* /* incx */, size_t, size_t, size_t);

  void F77_NAME(dpptri)
    (const char * /* uplo */, const int * /* n */, double *ap,
     int * /* info */, size_t);
}

/**
 * @parma out Vector with result.
 * @param lower The lower bounds.
 * @param upper The upper bounds.
 */
arma::ivec get_infin
(arma::ivec &out, arma::vec const &lower, arma::vec const &upper);

struct cor_vec_res {
  arma::vec cor_vec, sds;
};

/**
 returns the minimum number of samples as the original Fortran code.
 */
inline int default_minvls(int dim){
  dim = std::max(1, dim);
  constexpr int def_vals[10] =
    { 16L * 31L - 1L, 16L * 47L - 1L, 16L * 73L - 1L, 16L * 113L - 1L, 16L * 173L - 1L, 16L * 263L - 1L, 16L * 397L - 1L, 16L * 593L - 1L, 16L * 907L - 1L, 16L * 1361L - 1L };
  return def_vals[
    std::min(static_cast<int>(dim - 1L), 9)];
}

/**
 * @return a struct with the correlation matrix and standard deviation. The
 * correlation matrix is stored as a upper diagonal matrix.
 */
cor_vec_res get_cor_vec(const arma::mat&);

template<bool lower_tail, bool use_log, bool use_aprx>
double pnorm_use
  (double const x) {
  if constexpr(use_aprx){
    double const pnrm{pnorm_approx(x)};
    if(use_log)
      return lower_tail ? log(pnrm) :  log1p(-pnrm);
    return lower_tail ? pnrm : 1 - pnrm;
  }
  return pnorm_std(x, lower_tail, use_log);
}

/**
 * copies the upper triangular matrix.
 *
 * @param X Matrix to copy.
 * @param x Pointer to copy to.
 */
inline void copy_upper_tri
  (arma::mat const &X, double * PEDMOD_RESTRICT x) noexcept {
  arma::uword const p = X.n_cols;
  for(arma::uword c = 0; c < p; c++)
    for(arma::uword r = 0; r <= c; r++, x++)
      *x = X.at(r, c);
}

/**
 * copies the lower triangular matrix.
 *
 * @param X Matrix to copy.
 * @param x Pointer to copy to.
 */
inline void copy_lower_tri
  (arma::mat const &X, double * PEDMOD_RESTRICT x) noexcept {
  arma::uword const p = X.n_cols;
  for(arma::uword c = 0; c < p; c++)
    for(arma::uword r = c; r < p; r++, x++)
      *x = X.at(r, c);
}

enum cdf_methods : int {
  Korobov = 0,
  Sobol = 1
};

inline cdf_methods get_cdf_methods(int const x){
  if(x < 0 or x > 1)
    throw std::invalid_argument("cdf_methods is not implemented");
  return static_cast<cdf_methods>(x);
}

/**
 * TODO: describe what this class does.
 */
template<class T_Functor>
class cdf {
  T_Functor &functor;
  const arma::uword ndim,
                    n_integrands;
  const bool use_aprx;
  bool is_permutated = false;
  bool use_tilting;

  static constexpr bool
    needs_last_unif = T_Functor::needs_last_unif();

  // cached memory to use
  static cache_mem<int   > imem;
  static cache_mem<double> dmem;

  arma::ivec infin;
  arma::ivec indices;

  double * PEDMOD_RESTRICT const lower      = dmem.get_mem(),
         * PEDMOD_RESTRICT const upper      = lower + ndim,
         * PEDMOD_RESTRICT const sigma_chol = upper + ndim,
         * PEDMOD_RESTRICT const tilt_param =
         sigma_chol + (ndim * (ndim + 1L)) / 2L,
         * PEDMOD_RESTRICT const draw       = tilt_param + ndim,
         * PEDMOD_RESTRICT const dtmp_mem   = draw + ndim * n_qmc_seqs();

  // memory that can be used
  int * const itmp_mem{indices.end()};

  /**
   computes multiple integrands simultaneously.
   */
  template<bool with_tilting, bool with_aprx>
  void evaluate_intrands(
      unsigned const *ndim_in, double const * unifs,
      unsigned const *n_integrands_in,
      double * PEDMOD_RESTRICT integrand_val, unsigned const n_draws) PEDMOD_NOEXCEPT;

public:
  using out_type = typename T_Functor::out_type;

  /**
   * must be called prior to calling the constructor or any member
   * functions.
   */
  static void alloc_mem(unsigned const max_ndim, unsigned const max_threads);

  cdf(T_Functor &functor, arma::vec const &lower_in,
      arma::vec const &upper_in, arma::vec const &mu_in,
      arma::mat const &sigma_in, bool const do_reorder,
      bool const use_aprx, bool const use_tilting_in);

  /**
   TODO: add description.
   */
  void operator()(
      unsigned const *ndim_in, double const * unifs,
      unsigned const *n_integrands_in,
      double * PEDMOD_RESTRICT integrand_val, unsigned const n_draws) PEDMOD_NOEXCEPT;

  /**
   * Performs the approximation.
   *
   * @param maxvls Maximum number of function evaluations allowed.
   * @param abs_eps Required absolute accuracy.
   * @param rel_eps Required relative accuracy.
   * @param minvls Minimum number of samples.
   */
  out_type approximate
  (size_t const maxvls, double const abs_eps, double const rel_eps,
   cdf_methods const method, size_t const minvls, unsigned const n_sequences){
#ifdef DO_CHECKS
    if(abs_eps <= 0 and rel_eps <= 0)
      throw std::invalid_argument("cdf::approximate: no valid convergence threshold");
    if(maxvls <= 0L)
      throw std::invalid_argument("cdf::approximate: invalid 'maxvls'");
#endif

    // setup
    // needs to have at least n_integrands memory to use.
    double * const int_apprx = functor.get_wk_mem(),
           * const int_sdest = int_apprx + n_integrands;

    auto sampler = parallelrng::get_unif_drawer();

    if(ndim == 1L){
      /* handle the one-dimensional case as a special case */
      functor.univariate(int_apprx, lower[0], upper[0]);
      indices[0] = 0;
      // assume that there is zero error in the univariate case
      std::fill(int_sdest, int_sdest + n_integrands, 0.);

      return functor.get_output(int_apprx, int_sdest, 0, 0, 0,
                                indices.begin());

    } else if(std::isinf(*sigma_chol))
      throw std::runtime_error("std::isinf(*sigma_chol.begin())");

    /* perform the approximation */
    auto res = ([&]() -> rand_Korobov_output {
      if(method == cdf_methods::Sobol)
        return sobol_wrapper<cdf<T_Functor> >::comp(
            *this, ndim, minvls, maxvls, n_integrands, abs_eps, rel_eps,
            int_apprx, int_sdest, sampler, sobol::scrambling_type::owen,
            n_sequences);
      if(method != cdf_methods::Korobov)
        throw std::invalid_argument("method is not implemented");

      return rand_Korobov<cdf<T_Functor> >::comp(
          *this, ndim, minvls, maxvls, n_integrands, abs_eps, rel_eps,
          int_apprx, int_sdest, sampler, n_sequences);
    })();

    return functor.get_output(int_apprx, int_sdest, res.minvls, res.inform,
                              res.abserr, indices.begin());
  }
};

template<class T_Functor>
cache_mem<int   > cdf<T_Functor>::imem;
template<class T_Functor>
cache_mem<double> cdf<T_Functor>::dmem;

/**
 * functor classes used as template argument for cdf used to approximate the
 * likelihood. */
class likelihood {
  static cache_mem<double> dmen;

public:
  static void alloc_mem
  (unsigned const max_dim, unsigned const max_threads,
   unsigned const max_n_sequences){
    rand_Korobov<cdf<likelihood> >::alloc_mem(
        max_dim, get_n_integrands(), max_threads);
    sobol_wrapper<cdf<likelihood> >::alloc_mem(
        max_dim, get_n_integrands(), max_threads, max_n_sequences);
    dmen.set_n_mem(2, max_threads);
  }

  double * get_wk_mem(){
    return dmen.get_mem();
  }

  constexpr static unsigned get_n_integrands() {
    return 1;
  }

  void operator()
    (double const *, double * out, int const *, bool const,
     unsigned const n_draws)
    PEDMOD_NOEXCEPT {
#ifdef DO_CHECKS
    if(!out)
      throw std::invalid_argument("likelihood::operator(): invalid out");
#endif
    std::fill(out, out + n_draws, 1);
  }

  constexpr static bool needs_last_unif() {
    return false;
  }

  constexpr static double get_norm_constant() {
    return 1;
  }

  inline static void univariate(double * out,
                                double const lw, double const ub) {
    double const p_ub = std::isinf(ub) ? 1 : pnorm_std(ub, 1L, 0L),
                 p_lb = std::isinf(lw) ? 0 : pnorm_std(lw, 1L, 0L);
    *out = p_ub - p_lb;
  }

  struct out_type {
    /**
     * minvls Actual number of function evaluations used.
     * inform INFORM = 0 for normal exit, when
     *             ABSERR <= MAX(ABSEPS, RELEPS*||finest||)
     *          and
     *             INTVLS <= MAXCLS.
     *        INFORM = 1 If MAXVLS was too small to obtain the required
     *        accuracy. In this case a value finest is returned with
     *        estimated absolute accuracy ABSERR. */
    size_t minvls;
    int inform;
    /// maximum norm of estimated absolute accuracy of finest
    double abserr;
    /// likelihood approximation
    double likelihood;
  };

  out_type get_output(double const * res, double const * sdest,
                      size_t const minvls, int const inform, double const abserr,
                      int const *){
    return out_type { minvls, inform, abserr, *res };
  }

  void prep_permutated(arma::mat const&, int const*) { }
};

/**
 * functor classes used as template argument for cdf used to approximate the
 * derivatives of the likelihood factors for each family. That is, the
 * likelihood
 *
 *   int_B phi(u;X.beta, I + sum_k sigma[k] * C[k]) du
 *
 * For given matrices C and a design matrix X.
 *
 * The returned approximations is a) the likelihood factor and b) the
 * derivative of log likelihood w.r.t. the fixed effect coefficients and w.r.t.
 * each of the scale parameters.
 */
class pedigree_l_factor {
public:
  /// the scale matrices for the different effects
  std::vector<arma::mat> const scale_mats;
  /// the number of members in this family
  arma::uword const n_mem = scale_mats[0].n_rows;
  /// design matrix for the fixed effects
  arma::mat const X;
  /// the number of fixed effects
  arma::uword const n_fix = X.n_cols,
                 n_scales = scale_mats.size(),
             n_integrands = 1 + n_fix + n_scales;
  /// scale free constant to check that a matrix is positive semi definite
  static constexpr double eps_pos_def =
    10 * std::numeric_limits<double>::epsilon();

private:
  /// working memory
  static cache_mem<double> dmem;
  static cache_mem<int   > imem;

  /**
   * the matrix  [cluster size] x [number of fixed effect] matrix which is
   * needed to compute the derivatives w.r.t. the slopes for the fixed effects.
   */
  double * d_fix_mat;

  /**
   * Let S^top S = Sigma be the Cholesky decomposition and C_{i1}, ..., C_{iK}
   * be the scale matrices. Then this is the first pointer to the following for
   * each of the K scale matrices:
   *  a. the upper triangular part of the Cholesky decomposition of
   *     S^-top C_{i1}S^-1, ..., S^-top C_{iK}S^-1 if the matrix
   *     S^-top C_{ik}S^-1 is positive definite.
   *  b. the transpose of the Eigen vectors Q scaled by the square root of the
   *     Eigen values where QDQ^top = S^-top C_{i1}S^-1.
   *
   * These objects can be found increments of n_mem * n_mem
   */
  double * S_C_S_matrices;

  /**
   * stores how many non-zero Eigen values each S_C_S_matrices has. It is minus
   * one if it is a Cholesky decomposition is used.
   */
  int * S_C_n_eigen;

  /// points to the upper triangular part of the inverse.
  double * sig_inv;

  /// working memory to be used by cdf
  double * cdf_mem;

  /// working memory that can be used for anything
  double * interal_mem;

  /// array of pointer to the scale matrices' element which we will need.
  std::unique_ptr<double const *[]> scale_mats_ptr =
    std::unique_ptr<double const *[]>(new double const *[n_scales]);

  /// the normalization constant
  double norm_const = std::numeric_limits<double>::quiet_NaN();

public:
  /// sets the scale matrices. There are no checks on the validity
  pedigree_l_factor(std::vector<arma::mat> const &scale_mats,
                    unsigned const max_threads, arma::mat const &X_in,
                    unsigned const max_n_sequences);

  unsigned get_n_integrands() PEDMOD_NOEXCEPT {
    return n_integrands;
  }

  double * get_wk_mem() PEDMOD_NOEXCEPT {
    return cdf_mem;
  }

  constexpr static bool needs_last_unif() PEDMOD_NOEXCEPT {
    return true;
  }

  double get_norm_constant() PEDMOD_NOEXCEPT {
    return norm_const;
  }

  /**
   * setups the covariance matrix to use. This method must be called be
   * prior to using the object in an approximation.
   *
   * Args:
   *   sig: the covariance matrix.
   *   scales: scale parameters.
   *   norm_constant_arg: the normalization constant.
   *   only_cov: true if only the covariance matrix should be computed
   */
  void setup(arma::mat &sig, double const *scales,
             double const norm_constant_arg,
             bool const only_cov = false);

  void prep_permutated(arma::mat const &sig, int const *indices);

  void operator()
    (double const * PEDMOD_RESTRICT draw, double * PEDMOD_RESTRICT out,
     int const *, bool const, unsigned const n_draws);

  void univariate(double * out, double const lw, double const ub);

  struct out_type {
    /**
     * minvls Actual number of function evaluations used.
     * inform INFORM = 0 for normal exit, when
     *             ABSERR <= MAX(ABSEPS, RELEPS*||finest||)
     *          and
     *             INTVLS <= MAXCLS.
     *        INFORM = 1 If MAXVLS was too small to obtain the required
     *        accuracy. In this case a value finest is returned with
     *        estimated absolute accuracy ABSERR. */
    size_t minvls;
    int inform;
    /// maximum estimated absolute accuracy of finest
    double abserr;
    /// likelihood approximation
    double likelihood;
    /// the derivative approximation
    arma::vec derivs;
    /// the approximate standard errors
    arma::vec sd_errs;
  };

  out_type get_output
    (double * res,  double const * sdest, size_t const minvls,
     int const inform, double const abserr, int const *indices);
};

/**
 * functor classes used as template argument for cdf used to approximate the
 * derivatives and Hessian of the log likelihood factor of each family like
 * pedigree_l_factor for derivative for given matrices C and a design matrix X.
 *
 * The returned approximations is a) the likelihood factor and b) the
 * derivative of log likelihood w.r.t. the fixed effect coefficients and w.r.t.
 * each of the scale parameters.
 */
class pedigree_l_factor_Hessian {
public:
  /// the scale matrices for the different effects
  std::vector<arma::mat> const scale_mats;
  /// the number of members in this family
  arma::uword const n_mem = scale_mats[0].n_rows;
  /// design matrix for the fixed effects
  arma::mat const X;
  /// the number of fixed effects
  arma::uword const n_fix = X.n_cols,
                 n_scales = scale_mats.size(),
       n_integrands_inner =
         1 + n_mem * (1 + n_mem) + (n_fix + n_scales) * (n_fix + n_scales),
       n_integrands_outer = 1 + (n_fix + n_scales) * (1 + n_fix + n_scales),
             n_integrands = std::max(n_integrands_inner, n_integrands_outer);

private:
  /// working memory
  static cache_mem<double> dmem;

  /// working memory to be used by cdf
  double * cdf_mem;

  /**
   * the upper triangular part of the Cholesky decomposition of the of the
   * covariance matrix
   */
  double *vcov_chol;

  /// the inverse of the covariance matrix
  double *vcov_inv;

  /// the permuted version of X
  double *X_permu;

  /// pointers to possibly permuted versions of scale_mats
  std::vector<double*> scale_mats_permu = std::vector<double*>(n_scales);

  /// working memory that can be used for anything
  double *interal_mem;

  /// the normalization constant
  double norm_const = std::numeric_limits<double>::quiet_NaN();

public:
  /// sets the scale matrices. There are no checks on the validity
  pedigree_l_factor_Hessian
    (std::vector<arma::mat> const &scale_mats, unsigned const max_threads,
     arma::mat const &X_in, unsigned const max_n_sequences);

  unsigned get_n_integrands() PEDMOD_NOEXCEPT {
    return n_integrands;
  }

  double * get_wk_mem() PEDMOD_NOEXCEPT {
    return cdf_mem;
  }

  constexpr static bool needs_last_unif() PEDMOD_NOEXCEPT {
    return true;
  }

  double get_norm_constant() PEDMOD_NOEXCEPT {
    return norm_const;
  }

  /**
   * setups the covariance matrix to use. This method must be called be
   * prior to using the object in an approximation.
   *
   * Args:
   *   sig: the covariance matrix.
   *   scales: scale parameters.
   *   norm_constant_arg: the normalization constant.
   */
  void setup
    (arma::mat &sig, double const *scales, double const norm_constant_arg);

  void prep_permutated(arma::mat const &sig, int const *indices);

  void operator()
    (double const * PEDMOD_RESTRICT draw, double * PEDMOD_RESTRICT out,
     int const *, bool const, unsigned const n_draws);

  void univariate(double * out, double const lw, double const ub);

  struct out_type {
    /**
     * minvls Actual number of function evaluations used.
     * inform INFORM = 0 for normal exit, when
     *             ABSERR <= MAX(ABSEPS, RELEPS*||finest||)
     *          and
     *             INTVLS <= MAXCLS.
     *        INFORM = 1 If MAXVLS was too small to obtain the required
     *        accuracy. In this case a value finest is returned with
     *        estimated absolute accuracy ABSERR. */
    size_t minvls;
    int inform;
    /// maximum estimated absolute accuracy of finest
    double abserr;
    /// likelihood approximation
    double likelihood;
    /// the gradient approximation
    arma::vec gradient;
    /// the hessian approximation
    arma::vec hessian;
    /// the approximate standard errors
    arma::vec sd_errs;
  };

  out_type get_output
    (double * res,  double const * sdest, size_t const minvls,
     int const inform, double const abserr, int const *indices);
};

/**
 * Computes the derivatives for the integral
 *
 *   int_B phi(u; mu, Sigma) du
 *
 * The first element of the returned object is the likelihood factor. The
 * additional elements are the derivatives w.r.t. mu and Sigma with the latter
 * stored as the full k x k matrix ignoring the symmetry.
 */
class generic_l_factor {
  unsigned const n_vars,
           n_integrands = get_n_integrands(n_vars);

  /// working memory
  static cache_mem<double> dmem;

  double * cdf_mem() const { return dmem.get_mem(); }

  double * Sig_chol_tri() { return cdf_mem() + 2 * n_integrands; }

  double * internal_mem() {
    return Sig_chol_tri() + (n_vars * (n_vars + 1)) / 2;
  }

  /// the normalization constant
  double const norm_const;

  static unsigned get_n_integrands(unsigned const max_dim){
    return 1 + (max_dim * (max_dim + 3)) / 2;
  }

public:
  /// must be called prior to calling the member functions in the class
  static void alloc_mem
    (unsigned const max_dim, unsigned const max_threads,
     unsigned const max_n_sequences);

  generic_l_factor(arma::vec const &mu, arma::mat const &Sig,
                   double const norm_const):
    n_vars{mu.n_elem}, norm_const{norm_const} {
      if(mu.n_elem != Sig.n_rows)
        throw std::invalid_argument("mu.n_elem != Sig.n_rows");
      else if(Sig.n_cols != Sig.n_rows)
        throw std::invalid_argument("Sig.n_cols != Sig.n_rows");
    }

  void prep_permutated(arma::mat const &Sig, int const *indices) {
    arma::mat Sig_chol = arma::chol(Sig);
    copy_upper_tri(Sig_chol, Sig_chol_tri());
  }

  unsigned get_n_integrands() PEDMOD_NOEXCEPT {
    return n_integrands;
  }

  double * get_wk_mem() PEDMOD_NOEXCEPT {
    return cdf_mem();
  }

  constexpr static bool needs_last_unif() PEDMOD_NOEXCEPT {
    return true;
  }

  double get_norm_constant() PEDMOD_NOEXCEPT {
    return norm_const;
  }

  void operator()
  (double const * PEDMOD_RESTRICT draw, double * PEDMOD_RESTRICT out,
   int const *, bool const, unsigned const n_draws);

  void univariate(double * out, double const lw, double const ub);

  struct out_type {
    /**
     * minvls Actual number of function evaluations used.
     * inform INFORM = 0 for normal exit, when
     *             ABSERR <= MAX(ABSEPS, RELEPS*||finest||)
     *          and
     *             INTVLS <= MAXCLS.
     *        INFORM = 1 If MAXVLS was too small to obtain the required
     *        accuracy. In this case a value finest is returned with
     *        estimated absolute accuracy ABSERR. */
    size_t minvls;
    int inform;
    /// maximum estimated absolute accuracy of finest
    double abserr;
    /// likelihood approximation
    double likelihood;
    /// the derivative approximation
    arma::vec derivs;
    /// the approximate standard errors
    arma::vec sd_errs;
  };

  out_type get_output(double * res,  double const * sdest, size_t const minvls,
                      int const inform, double const abserr,
                      int const *indices);
};

} // namespace pedmod

#endif
