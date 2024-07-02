#include "pedigree-ll.h"
#include "ped-mem.h"
#include "openmp-exception_ptr.h"
#include <algorithm>

//' Multivariate Normal Distribution CDF and Its Derivative
//' @description
//' Provides an approximation of the multivariate normal distribution CDF
//' over a hyperrectangle and the derivative with respect to the mean vector
//' and the covariance matrix.
//'
//' @param lower numeric vector with lower bounds.
//' @param upper numeric vector with upper bounds.
//' @param mu numeric vector with means.
//' @param sigma covariance matrix.
//' @param maxvls maximum number of samples in the approximation.
//' @param abs_eps absolute convergence threshold.
//' @param rel_eps relative convergence threshold.
//' @param minvls minimum number of samples. Negative values provides a
//' default which depends on the dimension of the integration.
//' @param do_reorder \code{TRUE} if a heuristic variable reordering should
//' be used. \code{TRUE} is likely the best value.
//' @param use_aprx \code{TRUE} if a less precise approximation of
//' \code{\link{pnorm}} and \code{\link{qnorm}} should be used. This may
//' reduce the computation time while not affecting the result much.
//' @param method integer with the method to use. Zero yields randomized Korobov
//' lattice rules while one yields scrambled Sobol sequences.
//' @param n_sequences number of randomized quasi-Monte Carlo sequences to use.
//' More samples yields a better estimate of the error but a worse
//' approximation. Eight is used in the original Fortran code. If one is
//' used then the error will be set to zero because it cannot be estimated.
//' @param use_tilting \code{TRUE} if the minimax tilting method suggested
//' by Botev (2017) should be used. See \doi{10.1111/rssb.12162}.
//'
//' @return
//' \code{mvndst:}
//' An approximation of the CDF. The \code{"n_it"} attribute shows the number of
//' integrand evaluations, the \code{"inform"} attribute is zero if the
//' requested precision is achieved, and the \code{"abserr"} attribute
//' shows 3.5 times the estimated standard error.
//'
//' @examples
//' # simulate covariance matrix and the upper bound
//' set.seed(1)
//' n <- 10L
//' S <- drop(rWishart(1L, 2 * n, diag(n) / 2 / n))
//' u <- rnorm(n)
//'
//' system.time(pedmod_res <- mvndst(
//'     lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
//'     maxvls = 1e6, abs_eps = 0, rel_eps = 1e-4, use_aprx = TRUE))
//' pedmod_res
//'
//' # compare with mvtnorm
//' if(require(mvtnorm)){
//'     mvtnorm_time <- system.time(mvtnorm_res <- mvtnorm::pmvnorm(
//'         upper = u, sigma = S, algorithm = GenzBretz(
//'             maxpts = 1e6, abseps = 0, releps = 1e-4)))
//'     cat("mvtnorm_res:\n")
//'     print(mvtnorm_res)
//'
//'     cat("mvtnorm_time:\n")
//'     print(mvtnorm_time)
//' }
//'
//' # with titling
//' system.time(pedmod_res <- mvndst(
//'     lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
//'     maxvls = 1e6, abs_eps = 0, rel_eps = 1e-4, use_tilting = TRUE))
//' pedmod_res
//'
//' # compare with TruncatedNormal
//' if(require(TruncatedNormal)){
//'     TruncatedNormal_time <- system.time(
//'         TruncatedNormal_res <- TruncatedNormal::pmvnorm(
//'             lb = rep(-Inf, n), ub = u, sigma = S,
//'             B = attr(pedmod_res, "n_it"), type = "qmc"))
//'     cat("TruncatedNormal_res:\n")
//'     print(TruncatedNormal_res)
//'
//'     cat("TruncatedNormal_time:\n")
//'     print(TruncatedNormal_time)
//' }
//'
//' # check the gradient
//' system.time(pedmod_res <- mvndst_grad(
//'   lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
//'   maxvls = 1e5, minvls = 1e5, abs_eps = 0, rel_eps = 1e-4, use_aprx = TRUE))
//' pedmod_res
//'
//' \donttest{# compare with numerical differentiation. Should give the same up to Monte
//' # Carlo and finite difference error
//' if(require(numDeriv)){
//'   num_res <- grad(
//'     function(par){
//'       set.seed(1)
//'       mu <- head(par, n)
//'       S[upper.tri(S, TRUE)] <- tail(par, -n)
//'       S[lower.tri(S)] <- t(S)[lower.tri(S)]
//'       mvndst(
//'         lower = rep(-Inf, n), upper = u, sigma = S, mu = mu,
//'         maxvls = 1e4, minvls = 1e4, abs_eps = 0, rel_eps = 1e-4,
//'         use_aprx = TRUE)
//'     }, c(numeric(n), S[upper.tri(S, TRUE)]),
//'     method.args = list(d = .01, r = 2))
//'
//'   d_mu <- head(num_res, n)
//'   d_sigma <- matrix(0, n, n)
//'   d_sigma[upper.tri(d_sigma, TRUE)] <- tail(num_res, -n)
//'   d_sigma[upper.tri(d_sigma)] <- d_sigma[upper.tri(d_sigma)] / 2
//'   d_sigma[lower.tri(d_sigma)] <- t(d_sigma)[lower.tri(d_sigma)]
//'
//'   cat("numerical derivatives\n")
//'   print(rbind(numDeriv = d_mu,
//'               pedmod = pedmod_res$d_mu))
//'   print(d_sigma)
//'   cat("\nd_sigma from pedmod\n")
//'   print(pedmod_res$d_sigma) # for comparison
//' }}
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector mvndst
  (arma::vec const &lower, arma::vec const &upper, arma::vec const &mu,
   arma::mat const &sigma, unsigned const maxvls = 25000,
   double const abs_eps = .001, double const rel_eps = 0,
   int minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, int const method = 0,
   unsigned const n_sequences = 8, bool const use_tilting = false){
  arma::uword const n = lower.n_elem;
  if(upper.n_elem != n)
    throw std::invalid_argument("mvndst: invalid upper");
  if(mu.n_elem != n)
    throw std::invalid_argument("mvndst: invalid mu");
  if(sigma.n_cols != n or sigma.n_rows != n)
    throw std::invalid_argument("mvndst: invalid sigma");
  if(!std::isfinite(abs_eps) or !std::isfinite(rel_eps))
    throw std::invalid_argument("mvndst: invalid abs_eps or rel_eps");

  if(minvls < 0)
    minvls = pedmod::default_minvls(lower.n_elem);

  if(maxvls < static_cast<unsigned>(minvls) or maxvls < 1)
    throw std::invalid_argument("mvndst: invalid maxvls");

  pedmod::likelihood func;
  parallelrng::set_rng_seeds(1);

  pedmod::cdf<pedmod::likelihood>::alloc_mem(lower.n_elem, 1);
  pedmod::likelihood::alloc_mem(lower.n_elem, 1, n_sequences);
  auto const out = pedmod::cdf<pedmod::likelihood>(
    func, lower, upper, mu, sigma, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, pedmod::get_cdf_methods(method), minvls,
        n_sequences);

  Rcpp::NumericVector res(1);
  res[0] = out.likelihood;
  res.attr("n_it")   = Rcpp::IntegerVector::create(out.minvls);
  res.attr("inform") = Rcpp::IntegerVector::create(out.inform);
  res.attr("abserr") = Rcpp::NumericVector::create(out.abserr);
  return res;
}

//' @rdname mvndst
//' @return
//' \code{mvndst_grad:}
//' A list with
//' \itemize{
//'   \item \code{likelihood}: the likelihood approximation.
//'   \item \code{d_mu}: the derivative with respect to the the mean vector.
//'   \item \code{d_sigma}: the derivative with respect to the covariance matrix
//'   ignoring the symmetry (i.e. working the \eqn{n^2} parameters with
//'   \eqn{n} being the dimension rather than the \eqn{n(n + 1) / 2}
//'   free parameters).
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List mvndst_grad
  (arma::vec const &lower, arma::vec const &upper, arma::vec const &mu,
   arma::mat const &sigma, unsigned const maxvls = 25000,
   double const abs_eps = .001, double const rel_eps = 0,
   int minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, int const method = 0,
   unsigned const n_sequences = 8, bool const use_tilting = false){
  arma::uword const n = lower.n_elem;
  if(upper.n_elem != n)
    throw std::invalid_argument("mvndst: invalid upper");
  if(mu.n_elem != n)
    throw std::invalid_argument("mvndst: invalid mu");
  if(sigma.n_cols != n or sigma.n_rows != n)
    throw std::invalid_argument("mvndst: invalid sigma");
  if(!std::isfinite(abs_eps) or !std::isfinite(rel_eps))
    throw std::invalid_argument("mvndst: invalid abs_eps or rel_eps");

  if(minvls < 0)
    minvls = pedmod::default_minvls(lower.n_elem);

  if(maxvls < static_cast<unsigned>(minvls) or maxvls < 1)
    throw std::invalid_argument("mvndst: invalid maxvls");

  pedmod::generic_l_factor func(mu, sigma, 1);
  parallelrng::set_rng_seeds(1);

  pedmod::cdf<pedmod::generic_l_factor>::alloc_mem(lower.n_elem, 1);
  pedmod::generic_l_factor::alloc_mem(lower.n_elem, 1, n_sequences);
  auto const out = pedmod::cdf<pedmod::generic_l_factor>(
    func, lower, upper, mu, sigma, do_reorder, use_aprx,
    use_tilting).approximate(
        maxvls, abs_eps, rel_eps, pedmod::get_cdf_methods(method), minvls,
        n_sequences);

  Rcpp::NumericVector d_mu(n);
  Rcpp::NumericMatrix d_sig(n, n);
  std::copy(out.derivs.begin(), out.derivs.begin() + n, d_mu.begin());
  std::copy(out.derivs.begin() + n, out.derivs.end(), d_sig.begin());

  {
    auto scale_entries = [&](double &x){ x *= out.likelihood; };
    std::for_each(d_mu.begin(), d_mu.end(), scale_entries);
    std::for_each(d_sig.begin(), d_sig.end(), scale_entries);
  }

  Rcpp::List res = Rcpp::List::create(
    Rcpp::Named("likelihood") = out.likelihood,
    Rcpp::Named("d_mu") = std::move(d_mu),
    Rcpp::Named("d_sigma") = std::move(d_sig));

  res.attr("n_it")   = Rcpp::IntegerVector::create(out.minvls);
  res.attr("inform") = Rcpp::IntegerVector::create(out.inform);
  res.attr("abserr") = Rcpp::NumericVector::create(out.abserr);

  return res;
}

namespace {

arma::vec check_n_get_vls_scales
  (Rcpp::Nullable<Rcpp::NumericVector> vls_scales, size_t const n_terms,
   unsigned const maxvls){
  if(vls_scales.isNotNull()){
    Rcpp::NumericVector r_weights(vls_scales);
    if(static_cast<size_t>(r_weights.size()) != n_terms)
      throw std::invalid_argument(
          "invalid size of vls_scales. Should have length " +
            std::to_string(n_terms) +  " had length " +
            std::to_string(r_weights.size()) + ".");

    arma::vec out = r_weights;
    std::for_each(out.begin(), out.end(), [&](double const x){
      if(x * maxvls < 1)
        throw std::runtime_error("vls_scales[i] * maxvls < 1");
    });

    return out;
  }

  return {};
}

arma::vec check_n_get_cluster_weights
  (Rcpp::Nullable<Rcpp::NumericVector> cluster_weights, size_t const n_terms){
  if(cluster_weights.isNotNull()){
    Rcpp::NumericVector r_weights(cluster_weights);
    if(static_cast<size_t>(r_weights.size()) != n_terms)
      throw std::invalid_argument(
          "invalid size of cluster_weights. Should have length " +
            std::to_string(n_terms) +  " had length " +
            std::to_string(r_weights.size()) + ".");
    return r_weights;
  }

  return {};
}

struct pedigree_terms {
  unsigned const max_threads;
  std::vector<pedmod::pedigree_ll_term > terms;

  pedigree_terms(Rcpp::List data, unsigned const max_threads,
                 unsigned const n_sequences):
    max_threads(std::max(1U, max_threads)) {
    terms.reserve(data.size());
    for(auto x : data){
      Rcpp::List xl(static_cast<SEXP>(x)),
      s_mats(static_cast<SEXP>(xl["scale_mats"]));

      arma::mat const X = Rcpp::as<arma::mat>(xl["X"]);
      arma::vec const y = Rcpp::as<arma::vec>(xl["y"]);
      if(y.n_elem > 1000 or y.n_elem < 1)
        throw std::invalid_argument(
            "pedigree_terms: Either dimension zero or dimension greater than 1000");

      std::vector<arma::mat> scale_mats;
      scale_mats.reserve(s_mats.size());
      for(auto &s : s_mats)
        scale_mats.emplace_back(Rcpp::as<arma::mat>(s));

      terms.emplace_back(X, y, scale_mats, max_threads, n_sequences);
    }

    // checks
    if(terms.size() < 1)
      throw std::invalid_argument("pedigree_terms: no terms");
    unsigned const n_fix = terms[0].n_fix_effect(),
                n_scales = terms[0].n_scales();
    for(auto &tr : terms){
      if(tr.n_fix_effect() != n_fix)
        throw std::invalid_argument("pedigree_terms: number of fixed effects do not match");
      if(tr.n_scales() != static_cast<size_t>(n_scales))
        throw std::invalid_argument("pedigree_terms: number of scale matrices do not match");
    }
  }
};

struct pedigree_terms_loading {
  unsigned const max_threads;
  std::vector<pedmod::pedigree_ll_term_loading> terms;

  pedigree_terms_loading
    (Rcpp::List data, unsigned const max_threads, unsigned const n_sequences):
    max_threads(std::max(1U, max_threads)) {
    terms.reserve(data.size());
    for(auto x : data){
      Rcpp::List xl(static_cast<SEXP>(x)),
             s_mats(static_cast<SEXP>(xl["scale_mats"]));

      arma::mat const X = Rcpp::as<arma::mat>(xl["X"]);
      arma::mat const Z = Rcpp::as<arma::mat>(xl["Z"]);
      arma::vec const y = Rcpp::as<arma::vec>(xl["y"]);
      if(y.n_elem > 1000 or y.n_elem < 1)
        throw std::invalid_argument(
            "pedigree_terms_loading: Either dimension zero or dimension greater than 1000");

      std::vector<arma::mat> scale_mats;
      scale_mats.reserve(s_mats.size());
      for(auto &s : s_mats)
        scale_mats.emplace_back(Rcpp::as<arma::mat>(s));

      terms.emplace_back(X, Z, y, scale_mats, max_threads, n_sequences);
    }

    // checks
    if(terms.size() < 1)
      throw std::invalid_argument("pedigree_terms_loading: no terms");
    unsigned const n_fix = terms[0].n_fix_effect(),
           n_scale_coefs = terms[0].n_scale_coefs(),
                n_scales = terms[0].n_scales();
    for(auto &tr : terms){
      if(tr.n_fix_effect() != n_fix)
        throw std::invalid_argument("pedigree_terms_loading: number of fixed effects do not match");
      if(tr.n_scale_coefs() != n_scale_coefs)
        throw std::invalid_argument("pedigree_terms_loading: number of coefficients for the scale parameters do not match");
      if(tr.n_scales() != static_cast<size_t>(n_scales))
        throw std::invalid_argument("pedigree_terms_loading: number of scale matrices do not match");
    }
  }
};

template<class TermsObj>
Rcpp::IntegerVector get_indices
  (Rcpp::Nullable<Rcpp::IntegerVector> indices,
   TermsObj const &terms){
  if(indices.isNotNull())
    return Rcpp::IntegerVector(indices);

  Rcpp::IntegerVector out(terms.terms.size());
  for(int i = 0; i < out.size(); ++i)
    out[i] = i;
  return out;
}

template<class TermsObj>
unsigned eval_get_n_threads
  (unsigned const n_threads, TermsObj const &terms){
#ifndef _OPENMP
  return 1L;
#endif

  if(n_threads > terms.max_threads){
    Rcpp::Function warning("warning");
    warning("Cannot set the number of threads to ", std::to_string(n_threads),
            ". The object is created with a maximum of ",
            std::to_string(terms.max_threads), " threads.");
  }

  return std::min(n_threads, terms.max_threads);
}
} // namespace

//' Get a C++ Object for Log Marginal Likelihood Approximations
//'
//' @description
//' Constructs an object needed for \code{\link{eval_pedigree_ll}} and
//' \code{\link{eval_pedigree_grad}}.
//'
//' @param data \code{\link{list}} where each element is a list for a cluster
//' with an:
//' \itemize{
//'   \item{\code{"X"}}{ element with the design matrix for the fixed effect,}
//'   \item{\code{"Z"}}{ element with the design matrix for the loadings of the effects (only needed for \code{pedigree_ll_terms_loadings}),}
//'   \item{\code{"y"}}{ element with the zero-one outcomes, and}
//'   \item{\code{"scale_mats"}}{ element with a list where each element is a
//' scale/correlation matrix for a particular type of effect.}
//' }
//' @param max_threads maximum number of threads to use.
//' @param n_sequences number of randomized quasi-Monte Carlo sequences to use.
//' More samples yields a better estimate of the error but a worse
//' approximation. Eight is used in the original Fortran code. If one is
//' used then the error will be set to zero because it cannot be estimated.
//'
//' @details
//' An intercept column is not added to the \code{X} matrices
//' like what \code{\link{lm.fit}} and \code{\link{glm.fit}} do.
//' Thus, it is often important that the user adds an intercept column
//' to these matrices as it is hardly ever justified to not include the
//' intercept (the exceptions being e.g. when splines are used which include
//' the intercept and with certain dummy designs). This equally holds for
//' the \code{Z} matrices with \code{pedigree_ll_terms_loadings}.
//'
//' \code{pedigree_ll_terms_loadings} relax the assumption that the scale
//' parameter is the same for all individuals. \code{pedigree_ll_terms_loadings}
//' and \code{pedigree_ll_terms} yield the same model if \code{"Z"} is an
//' intercept column for all families but with a different parameterization.
//' In this case, \code{pedigree_ll_terms} will be
//' faster. See \code{vignette("pedmod", "pedmod")} for examples of using
//' \code{pedigree_ll_terms_loadings}.
//'
//' @examples
//' # three families as an example
//' fam_dat <- list(
//'   list(
//'     y = c(FALSE, TRUE, FALSE, FALSE),
//'     X = structure(c(
//'       1, 1, 1, 1, 1.2922654151273, 0.358134905909256, -0.734963997107464,
//'       0.855235473516044, -1.16189500386223, -0.387298334620742,
//'       0.387298334620742, 1.16189500386223),
//'       .Dim = 4:3, .Dimnames = list( NULL, c("(Intercept)", "X1", ""))),
//'     rel_mat = structure(c(
//'       1, 0.5, 0.5, 0.125, 0.5, 1, 0.5, 0.125, 0.5, 0.5,
//'       1, 0.125, 0.125, 0.125, 0.125, 1), .Dim = c(4L, 4L)),
//'     met_mat = structure(c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1),
//'                         .Dim = c(4L, 4L))),
//'   list(
//'     y = c(FALSE, FALSE, FALSE),
//'     X = structure(c(
//'       1, 1, 1, -0.0388728997202442, -0.0913782435233639,
//'       -0.0801619722392612, -1, 0, 1), .Dim = c(3L, 3L)),
//'     rel_mat = structure(c(
//'       1, 0.5, 0.125, 0.5, 1, 0.125, 0.125, 0.125, 1), .Dim = c(3L, 3L)),
//'     met_mat = structure(c(
//'       1, 1, 0, 1, 1, 0, 0, 0, 1), .Dim = c(3L, 3L))),
//'   list(
//'     y = c(TRUE, FALSE),
//'     X = structure(c(
//'       1, 1, 0.305275750370738, -1.49482995913648,  -0.707106781186547,
//'       0.707106781186547),
//'       .Dim = 2:3, .Dimnames = list( NULL, c("(Intercept)", "X1", ""))),
//'     rel_mat = structure(c(1, 0.5,  0.5, 1), .Dim = c(2L, 2L)),
//'     met_mat = structure(c(1, 1, 1, 1), .Dim = c(2L,  2L))))
//'
//' # get the data into the format needed for the package
//' dat_arg <- lapply(fam_dat, function(x){
//'   # we need the following for each family:
//'   #   y: the zero-one outcomes.
//'   #   X: the design matrix for the fixed effects.
//'   #   scale_mats: list with the scale matrices for each type of effect.
//'   list(y = as.numeric(x$y), X = x$X,
//'        scale_mats = list(x$rel_mat, x$met_mat))
//' })
//'
//' # get a pointer to the C++ object
//' ptr <- pedigree_ll_terms(dat_arg, max_threads = 1L)
//'
//' # get the argument for a the version with loadings
//' dat_arg_loadings <- lapply(fam_dat, function(x){
//'   list(y = as.numeric(x$y), X = x$X, Z = x$X[, 1:2],
//'        scale_mats = list(x$rel_mat, x$met_mat))
//' })
//'
//' ptr <- pedigree_ll_terms_loadings(dat_arg_loadings, max_threads = 1L)
//'
//' @export
// [[Rcpp::export]]
SEXP pedigree_ll_terms(Rcpp::List data, unsigned const max_threads = 1,
                       unsigned const n_sequences = 8){
  Rcpp::XPtr<pedigree_terms> out(
    new pedigree_terms(data, max_threads, n_sequences));
  out.attr("class") = "pedigree_ll_terms_ptr";
  return out;
}

// [[Rcpp::export(.get_n_scales, rng = false)]]
int get_n_scales(SEXP ptr){
  return Rcpp::XPtr<pedigree_terms>(ptr)->terms[0].n_scales();
}

// [[Rcpp::export(.get_n_scales_loadings, rng = false)]]
int get_n_scales_loadings(SEXP ptr){
  Rcpp::XPtr<pedigree_terms_loading> obj(ptr);
  return obj->terms[0].n_scales() * obj->terms[0].n_scale_coefs();
}

// [[Rcpp::export(.get_n_terms, rng = false)]]
int get_n_terms(SEXP ptr){
  return Rcpp::XPtr<pedigree_terms>(ptr)->terms.size();
}

// [[Rcpp::export(.get_n_terms_loadings, rng = false)]]
int get_n_terms_loadings(SEXP ptr){
  return Rcpp::XPtr<pedigree_terms_loading>(ptr)->terms.size();
}

// [[Rcpp::export("eval_pedigree_ll_cpp")]]
Rcpp::NumericVector eval_pedigree_ll
  (SEXP ptr, arma::vec par, int const maxvls,
   double const abs_eps, double const rel_eps,
   Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue,
   int const minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, unsigned n_threads = 1L,
   Rcpp::Nullable<Rcpp::NumericVector> cluster_weights = R_NilValue,
   int const method = 0, bool const use_tilting = false,
   Rcpp::Nullable<Rcpp::NumericVector> vls_scales = R_NilValue){
  Rcpp::XPtr<pedigree_terms> terms_ptr(ptr);
  std::vector<pedmod::pedigree_ll_term > &terms = terms_ptr->terms;
  n_threads = eval_get_n_threads(n_threads, *terms_ptr);

  parallelrng::set_rng_seeds(n_threads);

  // checks
  if(static_cast<size_t>(par.size()) != terms[0].n_par())
    throw std::invalid_argument(
        "eval_pedigree_ll: invalid par argument. Had " +
          std::to_string(par.size()) + " elements but should have " +
          std::to_string(terms[0].n_par()) + ".");

  if(maxvls < minvls or maxvls < 1)
    throw std::invalid_argument("mvndst: invalid maxvls");

  arma::vec const c_weights
    {check_n_get_cluster_weights(cluster_weights, terms.size())};
  bool const has_weights = c_weights.size() > 0;

  arma::vec const vls_scales_use
    {check_n_get_vls_scales(vls_scales, terms.size(), maxvls)};
  bool const has_vls_scales{vls_scales_use.size() > 0};

  // transform scale parameters
  auto const n_fix = terms[0].n_fix_effect();
  auto const n_scales = terms[0].n_scales();
  for(unsigned i = n_fix; i < n_fix + n_scales; ++i)
    par[i] = std::exp(par[i]);

  pedmod::cache_mem<double> r_mem;
  r_mem.set_n_mem(2, n_threads);

  // compute
  auto all_idx = get_indices(indices, *terms_ptr);
  int const * idx = &all_idx[0];

  int n_fails(0);
  openmp_exception_ptr exception_handler;
  pedmod::cdf_methods const meth = pedmod::get_cdf_methods(method);

  for(unsigned thread = 0; thread < n_threads; ++thread){
    double * wmem = r_mem.get_mem(thread);
    std::fill(wmem, wmem + 2, 0);
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
{
#endif
  double *wmem = r_mem.get_mem();

#ifdef _OPENMP
#pragma omp for schedule(static) reduction(+:n_fails)
#endif
  for(int i = 0; i < all_idx.size(); ++i)
    exception_handler.run([&]() -> void {
      if(idx[i] >= static_cast<int>(terms.size()))
        return;
      bool did_fail(false);
      double const w_i = has_weights ? c_weights[idx[i]] : 1;
      if(std::abs(w_i) < std::numeric_limits<double>::epsilon())
        return;

      int minvls_use{minvls};
      int maxvls_use{maxvls};
      if(has_vls_scales){
        if(minvls > 0)
          minvls_use = std::max<int>(1, std::lround(minvls * vls_scales_use[i]));
        maxvls_use = std::lround(maxvls * vls_scales_use[i]);
      }

      auto const res = terms.at(idx[i]).fn(
        &par[0], maxvls_use, abs_eps, rel_eps, minvls_use, do_reorder, use_aprx,
        did_fail, meth, use_tilting);

      wmem[0] += w_i * res.log_likelihood;
      wmem[1] += w_i * w_i * res.estimator_var;
      n_fails += did_fail;
    });
#ifdef _OPENMP
}
#endif

  exception_handler.rethrow_if_error();

  double out(0.),
     var_est(0.);
  for(unsigned i = 0; i < n_threads; ++i){
    out     += r_mem.get_mem(i)[0];
    var_est += r_mem.get_mem(i)[1];
  }

  Rcpp::NumericVector v_out = Rcpp::NumericVector::create(out);
  v_out.attr("n_fails") = Rcpp::IntegerVector::create(n_fails);
  v_out.attr("std"    ) = Rcpp::NumericVector::create(std::sqrt(var_est));
  return v_out;
}


// [[Rcpp::export("eval_pedigree_grad_cpp")]]
Rcpp::NumericVector eval_pedigree_grad
  (SEXP ptr, arma::vec par, int const maxvls,
   double const abs_eps, double const rel_eps,
   Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue,
   int const minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, unsigned n_threads = 1L,
   Rcpp::Nullable<Rcpp::NumericVector> cluster_weights = R_NilValue,
   int const method = 0, bool const use_tilting = false,
   Rcpp::Nullable<Rcpp::NumericVector> vls_scales = R_NilValue){
  Rcpp::XPtr<pedigree_terms> terms_ptr(ptr);
  std::vector<pedmod::pedigree_ll_term > &terms = terms_ptr->terms;
  n_threads = eval_get_n_threads(n_threads, *terms_ptr);

  parallelrng::set_rng_seeds(n_threads);

  // checks
  if(static_cast<size_t>(par.size()) != terms[0].n_par())
    throw std::invalid_argument(
        "eval_pedigree_grad: invalid par argument. Had " +
          std::to_string(par.size()) + " elements but should have " +
          std::to_string(terms[0].n_par()) + ".");

  arma::vec const c_weights
    {check_n_get_cluster_weights(cluster_weights, terms.size())};
  bool const has_weights = c_weights.size() > 0;

  arma::vec const vls_scales_use
    {check_n_get_vls_scales(vls_scales, terms.size(), maxvls)};
  bool const has_vls_scales{vls_scales_use.size() > 0};

  // transform scale parameters
  auto const n_fix = terms[0].n_fix_effect();
  for(unsigned i = n_fix; i < par.size(); ++i)
    par[i] = std::exp(par[i]);

  pedmod::cache_mem<double> r_mem;
  r_mem.set_n_mem(2 * (1 + par.size()), n_threads);

  // compute
  auto all_idx = get_indices(indices, *terms_ptr);
  int const * idx = &all_idx[0];
  int n_fails(0);

  openmp_exception_ptr exception_handler;
  pedmod::cdf_methods const meth = pedmod::get_cdf_methods(method);

  for(unsigned thread = 0; thread < n_threads; ++thread){
    double * wmem = r_mem.get_mem(thread);
    std::fill(wmem, wmem + 2 * (1 + par.size()), 0);
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
{
#endif
  double * wmem    = r_mem.get_mem(),
         * var_est = wmem + 1 + par.size();

#ifdef _OPENMP
#pragma omp for schedule(static) reduction(+:n_fails)
#endif
  for(int i = 0; i < all_idx.size(); ++i)
    exception_handler.run([&]() -> void {
      if(idx[i] >= static_cast<int>(terms.size()))
        return;
      bool did_fail(false);
      double const w_i = has_weights ? c_weights[idx[i]] : 1;
      if(std::abs(w_i) < std::numeric_limits<double>::epsilon())
        return;

      int minvls_use{minvls};
      int maxvls_use{maxvls};
      if(has_vls_scales){
        if(minvls > 0)
          minvls_use = std::max<int>(1, std::lround(minvls * vls_scales_use[i]));
        maxvls_use = std::lround(maxvls * vls_scales_use[i]);
      }

      *wmem += terms.at(idx[i]).gr(
        &par[0], wmem + 1, var_est, maxvls_use, abs_eps, rel_eps, minvls_use,
        do_reorder, use_aprx, did_fail, w_i, meth, use_tilting);
      n_fails += did_fail;
    });
#ifdef _OPENMP
}
#endif

  exception_handler.rethrow_if_error();

  // aggregate the result
  auto const n_par = terms[0].n_par();
  Rcpp::NumericVector grad(n_par),
                   std_est(n_par + 1);
  double ll(0.);
  for(unsigned i = 0; i < n_threads; ++i){
    double *wmem = r_mem.get_mem(i);
    ll += *wmem;
    for(unsigned j = 0; j < par.size(); ++j){
      grad   [j] += wmem[j + 1];
      std_est[j] += wmem[j + 1 + par.size()];
    }
    std_est[par.size()] += wmem[par.size() + 1 + par.size()];
  }

  for(unsigned j = 0; j < par.size() + 1; ++j)
    std_est[j] = std::sqrt(std_est[j]);
  for(unsigned j = 1 + n_fix; j < par.size() + 1; ++j)
    std_est[j] *= par[j - 1];

  // account for exp(...)
  for(unsigned i = n_fix; i < n_par; ++i)
    grad[i] *= par[i];
  grad.attr("logLik")  = Rcpp::NumericVector::create(ll);
  grad.attr("n_fails") = Rcpp::IntegerVector::create(n_fails);
  grad.attr("std")     = std_est;

  return grad;
}

// [[Rcpp::export("eval_pedigree_hess_cpp")]]
Rcpp::NumericMatrix eval_pedigree_hess
  (SEXP ptr, arma::vec par, int const maxvls,
   double const abs_eps, double const rel_eps,
   Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue,
   int const minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, unsigned n_threads = 1L,
   Rcpp::Nullable<Rcpp::NumericVector> cluster_weights = R_NilValue,
   int const method = 0, bool const use_tilting = false,
   Rcpp::Nullable<Rcpp::NumericVector> vls_scales = R_NilValue){
  Rcpp::XPtr<pedigree_terms> terms_ptr(ptr);
  std::vector<pedmod::pedigree_ll_term > &terms = terms_ptr->terms;
  n_threads = eval_get_n_threads(n_threads, *terms_ptr);

  parallelrng::set_rng_seeds(n_threads);

  // checks
  auto const n_par = terms[0].n_par();
  if(static_cast<size_t>(par.size()) != n_par)
    throw std::invalid_argument(
        "eval_pedigree_hess: invalid par argument. Had " +
          std::to_string(par.size()) + " elements but should have " +
          std::to_string(n_par) + ".");

  arma::vec const c_weights
    {check_n_get_cluster_weights(cluster_weights, terms.size())};
  bool const has_weights = c_weights.size() > 0;

  arma::vec const vls_scales_use
    {check_n_get_vls_scales(vls_scales, terms.size(), maxvls)};
  bool const has_vls_scales{vls_scales_use.size() > 0};

  // transform scale parameters
  auto const n_fix = terms[0].n_fix_effect();
  for(unsigned i = n_fix; i < n_par; ++i)
    par[i] = std::exp(par[i]);

  pedmod::cache_mem<double> r_mem;
  auto const dim_out = 1 + n_par * (1 + n_par);
  r_mem.set_n_mem(2 * dim_out, n_threads);

  // compute
  auto all_idx = get_indices(indices, *terms_ptr);
  int const * idx = &all_idx[0];
  int n_fails(0);

  openmp_exception_ptr exception_handler;
  pedmod::cdf_methods const meth = pedmod::get_cdf_methods(method);

  for(unsigned thread = 0; thread < n_threads; ++thread){
    double * wmem = r_mem.get_mem(thread);
    std::fill(wmem, wmem + 2 * dim_out, 0);
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
{
#endif
  double * wmem    = r_mem.get_mem(),
         * var_est = wmem + dim_out;

#ifdef _OPENMP
#pragma omp for schedule(static) reduction(+:n_fails)
#endif
  for(int i = 0; i < all_idx.size(); ++i)
    exception_handler.run([&]() -> void {
      if(idx[i] >= static_cast<int>(terms.size()))
        return;
      bool did_fail(false);
      double const w_i = has_weights ? c_weights[idx[i]] : 1;
      if(std::abs(w_i) < std::numeric_limits<double>::epsilon())
        return;

      int minvls_use{minvls};
      int maxvls_use{maxvls};
      if(has_vls_scales){
        if(minvls > 0)
          minvls_use = std::max<int>(1, std::lround(minvls * vls_scales_use[i]));
        maxvls_use = std::lround(maxvls * vls_scales_use[i]);
      }

      *wmem += terms.at(idx[i]).hessian(
        &par[0], wmem + 1,  wmem + 1 + n_par, var_est, maxvls_use, abs_eps,
        rel_eps, minvls_use, do_reorder, use_aprx, did_fail, w_i, meth,
        use_tilting);
      n_fails += did_fail;
    });
#ifdef _OPENMP
}
#endif

  exception_handler.rethrow_if_error();

  // aggregate the result
  Rcpp::NumericVector grad(n_par),
                   std_est(dim_out);
  Rcpp::NumericMatrix hess(n_par, n_par);

  double ll{};
  for(unsigned i = 0; i < n_threads; ++i){
    double *wmem = r_mem.get_mem(i);
    ll += *wmem;
    for(unsigned j = 0; j < n_par; ++j)
      grad(j) += wmem[j + 1];
    for(unsigned j = 0; j < n_par; ++j)
      for(unsigned k = 0; k < n_par; ++k)
        hess(k, j) += wmem[1 + n_par + k + j * n_par];

    for(unsigned j = 0; j < dim_out; ++j)
      std_est(j) += wmem[j + dim_out];
  }

  for(unsigned j = 0; j < dim_out; ++j)
    std_est(j) = std::sqrt(std_est[j]);

  hess.attr("logLik")  = Rcpp::NumericVector::create(ll);
  hess.attr("grad") = grad;
  hess.attr("n_fails") = Rcpp::IntegerVector::create(n_fails);
  hess.attr("std")     = std_est;

  return hess;
}

//' @rdname pedigree_ll_terms
//'
//' @export
// [[Rcpp::export]]
SEXP pedigree_ll_terms_loadings
  (Rcpp::List data, unsigned const max_threads = 1,
   unsigned const n_sequences = 8){
  Rcpp::XPtr<pedigree_terms_loading> out(
    new pedigree_terms_loading(data, max_threads, n_sequences));

  out.attr("class") = "pedigree_ll_terms_loadings_ptr";
  return out;
}

// [[Rcpp::export("eval_pedigree_ll_loadings_cpp")]]
Rcpp::NumericVector eval_pedigree_ll_loadings
  (SEXP ptr, arma::vec par, int const maxvls,
   double const abs_eps, double const rel_eps,
   Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue,
   int const minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, unsigned n_threads = 1L,
   Rcpp::Nullable<Rcpp::NumericVector> cluster_weights = R_NilValue,
   int const method = 0, bool const use_tilting = false,
   Rcpp::Nullable<Rcpp::NumericVector> vls_scales = R_NilValue){
  Rcpp::XPtr<pedigree_terms_loading> terms_ptr(ptr);
  std::vector<pedmod::pedigree_ll_term_loading> &terms = terms_ptr->terms;
  n_threads = eval_get_n_threads(n_threads, *terms_ptr);

  parallelrng::set_rng_seeds(n_threads);

  // checks
  if(static_cast<size_t>(par.size()) != terms[0].n_par())
    throw std::invalid_argument(
        "eval_pedigree_ll_loadings: invalid par argument. Had " +
          std::to_string(par.size()) + " elements but should have " +
          std::to_string(terms[0].n_par()) + ".");

  if(maxvls < minvls or maxvls < 1)
    throw std::invalid_argument("mvndst: invalid maxvls");

  arma::vec const c_weights
    {check_n_get_cluster_weights(cluster_weights, terms.size())};
  bool const has_weights = c_weights.size() > 0;

  arma::vec const vls_scales_use
    {check_n_get_vls_scales(vls_scales, terms.size(), maxvls)};
  bool const has_vls_scales{vls_scales_use.size() > 0};

  pedmod::cache_mem<double> r_mem;
  r_mem.set_n_mem(2, n_threads);

  // compute
  auto all_idx = get_indices(indices, *terms_ptr);
  int const * idx = &all_idx[0];

  int n_fails(0);
  openmp_exception_ptr exception_handler;
  pedmod::cdf_methods const meth = pedmod::get_cdf_methods(method);

  for(unsigned thread = 0; thread < n_threads; ++thread){
    double * wmem = r_mem.get_mem(thread);
    std::fill(wmem, wmem + 2, 0);
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
{
#endif
  double *wmem = r_mem.get_mem();

#ifdef _OPENMP
#pragma omp for schedule(static) reduction(+:n_fails)
#endif
  for(int i = 0; i < all_idx.size(); ++i)
    exception_handler.run([&]() -> void {
      if(idx[i] >= static_cast<int>(terms.size()))
        return;
      bool did_fail(false);
      double const w_i = has_weights ? c_weights[idx[i]] : 1;
      if(std::abs(w_i) < std::numeric_limits<double>::epsilon())
        return;

      int minvls_use{minvls};
      int maxvls_use{maxvls};
      if(has_vls_scales){
        if(minvls > 0)
          minvls_use = std::max<int>(1, std::lround(minvls * vls_scales_use[i]));
        maxvls_use = std::lround(maxvls * vls_scales_use[i]);
      }

      auto const res = terms.at(idx[i]).fn(
        &par[0], maxvls_use, abs_eps, rel_eps, minvls_use, do_reorder, use_aprx,
        did_fail, meth, true);

      wmem[0] += w_i * res.log_likelihood;
      wmem[1] += w_i * w_i * res.estimator_var;
      n_fails += did_fail;
    });
#ifdef _OPENMP
}
#endif

  exception_handler.rethrow_if_error();

  double out(0.),
     var_est(0.);
  for(unsigned i = 0; i < n_threads; ++i){
    out     += r_mem.get_mem(i)[0];
    var_est += r_mem.get_mem(i)[1];
  }

  Rcpp::NumericVector v_out = Rcpp::NumericVector::create(out);
  v_out.attr("n_fails") = Rcpp::IntegerVector::create(n_fails);
  v_out.attr("std"    ) = Rcpp::NumericVector::create(std::sqrt(var_est));
  return v_out;
}

// [[Rcpp::export("eval_pedigree_grad_loadings_cpp")]]
Rcpp::NumericVector eval_pedigree_grad_loadings
  (SEXP ptr, arma::vec par, int const maxvls,
   double const abs_eps, double const rel_eps,
   Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue,
   int const minvls = -1, bool const do_reorder = true,
   bool const use_aprx = false, unsigned n_threads = 1L,
   Rcpp::Nullable<Rcpp::NumericVector> cluster_weights = R_NilValue,
   int const method = 0, bool const use_tilting = false,
   Rcpp::Nullable<Rcpp::NumericVector> vls_scales = R_NilValue){
  Rcpp::XPtr<pedigree_terms_loading> terms_ptr(ptr);
  std::vector<pedmod::pedigree_ll_term_loading> &terms = terms_ptr->terms;
  n_threads = eval_get_n_threads(n_threads, *terms_ptr);

  parallelrng::set_rng_seeds(n_threads);

  // checks
  if(static_cast<size_t>(par.size()) != terms[0].n_par())
    throw std::invalid_argument(
        "eval_pedigree_ll_loadings: invalid par argument. Had " +
          std::to_string(par.size()) + " elements but should have " +
          std::to_string(terms[0].n_par()) + ".");

  if(maxvls < minvls or maxvls < 1)
    throw std::invalid_argument("mvndst: invalid maxvls");

  arma::vec const c_weights
    {check_n_get_cluster_weights(cluster_weights, terms.size())};
  bool const has_weights = c_weights.size() > 0;

  arma::vec const vls_scales_use
    {check_n_get_vls_scales(vls_scales, terms.size(), maxvls)};
  bool const has_vls_scales{vls_scales_use.size() > 0};

  pedmod::cache_mem<double> r_mem;
  r_mem.set_n_mem(2 * (1 + par.size()), n_threads);

  // compute
  auto all_idx = get_indices(indices, *terms_ptr);
  int const * idx = &all_idx[0];

  int n_fails(0);
  openmp_exception_ptr exception_handler;
  pedmod::cdf_methods const meth = pedmod::get_cdf_methods(method);

  for(unsigned thread = 0; thread < n_threads; ++thread){
    double * wmem = r_mem.get_mem(thread);
    std::fill(wmem, wmem + 2 * (1 + par.size()), 0);
  }

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
{
#endif
  double * wmem    = r_mem.get_mem(),
         * var_est = wmem + 1 + par.size();

#ifdef _OPENMP
#pragma omp for schedule(static) reduction(+:n_fails)
#endif
  for(int i = 0; i < all_idx.size(); ++i)
    exception_handler.run([&]() -> void {
      if(idx[i] >= static_cast<int>(terms.size()))
        return;
      bool did_fail(false);
      double const w_i = has_weights ? c_weights[idx[i]] : 1;
      if(std::abs(w_i) < std::numeric_limits<double>::epsilon())
        return;

      int minvls_use{minvls};
      int maxvls_use{maxvls};
      if(has_vls_scales){
        if(minvls > 0)
          minvls_use = std::max<int>(1, std::lround(minvls * vls_scales_use[i]));
        maxvls_use = std::lround(maxvls * vls_scales_use[i]);
      }

      *wmem += terms.at(idx[i]).gr(
        &par[0], wmem + 1, var_est, maxvls_use, abs_eps, rel_eps, minvls_use,
        do_reorder, use_aprx, did_fail, w_i, meth, use_tilting);
      n_fails += did_fail;
    });
#ifdef _OPENMP
}
#endif

  exception_handler.rethrow_if_error();

  // aggregate the result
  auto const n_par = terms[0].n_par();
  Rcpp::NumericVector grad(n_par),
                   std_est(n_par + 1);
  double ll(0.);
  for(unsigned i = 0; i < n_threads; ++i){
    double *wmem = r_mem.get_mem(i);
    ll += *wmem;
    for(unsigned j = 0; j < par.size(); ++j){
      grad   [j] += wmem[j + 1];
      std_est[j] += wmem[j + 1 + par.size()];
    }
    std_est[par.size()] += wmem[par.size() + 1 + par.size()];
  }

  for(unsigned j = 0; j < par.size() + 1; ++j)
    std_est[j] = std::sqrt(std_est[j]);

  grad.attr("logLik")  = Rcpp::NumericVector::create(ll);
  grad.attr("n_fails") = Rcpp::IntegerVector::create(n_fails);
  grad.attr("std")     = std_est;

  return grad;
}
