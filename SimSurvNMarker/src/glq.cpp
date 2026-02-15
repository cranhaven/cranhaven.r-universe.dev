// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

namespace {
constexpr bool const do_check = false;
}

/* Performs Gauss–Legendre quadrature */
// [[Rcpp::export(rng = false)]]
SEXP glq(
    SEXP lb, SEXP ub, SEXP nodes, SEXP weights, SEXP f, SEXP rho){
  R_len_t const n_out = Rf_length(lb),
              n_nodes = Rf_length(weights);
  if(do_check){
    if(TYPEOF(lb) != REALSXP or TYPEOF(ub) != REALSXP or
         TYPEOF(nodes) != REALSXP or TYPEOF(weights) != REALSXP)
      throw std::invalid_argument("some inputs are not numeric");
    if(Rf_length(ub) != n_out)
      throw std::invalid_argument("lb length != ub length");
    if(Rf_length(weights) != n_nodes)
      throw std::invalid_argument("nodes length != weights length");
    if(!Rf_isFunction(f))
      throw std::invalid_argument("f is not a function");
    if(!Rf_isEnvironment(rho))
      throw std::invalid_argument("rho is not an environment");
  }

  SEXP out     = PROTECT(Rf_allocVector(REALSXP, n_out)),
       R_fcall = PROTECT(Rf_lang2(f, R_NilValue)),
      node_arg = PROTECT(Rf_allocVector(REALSXP, n_nodes));

  double const * const nodes_start   = REAL(nodes),
               * const weights_start = REAL(weights),
               *       ubp = REAL(ub),
               *       lbp = REAL(lb);
  double * const arg_start = REAL(node_arg),
         *       o_i       = REAL(out);

  for(R_len_t i = 0; i < n_out; ++i, ++ubp, ++lbp, ++o_i){
    double const d1 = (*ubp - *lbp) / 2.,
                 d2 = (*ubp + *lbp) / 2.;

    *o_i = 0.;
    double * a = arg_start;
    double const * n = nodes_start;
    for(R_len_t j = 0; j < n_nodes; ++j)
      *a++ = d1 * *n++ + d2;
    SETCADR(R_fcall, node_arg);
    SEXP ans = Rf_eval(R_fcall, rho);
    if(do_check)
      if(TYPEOF(ans) != REALSXP or Rf_length(ans) != n_nodes)
        throw std::runtime_error("invalid output of f");

    double const * v = REAL(ans),
                 * w = weights_start;
    for(R_len_t j = 0; j < n_nodes; ++j)
      *o_i += *w++ * *v++;

    *o_i *= d1;
  }

  UNPROTECT(3);

  return out;
}

/*** R
# assign data for an example
gl_dat <- SimSurvNMarker::get_gl_rule(30L)
set.seed(1L)
n <- 100L
lb <- runif(100)
ub <- runif(100) + lb

f <- function(x)
  x * sin(x)

# assign R version
r_version_inner <- function(lb, ub, nodes, weights, f){
  nodes <- (ub - lb) / 2 * nodes + (ub + lb) / 2
  val <- f(nodes)
  (ub - lb) / 2 * drop(gl_dat$weight %*% val)
}
r_version_inner <- compiler::cmpfun(r_version_inner)

r_version <- function(lb, ub, nodes, weights, f){
  mapply(r_version_inner, lb = lb, ub = ub,
         MoreArgs = list(nodes = nodes, weights = weights, f = f),
         SIMPLIFY = TRUE)
}
r_version <- compiler::cmpfun(r_version)

# we get the same
all.equal(
  r_version_inner(lb = lb[1], ub = ub[1], nodes = gl_dat$node,
                    weights = gl_dat$weight, f = f),
  glq            (lb = lb[1], ub = ub[1], nodes = gl_dat$node,
                  weights = gl_dat$weight, f = f, environment()))
all.equal(
  r_version(lb = lb, ub = ub, nodes = gl_dat$node,
            weights = gl_dat$weight, f = f),
  glq      (lb = lb, ub = ub, nodes = gl_dat$node,
            weights = gl_dat$weight, f = f, environment()))

# the Cpp version is faster
library(microbenchmark)
microbenchmark(
  R   = r_version_inner(lb = lb[1], ub = ub[1], nodes = gl_dat$node,
                        weights = gl_dat$weight, f = f),
  cpp = glq            (lb = lb[1], ub = ub[1], nodes = gl_dat$node,
                        weights = gl_dat$weight, f = f, environment()))

microbenchmark(
  R   = r_version(lb = lb, ub = ub, nodes = gl_dat$node,
                  weights = gl_dat$weight, f = f),
  cpp = glq      (lb = lb, ub = ub, nodes = gl_dat$node,
                  weights = gl_dat$weight, f = f, environment()))
*/
