.not_supported_for <- function(x)
  stop(sprintf("'%s' does not does inherit from an implemented type",
               deparse(substitute(x))))

.is_implemented_ptr <- function(x)
  inherits(x, c("pedigree_ll_terms_ptr", "pedigree_ll_terms_loadings_ptr"))

.warn_on_standardized <- function(standardized)
  if(standardized)
    warning("standardized makes no difference yet and is not accounted for")

#' @rdname eval_pedigree
#'
#' @title Approximate the Log Marginal Likelihood
#' @description
#' Approximate the log marginal likelihood and the derivatives with
#' respect to the model parameters.
#'
#' @param maxvls maximum number of samples in the approximation for each
#' marginal likelihood term.
#' @param minvls minimum number of samples for each
#' marginal likelihood term. Negative values provides a
#' default which depends on the dimension of the integration.
#' @param ptr object from \code{\link{pedigree_ll_terms}} or
#' \code{\link{pedigree_ll_terms_loadings}}.
#' @param par numeric vector with parameters. For an object from
#' \code{\link{pedigree_ll_terms}} these are the fixed effect coefficients and
#' log scale parameters. The log scale parameters should be last. For an object
#' from \code{\link{pedigree_ll_terms_loadings}} these are the fixed effects
#' and the coefficients for scale parameters.
#' @param indices zero-based vector with indices of which log marginal
#' likelihood terms to include. Use \code{NULL} if all indices should be
#' used.
#' @param n_threads number of threads to use.
#' @param cluster_weights numeric vector with weights for each cluster. Use
#' \code{NULL} if all clusters have weight one.
#' @param standardized logical for whether to use the standardized or direct
#' parameterization. See \code{\link{standardized_to_direct}} and the vignette
#' at \code{vignette("pedmod", package = "pedmod")}.
#' @param vls_scales can be a numeric vector with a positive scalar for each
#' cluster. Then \code{vls_scales[i] * minvls} and
#' \code{vls_scales[i] * maxvls} is used for cluster \code{i} rather than
#' \code{minvls} and \code{maxvls}. Set \code{vls_scales = NULL} if the latter
#' should be used.
#'
#' @inheritParams pedigree_ll_terms
#' @inheritParams mvndst
#'
#' @details
#' \code{eval_pedigree_hess} is only implemented for objects from
#' \code{\link{pedigree_ll_terms}}.
#'
#' @return \code{eval_pedigree_ll}:
#' a scalar with the log marginal likelihood approximation.
#' It has an attribute called \code{"n_fails"} which shows the number of
#' log marginal likelihood term approximations which do not satisfy
#' the \code{abs_eps} and \code{rel_eps} criteria and an attribute called
#' \code{std} with a standard error estimate based on the delta rule.
#'
#' @examples
#' # three families as an example
#' fam_dat <- list(
#'   list(
#'     y = c(FALSE, TRUE, FALSE, FALSE),
#'     X = structure(c(
#'       1, 1, 1, 1, 1.2922654151273, 0.358134905909256, -0.734963997107464,
#'       0.855235473516044, -1.16189500386223, -0.387298334620742,
#'       0.387298334620742, 1.16189500386223),
#'       .Dim = 4:3, .Dimnames = list( NULL, c("(Intercept)", "X1", ""))),
#'     rel_mat = structure(c(
#'       1, 0.5, 0.5, 0.125, 0.5, 1, 0.5, 0.125, 0.5, 0.5,
#'       1, 0.125, 0.125, 0.125, 0.125, 1), .Dim = c(4L, 4L)),
#'     met_mat = structure(c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1),
#'                         .Dim = c(4L, 4L))),
#'   list(
#'     y = c(FALSE, FALSE, FALSE),
#'     X = structure(c(
#'       1, 1, 1, -0.0388728997202442, -0.0913782435233639,
#'       -0.0801619722392612, -1, 0, 1), .Dim = c(3L, 3L)),
#'     rel_mat = structure(c(
#'       1, 0.5, 0.125, 0.5, 1, 0.125, 0.125, 0.125, 1), .Dim = c(3L, 3L)),
#'     met_mat = structure(c(
#'       1, 1, 0, 1, 1, 0, 0, 0, 1), .Dim = c(3L, 3L))),
#'   list(
#'     y = c(TRUE, FALSE),
#'     X = structure(c(
#'       1, 1, 0.305275750370738, -1.49482995913648,  -0.707106781186547,
#'       0.707106781186547),
#'       .Dim = 2:3, .Dimnames = list( NULL, c("(Intercept)", "X1", ""))),
#'     rel_mat = structure(c(1, 0.5,  0.5, 1), .Dim = c(2L, 2L)),
#'     met_mat = structure(c(1, 1, 1, 1), .Dim = c(2L,  2L))))
#'
#' # get the data into the format needed for the package
#' dat_arg <- lapply(fam_dat, function(x){
#'   # we need the following for each family:
#'   #   y: the zero-one outcomes.
#'   #   X: the design matrix for the fixed effects.
#'   #   scale_mats: list with the scale matrices for each type of effect.
#'   list(y = as.numeric(x$y), X = x$X,
#'        scale_mats = list(x$rel_mat, x$met_mat))
#' })
#'
#' # get a pointer to the C++ object
#' ptr <- pedigree_ll_terms(dat_arg, max_threads = 1L)
#'
#' # approximate the log marginal likelihood
#' beta <- c(-1, 0.3, 0.2) # fixed effect coefficients
#' scs <- c(0.5, 0.33)     # scales parameters
#'
#' set.seed(44492929)
#' system.time(ll1 <- eval_pedigree_ll(
#'   ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e5,
#'   rel_eps = 1e-5, minvls = 2000, use_aprx = FALSE))
#' ll1 # the approximation
#'
#' # with the approximation of pnorm and qnorm
#' system.time(ll2 <- eval_pedigree_ll(
#'   ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e5,
#'   rel_eps = 1e-5, minvls = 2000, use_aprx = TRUE))
#' all.equal(ll1, ll2, tolerance = 1e-5)
#'
#' # cluster weights can be used as follows to repeat the second family three
#' # times and remove the third
#' system.time(deriv_w_weight <- eval_pedigree_grad(
#'   ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE,
#'   cluster_weights = c(1, 3, 0)))
#'
#' # the same as manually repeating second cluster and not including the third
#' dum_dat <- dat_arg[c(1, 2, 2, 2)]
#' dum_ptr <- pedigree_ll_terms(dum_dat, 1L)
#' system.time(deriv_dum <- eval_pedigree_grad(
#'   ptr = dum_ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE))
#' all.equal(deriv_dum, deriv_w_weight, tolerance = 1e-3)
#'
#' # the hessian is computed on the scale parameter scale rather than on the
#' # log of the scale parameters
#' system.time(hess_w_weight <- eval_pedigree_hess(
#'   ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE,
#'   cluster_weights = c(1, 3, 0)))
#'
#' system.time(hess_dum <- eval_pedigree_hess(
#'   ptr = dum_ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE))
#' attr(hess_w_weight, "n_fails") <- attr(hess_dum, "n_fails") <- NULL
#' all.equal(hess_w_weight, hess_dum, tolerance = 1e-3)
#'
#' # the results are consistent with the gradient output
#' all.equal(attr(deriv_dum, "logLik"), attr(hess_dum, "logLik"),
#'           tolerance = 1e-5)
#'
#' hess_grad <- attr(hess_dum, "grad")
#' all.equal(hess_grad, deriv_dum, check.attributes = FALSE,
#'           tolerance = 1e-3)
#'
#' # with loadings
#' dat_arg_loadings <- lapply(fam_dat, function(x){
#'   list(y = as.numeric(x$y), X = x$X, Z = x$X[, 1:2],
#'        scale_mats = list(x$rel_mat, x$met_mat))
#' })
#'
#' ptr_loadings <-
#'   pedigree_ll_terms_loadings(dat_arg_loadings, max_threads = 1L)
#'
#' scs <- c(log(0.5) / 2, 0.1, log(0.33) / 2, 0.2) # got more scales parameters
#' eval_pedigree_ll(
#'   ptr = ptr_loadings, par = c(beta, scs), abs_eps = -1, maxvls = 1e4,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE)
#' eval_pedigree_grad(
#'   ptr = ptr_loadings, par = c(beta, scs), abs_eps = -1, maxvls = 1e4,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE)
#'
#' # can recover the result from before
#' scs <- c(log(0.5) / 2, 0, log(0.33) / 2, 0)
#' ll3 <- eval_pedigree_ll(
#'   ptr = ptr_loadings, par = c(beta, scs), abs_eps = -1, maxvls = 1e4,
#'   rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE)
#' all.equal(ll1, ll3, tolerance = 1e-5)
#'
#' @export
eval_pedigree_ll <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices = NULL, minvls = -1L,
  do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L, cluster_weights = NULL,
  standardized = FALSE, method = 0L, use_tilting = FALSE, vls_scales = NULL){
  fun <- if(inherits(ptr, "pedigree_ll_terms_ptr")){
    .eval_pedigree_ll
  } else if(inherits(ptr, "pedigree_ll_terms_loadings_ptr")){
    .eval_pedigree_ll_loadings
  } else
    .not_supported_for(ptr)

  fun(ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps,
      rel_eps = rel_eps, indices = indices, minvls = minvls,
      do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
      cluster_weights = cluster_weights, standardized = standardized,
      method = method, use_tilting = use_tilting, vls_scales = vls_scales)
}

.eval_pedigree_ll <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices, minvls, do_reorder,
  use_aprx, n_threads, cluster_weights, standardized, method, use_tilting,
  vls_scales){
  if(standardized)
    par <- standardized_to_direct(par = par, n_scales = get_n_scales(ptr))

  eval_pedigree_ll_cpp(
    ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps, rel_eps = rel_eps,
    indices = indices, minvls = minvls, do_reorder = do_reorder,
    use_aprx = use_aprx, n_threads = n_threads, use_tilting = use_tilting,
    cluster_weights = cluster_weights, method = method, vls_scales = vls_scales)
}

.eval_pedigree_ll_loadings <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices, minvls, do_reorder,
  use_aprx, n_threads, cluster_weights, standardized, method, use_tilting,
  vls_scales){
  .warn_on_standardized(standardized)

  eval_pedigree_ll_loadings_cpp(
    ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps, rel_eps = rel_eps,
    indices = indices, minvls = minvls, do_reorder = do_reorder,
    use_aprx = use_aprx, n_threads = n_threads, use_tilting = use_tilting,
    cluster_weights = cluster_weights, method = method, vls_scales = vls_scales)
}

#' @rdname eval_pedigree
#'
#' @return \code{eval_pedigree_grad}: a vector with the derivatives with
#' respect to \code{par}. An attribute called \code{"logLik"} contains the
#' log marginal likelihood approximation. There will also be \code{"n_fails"}
#' attribute like for \code{eval_pedigree_ll} and an attribute called
#' \code{"std"} which first element is the standard error estimate of the
#' log likelihood based on the delta method and the last elements are the
#' standard error estimates of the gradient. The latter ignores the Monte Carlo
#' error from the likelihood approximation.
#'
#' @export
eval_pedigree_grad <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices = NULL, minvls = -1L,
  do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L, cluster_weights = NULL,
  standardized = FALSE, method = 0L, use_tilting = FALSE, vls_scales = NULL){
  fun <- if(inherits(ptr, "pedigree_ll_terms_ptr")){
    .eval_pedigree_grad
  } else if(inherits(ptr, "pedigree_ll_terms_loadings_ptr")){
    .eval_pedigree_grad_loadings
  } else
    .not_supported_for(ptr)

  fun(ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps,
      rel_eps = rel_eps, indices = indices, minvls = minvls,
      do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
      cluster_weights = cluster_weights, standardized = standardized,
      method = method, use_tilting = use_tilting, vls_scales = vls_scales)
}

.eval_pedigree_grad <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices, minvls, do_reorder,
  use_aprx, n_threads, cluster_weights, standardized, method, use_tilting,
  vls_scales){
  if(standardized)
    par <- standardized_to_direct(par = par, n_scales = get_n_scales(ptr),
                                  jacobian = TRUE)

  out <- eval_pedigree_grad_cpp(
    ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps, rel_eps = rel_eps,
    indices = indices, minvls = minvls, do_reorder = do_reorder,
    use_aprx = use_aprx, n_threads = n_threads, use_tilting = use_tilting,
    cluster_weights = cluster_weights, method = method, vls_scales = vls_scales)

  if(!standardized)
    return(out)

  res <- drop(out %*% attr(par, "jacobian"))
  attributes(res) <- attributes(out)
  res
}

.eval_pedigree_grad_loadings <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices, minvls, do_reorder,
  use_aprx, n_threads, cluster_weights, standardized, method,
  use_tilting, vls_scales){
  .warn_on_standardized(standardized)

  eval_pedigree_grad_loadings_cpp(
    ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps, rel_eps = rel_eps,
    indices = indices, minvls = minvls, do_reorder = do_reorder,
    use_aprx = use_aprx, n_threads = n_threads, use_tilting = use_tilting,
    cluster_weights = cluster_weights, method = method, vls_scales = vls_scales)
}


#' @rdname eval_pedigree
#'
#' @return \code{eval_pedigree_hess}: a matrix with the hessian with
#' respect to \code{par}.
#' An attribute called \code{"logLik"} contains the
#' log marginal likelihood approximation and an attribute called \code{"grad"}
#' contains the gradient·
#' The attribute \code{"hess_org"} contains the Hessian with the scale
#' parameter on the identity scale rather than the log scale.
#' \code{"vcov"} and \code{"vcov_org"} are
#' the covariance matrices from the hessian and \code{"hess_org"}.
#'
#' @export
eval_pedigree_hess <- function(
  ptr, par, maxvls, abs_eps, rel_eps, indices = NULL, minvls = -1L,
  do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L, cluster_weights = NULL,
  standardized = FALSE, method = 0L, use_tilting = FALSE, vls_scales = NULL){
  stopifnot(inherits(ptr, "pedigree_ll_terms_ptr"))
  .warn_on_standardized(standardized)

  hess_org <- eval_pedigree_hess_cpp(
    ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps,
    rel_eps = rel_eps, indices = indices, minvls = minvls,
    do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
    cluster_weights = cluster_weights, method = method,
    use_tilting = use_tilting, vls_scales = vls_scales)

  # compute the Hessian on the log scale of the scale parameters
  gr <- attr(hess_org, "grad")

  n_scales <- .get_n_scales(ptr)
  n_par <- length(par)
  jac <- rep(1, n_par)
  idx_scale <- seq_len(n_scales) + n_par - n_scales
  scales <- exp(tail(par, n_scales))
  jac[idx_scale] <- scales
  jac <- diag(jac)

  # TODO: this can be done way smarter
  hess_inner <- matrix(0, n_par * n_par, n_par)
  for(i in seq_along(idx_scale)){
    idx <- idx_scale[i]
    hess_inner[(idx - 1L) * n_par + idx, idx] <- scales[i]
  }

  hess <- jac %*% hess_org %*% jac + (t(gr) %x% diag(n_par)) %*% hess_inner

  attributes(hess) <- attributes(hess_org)
  attributes(hess_org) <- NULL
  dim(hess_org) <- dim(hess)
  attr(hess, "hess_org") <- hess_org

  try_solve <- function(x)
    try(solve(-x))

  attr(hess, "vcov") <- try_solve(hess)
  attr(hess, "vcov_org") <- try_solve(hess_org)
  attr(hess, "grad") <- drop(jac %*% gr)

  hess
}
