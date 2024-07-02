get_n_scales <- function(ptr){
  if(inherits(ptr, "pedigree_ll_terms_ptr")){
    .get_n_scales(ptr)
  } else if(inherits(ptr, "pedigree_ll_terms_loadings_ptr")){
    .get_n_scales_loadings(ptr)
  } else
    .not_supported_for(ptr)
}

get_n_terms <- function(ptr){
  if(inherits(ptr, "pedigree_ll_terms_ptr")){
    .get_n_terms(ptr)
  } else if(inherits(ptr, "pedigree_ll_terms_loadings_ptr")){
    .get_n_terms_loadings(ptr)
  } else
    .not_supported_for(ptr)
}

#' Optimize the Log Marginal Likelihood
#'
#' Optimizes \code{\link{eval_pedigree_ll}} and \code{\link{eval_pedigree_grad}}
#' using a passed optimization function.
#'
#' \code{pedmod_start} and \code{pedmod_start_loadings}
#' yield starting values which can be used for
#' \code{pedmod_opt}. The methods are based on a heuristics.
#'
#' @inheritParams eval_pedigree_ll
#' @param par starting values passed to \code{opt_func}.
#' @param opt_func function to perform minimization with arguments like
#' \code{\link{optim}}. BFGS is used with \code{\link{optim}} if this argument
#' is \code{NULL}.
#' @param seed seed to pass to \code{\link{set.seed}} before each gradient and
#' function evaluation. Use \code{NULL} if the seed should not be fixed.
#' @param fix integer vector with indices of \code{par} to fix. This is useful
#' for computing profile likelihoods. \code{NULL} yields all parameters.
#' @param ... Arguments passed to \code{opt_func}.
#' @param abs_eps absolute convergence threshold for
#' \code{\link{eval_pedigree_ll}} and \code{\link{eval_pedigree_grad}}.
#' @param rel_eps rel_eps convergence threshold for
#' \code{\link{eval_pedigree_ll}} and \code{\link{eval_pedigree_grad}}.
#'
#' @seealso
#' \code{\link{pedmod_sqn}}.
#'
#' @return
#' \code{pedmod_opt}: The output from the \code{opt_func} argument. Thus, if
#' \code{fix} is supplied then this is optimal values of only \code{par[-fix]}
#' with
#' \code{par[fix]} being fixed to the inputs. Thus, the length is only the
#' number of non-fixed parameters.
#'
#' @importFrom stats optim
#'
#' @examples
#' \donttest{
#' # we simulate outcomes with an additive genetic effect. The kinship matrix is
#' # the same for all families and given by
#' K <- matrix(c(
#'   0.5  , 0    , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0    , 0.5  , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0.25 , 0.25 , 0.5  , 0   , 0.25 , 0   , 0.25  , 0.25  , 0.125 , 0.125 ,
#'   0    , 0    , 0    , 0.5 , 0    , 0   , 0.25  , 0.25  , 0     , 0     ,
#'   0.25 , 0.25 , 0.25 , 0   , 0.5  , 0   , 0.125 , 0.125 , 0.25  , 0.25  ,
#'   0    , 0    , 0    , 0   , 0    , 0.5 , 0     , 0     , 0.25  , 0.25  ,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.5   , 0.25  , 0.0625, 0.0625,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.25  , 0.5   , 0.0625, 0.0625,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.5   , 0.25  ,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.25  , 0.5
#' ), 10)
#'
#' # simulates a data set.
#' #
#' # Args:
#' #   n_fams: number of families.
#' #   beta: the fixed effect coefficients.
#' #   sig_sq: the scale parameter.
#' sim_dat <- function(n_fams, beta = c(-1, 1, 2), sig_sq = 3){
#'   # setup before the simulations
#'   Cmat <- 2 * K
#'   n_obs <- NROW(K)
#'   Sig <- diag(n_obs) + sig_sq * Cmat
#'   Sig_chol <- chol(Sig)
#'
#'   # simulate the data
#'   out <- replicate(
#'     n_fams, {
#'       # simulate covariates
#'       X <- cbind(`(Intercept)` = 1, Continuous = rnorm(n_obs),
#'                  Binary = runif(n_obs) > .5)
#'
#'       # assign the linear predictor + noise
#'       eta <- drop(X %*% beta) + drop(rnorm(n_obs) %*% Sig_chol)
#'
#'       # return the list in the format needed for the package
#'       list(y = as.numeric(eta > 0), X = X, scale_mats = list(Cmat))
#'     }, simplify = FALSE)
#'
#'   # add attributes with the true values and return
#'   attributes(out) <- list(beta = beta, sig_sq = sig_sq)
#'   out
#' }
#'
#' # simulate the data
#' set.seed(1)
#' dat <- sim_dat(100L)
#'
#' # fit the model
#' ptr <- pedigree_ll_terms(dat, max_threads = 1L)
#' start <- pedmod_start(ptr = ptr, data = dat, n_threads = 1L)
#' fit <- pedmod_opt(ptr = ptr, par = start$par, n_threads = 1L, use_aprx = TRUE,
#'                   maxvls = 5000L, minvls = 1000L, abs_eps = 0, rel_eps = 1e-3)
#' fit$par # the estimate
#' -fit$value # the log maximum likelihood
#' start$logLik_no_rng # the log maximum likelihood without the random effects
#' }
#' @export
pedmod_opt <- function(ptr, par, maxvls, abs_eps, rel_eps,
                       opt_func = NULL, seed = 1L, indices = NULL, minvls = -1L,
                       do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L,
                       cluster_weights = NULL, fix = NULL, standardized = FALSE,
                       method = 0L, use_tilting = FALSE, vls_scales = NULL,
                       ...){
  # checks
  stopifnot(!missing(ptr), !missing(par), !missing(maxvls), !missing(abs_eps),
            !missing(rel_eps),
            .is_implemented_ptr(ptr))

  # handle defaults
  if(is.null(opt_func)){
    opt_func <- optim
    formals(opt_func)$method <- "BFGS"
    formals(opt_func)$control <- list(fnscale = get_n_terms(ptr))
  }
  if(is.null(fix))
    fix <- integer()
  else
    fix <- unique(fix)

  # checks
  stopifnot(is.function(opt_func),
            is.integer(fix), all(fix %in% seq_along(par)),
            is.numeric(par))

  # assign objective and gradient function
  get_par <- function(x){
    if(length(fix) < 1L)
      return(x)
    out <- par
    out[-fix] <- x
    out
  }
  fn <- function(x){
    if(!is.null(seed))
      set.seed(seed)
    out <- try(-eval_pedigree_ll(
        ptr = ptr, par = get_par(x), maxvls = maxvls, rel_eps = rel_eps,
        indices = indices, minvls = minvls, abs_eps = abs_eps,
        do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
        cluster_weights = cluster_weights, standardized = standardized,
        method = method, use_tilting = use_tilting, vls_scales = vls_scales),
      silent = TRUE)
    if(inherits(out, "try-error"))
      return(NA_real_)
    out
  }
  gr <- function(x){
    if(!is.null(seed))
      set.seed(seed)
    out <-
      -eval_pedigree_grad(ptr = ptr, par = get_par(x), maxvls = maxvls,
                          rel_eps = rel_eps, indices = indices, minvls = minvls,
                          abs_eps = abs_eps, do_reorder = do_reorder,
                          use_aprx = use_aprx, n_threads = n_threads,
                          cluster_weights = cluster_weights,
                          standardized = standardized,
                          method = method, use_tilting = use_tilting,
                          vls_scales = vls_scales)
    if(length(fix) > 0)
      out <- out[-fix]
    out
  }

  # optimize and return
  opt_func(par = if(length(fix) > 0) par[-fix] else par, fn = fn, gr = gr, ...)
}

#' @rdname pedmod_opt
#'
#' @param data the \code{\link{list}} that was passed to
#' \code{\link{pedigree_ll_terms}} or \code{\link{pedigree_ll_terms_loadings}}.
#' @param scale_max the maximum value for the scale parameters. Sometimes, the
#' optimization method tends to find large scale parameters and get stuck.
#' Setting a maximum solves this.
#' @param sc_start starting value for the scale parameters. Use \code{NULL} if
#' you have no value to start with.
#'
#' @return
#' \code{pedmod_start}: A \code{list} with:
#' \itemize{
#'  \item par: the starting value.
#'  \item beta_no_rng: the fixed effects MLEs without random effects.
#'  \item logLik_no_rng: the log maximum likelihood without random effects.
#'  \item logLik_est: the likelihood at par.
#' }
#' @importFrom stats binomial glm.fit
#' @importFrom utils tail
#' @export
pedmod_start <- function(ptr, data, maxvls = 1000L, abs_eps = 0, rel_eps = 1e-2,
                         seed = 1L, indices = NULL, scale_max = 9,
                         minvls = 100L, do_reorder = TRUE, use_aprx = TRUE,
                         n_threads = 1L, cluster_weights = NULL,
                         standardized = FALSE, method = 0L,
                         sc_start = NULL, use_tilting = FALSE,
                         vls_scales = NULL){
  # checks
  stopifnot(is.numeric(scale_max), length(scale_max) == 1L,
            scale_max > 0,
            is.numeric(seed), length(seed) %in% 0:1)
  if(!inherits(ptr, "pedigree_ll_terms_ptr"))
    .not_supported_for(ptr)

  #####
  # get the starting values for the fixed effects
  y <- unlist(lapply(data, `[[`, "y"))
  X <- do.call(rbind, lapply(data, `[[`, "X"))
  if(!is.null(cluster_weights))
    w <- unlist(Map(
      rep, cluster_weights,
      times = sapply(data, function(x) length(x$y))))
  else
    w <- rep(1, length(y))

  # checks
  n <- length(y)
  stopifnot(
    length(y) > 0, is.numeric(y),
    NROW(X) == n,
    is.null(cluster_weights) || (
      length(cluster_weights) == length(data) &&
        is.numeric(cluster_weights)))

  # fit the model without random effects
  start_fit <-  glm.fit(X, y, weights = w, family = binomial("probit"))
  beta <- start_fit$coefficients
  logLik_no_rng <- -sum(start_fit$deviance) / 2

  # function to optimize the parameters
  do_opt <- function(sc, fn, gr){
    n_scales <- get_n_scales(ptr)
    optim(sc, fn, gr, upper = rep(log(scale_max), n_scales),
          method = "L-BFGS-B", control = list(
            lmm = 10L, maxit = 1000L, fnscale = get_n_terms(ptr)))
  }
  try_opt <- function(fn, gr){
    n_scales <- get_n_scales(ptr)
    stopifnot(n_scales > 0,
              is.null(sc_start) ||
                (length(sc_start) == n_scales && is.numeric(sc_start) &&
                   all(is.finite(sc_start) & sc_start > 0 &
                         sc_start <= scale_max)))

    check_ok <- function(opt)
      !inherits(opt, "try-error") && opt$convergence < 2

    has_starting_value <- !is.null(sc_start)
    if(has_starting_value){
      sc <- log(sc_start)
      opt <- try(do_opt(sc, fn = fn, gr = gr), silent = TRUE)
      is_ok <- check_ok(opt)
    }

    if(!has_starting_value || !is_ok)
      for(sc_sqrt in seq(.1, sqrt(scale_max), length.out = 5)){
        sc <- rep(log(sc_sqrt) * 2, n_scales)
        opt <- try(do_opt(sc, fn = fn, gr = gr), silent = TRUE)
        if(is_ok <- check_ok(opt))
          # found a good solution
          break
      }

    list(opt = opt, sc = sc, is_ok = is_ok)
  }


  if(standardized){
    fn <- function(sc){
      if(!is.null(seed))
        set.seed(seed)

      out <- try(-eval_pedigree_ll(
        ptr = ptr, par = c(beta, sc), maxvls = maxvls, abs_eps = abs_eps,
        rel_eps = rel_eps, minvls = minvls, use_aprx = use_aprx,
        n_threads = n_threads, cluster_weights = cluster_weights,
        indices = indices, do_reorder = do_reorder,
        standardized = standardized, method = method,
        use_tilting = use_tilting, vls_scales = vls_scales),
        silent = TRUE)
      if(inherits(out, "try-error"))
        return(NA_real_)

      out
    }
    gr <- function(sc){
      if(!is.null(seed))
        set.seed(seed)

      out <- -eval_pedigree_grad(
        ptr = ptr, par = c(beta, sc), maxvls = maxvls, abs_eps = abs_eps,
        rel_eps = rel_eps, minvls = minvls, use_aprx = use_aprx,
        n_threads = n_threads, cluster_weights = cluster_weights,
        indices = indices, do_reorder = do_reorder,
        standardized = standardized, method = method,
        use_tilting = use_tilting, vls_scales = vls_scales)

      tail(out, -length(beta))
    }

    # optimize
    res <- try_opt(fn = fn, gr = gr)

    if(!res$is_ok){
      # fall to find a good solution
      n_scales <- get_n_scales(ptr)
      sc <- rep(log(.01), n_scales)
      logLik_est <- -fn(sc)

    } else {
      sc <- res$opt$par
      logLik_est <- -res$opt$value
    }

    return(list(par = c(beta, sc), beta_no_rng = beta,
                logLik_no_rng = logLik_no_rng,
                logLik_est = logLik_est, opt = res$opt))
  }

  #####
  # optimize the model where beta is proportional to estimates without the
  # random effects
  fn <- function(sc){
    if(!is.null(seed))
      set.seed(seed)
    beta_scaled <- beta * sqrt(1 + sum(exp(sc)))

    out <- try(-eval_pedigree_ll(
      ptr = ptr, par = c(beta_scaled, sc), maxvls = maxvls, abs_eps = abs_eps,
      rel_eps = rel_eps, minvls = minvls, use_aprx = use_aprx,
      n_threads = n_threads, cluster_weights = cluster_weights,
      indices = indices, do_reorder = do_reorder, method = method,
      use_tilting = use_tilting, vls_scales = vls_scales),
      silent = TRUE)
    if(inherits(out, "try-error"))
      return(NA_real_)

    out
  }
  gr <- function(sc){
    if(!is.null(seed))
      set.seed(seed)
    fac <- sqrt(1 + sum(exp(sc)))
    beta_scaled <- beta * fac

    out <- -eval_pedigree_grad(
      ptr = ptr, par = c(beta_scaled, sc), maxvls = maxvls, abs_eps = abs_eps,
      rel_eps = rel_eps, minvls = minvls, use_aprx = use_aprx,
      n_threads = n_threads, cluster_weights = cluster_weights,
      indices = indices, do_reorder = do_reorder, method = method,
      use_tilting = use_tilting, vls_scales = vls_scales)

    sum_d_beta <- sum(beta * out[seq_along(beta)])
    sum_d_beta * exp(sc) / (2 * fac) + tail(out, -length(beta))
  }

  # optimize
  res <- try_opt(fn = fn, gr = gr)

  if(!res$is_ok){
    # fall to find a good solution
    n_scales <- get_n_scales(ptr)
    sc <- rep(log(.01), n_scales)
    beta_scaled <- beta * sqrt(1 + sum(exp(sc)))
    logLik_est <- -fn(sc)

  } else {
    sc <- res$opt$par
    beta_scaled <- beta * sqrt(1 + sum(exp(sc)))
    logLik_est <- -res$opt$value

  }

  list(par = c(beta_scaled, sc), beta_no_rng = beta,
       logLik_no_rng = logLik_no_rng,
       logLik_est = logLik_est, opt = res$opt)
}

#' @rdname pedmod_opt
#'
#' @param sc_start_invariant scale parameter(s) like sc_start. It is the value
#' that all individuals should have (i.e. not one that varies by individual).
#'
#' @return
#' \code{pedmod_start_loadings}: A \code{list} with:
#' \itemize{
#'  \item par: the starting value.
#'  \item beta_no_rng: the fixed effects MLEs without random effects.
#'  \item logLik_no_rng: the log maximum likelihood without random effects.
#' }
#'
#' @importFrom stats lm.fit
#' @export
pedmod_start_loadings <- function(
  ptr, data, indices = NULL, cluster_weights = NULL,
  sc_start_invariant = NULL){
  # checks
  if(!inherits(ptr, "pedigree_ll_terms_loadings_ptr"))
    .not_supported_for(ptr)

  #####
  # get the starting values for the fixed effects
  y <- unlist(lapply(data, `[[`, "y"))
  X <- do.call(rbind, lapply(data, `[[`, "X"))
  Z <- do.call(rbind, lapply(data, `[[`, "Z"))
  if(!is.null(cluster_weights))
    w <- unlist(Map(
      rep, cluster_weights,
      times = sapply(data, function(x) length(x$y))))
  else
    w <- rep(1, length(y))

  # checks
  n <- length(y)
  stopifnot(
    length(y) > 0, is.numeric(y),
    NROW(X) == n, NROW(Z) == n,
    is.null(cluster_weights) || (
      length(cluster_weights) == length(data) &&
        is.numeric(cluster_weights)))

  # fit the model without random effects
  start_fit <-  glm.fit(X, y, weights = w, family = binomial("probit"))
  beta <- start_fit$coefficients
  logLik_no_rng <- -sum(start_fit$deviance) / 2

  # find the linear combination which yields an intercept
  n_scales <- get_n_scales(ptr)
  n_scales_mats <- n_scales %/% NCOL(Z)

  if(is.null(sc_start_invariant))
    sc_start_invariant <- .5^2
  if(length(sc_start_invariant) == 1)
    sc_start_invariant <- rep(sc_start_invariant, n_scales_mats)

  stopifnot(is.numeric(sc_start_invariant), all(sc_start_invariant > 0),
            length(sc_start_invariant) == n_scales_mats)

  fit_slope <- lm.fit(Z, rep(1, NROW(Z)))$coef
  thetas <- fit_slope %o% log(sc_start_invariant) / 2
  beta_scaled <- beta * sqrt(1 + sum(sc_start_invariant))

  list(par = c(beta_scaled, thetas),
       beta_no_rng = beta, logLik_no_rng = logLik_no_rng)
}

#' Optimize the Log Marginal Likelihood Using a Stochastic Quasi-Newton Method
#'
#' Optimizes \code{\link{eval_pedigree_ll}} and \code{\link{eval_pedigree_grad}}
#' using a stochastic quasi-Newton method.
#'
#' @inheritParams pedmod_opt
#' @param ptr object from \code{\link{pedigree_ll_terms}}.
#' @param par starting values.
#' @param step_factor factor used for the step size. The step size is
#' \code{step_factor} divided by the iteration number.
#' @param n_it number of stochastic gradient steps to make.
#' @param n_grad_steps number of stochastic gradient steps to make between each
#' Hessian approximation update.
#' @param n_grad number of log marginal likelihood terms to include in the
#' stochastic gradient step.
#' @param n_hess number of log marginal likelihood terms to include in the
#' gradients used for the Hessian approximation update. This is set to the
#' entire sample (or \code{indices}) if this is greater than or equal to half
#' the number of log marginal likelihood terms.
#' @param minvls_hess \code{minvls} argument to use when updating the Hessian
#' approximation.
#' @param maxvls_hess \code{maxvls} argument to use when updating the Hessian
#' approximation.
#' @param abs_eps_hess \code{abs_eps} argument to use when updating the Hessian
#' approximation.
#' @param rel_eps_hess \code{rel_eps} argument to use when updating the Hessian
#' approximation.
#' @param verbose logical for whether to print output during the estimation.
#' @param check_every integer for the number of gradient steps between checking
#' that the likelihood did increase. If not, the iterations are reset and the
#' step-size is halved.
#'
#' @details
#' The function uses a stochastic quasi-Newton method like suggested by
#' Byrd et al. (2016) with a few differences: Differences in gradients are
#' used rather than Hessian-vector products, BFGS rather than L-BFGS is used
#' because the problem is typically low dimensional, and damped BFGS updates
#' are used (see e.g. chapter 18 of Nocedal and Wright, 2006).
#'
#' Separate arguments for the gradient approximation in the Hessian update are
#' provided as one may want a more precise approximation for these gradients.
#' \code{step_factor} likely depends on the other parameters and the data set
#' and should be altered.
#'
#' @references
#'
#' Byrd, R. H., Hansen, S. L., Nocedal, J., & Singer, Y. (2016).
#' \emph{A stochastic quasi-Newton method for large-scale optimization}.
#' SIAM Journal on Optimization, 26(2), 1008-1031.
#'
#' Nocedal, J., & Wright, S. (2006). \emph{Numerical optimization}.
#' Springer Science & Business Media.
#'
#' @seealso
#' \code{\link{pedmod_opt}} and \code{\link{pedmod_start}}.
#'
#' @return
#' A list with the following elements:
#'
#' \item{par}{estimated parameters.}
#' \item{omegas}{parameter estimates after each iteration.}
#' \item{H}{Hessian approximation in the quasi-Newton method. It should not
#' be treated as the Hessian.}
#'
#' @examples
#' \donttest{
#' # we simulate outcomes with an additive genetic effect. The kinship matrix is
#' # the same for all families and given by
#' K <- matrix(c(
#'   0.5  , 0    , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0    , 0.5  , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0.25 , 0.25 , 0.5  , 0   , 0.25 , 0   , 0.25  , 0.25  , 0.125 , 0.125 ,
#'   0    , 0    , 0    , 0.5 , 0    , 0   , 0.25  , 0.25  , 0     , 0     ,
#'   0.25 , 0.25 , 0.25 , 0   , 0.5  , 0   , 0.125 , 0.125 , 0.25  , 0.25  ,
#'   0    , 0    , 0    , 0   , 0    , 0.5 , 0     , 0     , 0.25  , 0.25  ,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.5   , 0.25  , 0.0625, 0.0625,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.25  , 0.5   , 0.0625, 0.0625,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.5   , 0.25  ,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.25  , 0.5
#' ), 10)
#'
#' # simulates a data set.
#' #
#' # Args:
#' #   n_fams: number of families.
#' #   beta: the fixed effect coefficients.
#' #   sig_sq: the scale parameter.
#' sim_dat <- function(n_fams, beta = c(-1, 1, 2), sig_sq = 3){
#'   # setup before the simulations
#'   Cmat <- 2 * K
#'   n_obs <- NROW(K)
#'   Sig <- diag(n_obs) + sig_sq * Cmat
#'   Sig_chol <- chol(Sig)
#'
#'   # simulate the data
#'   out <- replicate(
#'     n_fams, {
#'       # simulate covariates
#'       X <- cbind(`(Intercept)` = 1, Continuous = rnorm(n_obs),
#'                  Binary = runif(n_obs) > .5)
#'
#'       # assign the linear predictor + noise
#'       eta <- drop(X %*% beta) + drop(rnorm(n_obs) %*% Sig_chol)
#'
#'       # return the list in the format needed for the package
#'       list(y = as.numeric(eta > 0), X = X, scale_mats = list(Cmat))
#'     }, simplify = FALSE)
#'
#'   # add attributes with the true values and return
#'   attributes(out) <- list(beta = beta, sig_sq = sig_sq)
#'   out
#' }
#'
#' # simulate the data
#' set.seed(1)
#' dat <- sim_dat(100L)
#'
#' # fit the model
#' ptr <- pedigree_ll_terms(dat, max_threads = 1L)
#' start <- pedmod_start(ptr = ptr, data = dat, n_threads = 1L)
#' fit <- pedmod_sqn(ptr = ptr, par = start$par, n_threads = 1L, use_aprx = TRUE,
#'                   maxvls = 5000L, minvls = 1000L, abs_eps = 0, rel_eps = 1e-3,
#'                   n_grad_steps = 20L, step_factor = 1, n_grad = 10L,
#'                   n_hess = 50L, check_every = 50L, n_it = 1000L)
#' fit$par # maximum likelihood estimate
#' # the maximum likelihood
#' eval_pedigree_ll(ptr = ptr, fit$par, maxvls = 5000L, abs_eps = 0,
#'                  rel_eps = 1e-3, minvls = 1000L)
#' }
#' @export
pedmod_sqn <- function(ptr, par, maxvls, abs_eps, rel_eps, step_factor,
                       n_it, n_grad_steps, indices = NULL, minvls = -1L,
                       n_grad = 50L, n_hess = 500L, do_reorder = TRUE,
                       use_aprx = FALSE, n_threads = 1L, cluster_weights = NULL,
                       fix = NULL, standardized = FALSE, minvls_hess = minvls,
                       maxvls_hess = maxvls, abs_eps_hess = abs_eps,
                       rel_eps_hess = rel_eps, verbose = FALSE,
                       method = 0L, check_every = 2L * n_grad_steps,
                       use_tilting = FALSE, vls_scales = NULL){
  # checks
  stopifnot(!missing(ptr), !missing(par), !missing(maxvls), !missing(abs_eps),
            !missing(rel_eps))
  if(!inherits(ptr, "pedigree_ll_terms_ptr"))
    .not_supported_for(ptr)

  #####
  # setup before the estimation
  n_pars <- length(par) - length(fix)
  omegas <- matrix(NA_real_, n_pars, ceiling(n_it / n_grad_steps) + 2L)
  H <- diag(n_pars)
  any_fixed <- length(fix) > 0
  w_old <- if(any_fixed) par[-fix] else par
  omegas[, 1L] <- w_old

  n_terms <- get_n_terms(ptr)
  if(is.null(indices))
    indices <- 0:(n_terms - 1L)
  n_terms <- min(n_terms, length(indices))
  n_grad <- min(n_grad, n_terms)
  n_hess <- min(n_hess, n_terms)

  # we alter n_hess if it is greater than or equal to the number of observations
  if(n_hess >= n_terms / 2)
    n_hess <- n_terms
  hess_use_all <- n_hess == n_terms

  # assign the function and the gradient functions
  get_par <- function(x){
    if(!any_fixed)
      return(x)
    out <- par
    out[-fix] <- x
    out
  }
  fn <- function(x, abs_eps, rel_eps, minvls, maxvls, indices){
    out <- try(-eval_pedigree_ll(
      ptr = ptr, par = get_par(x), maxvls = maxvls, rel_eps = rel_eps,
      indices = indices, minvls = minvls, abs_eps = abs_eps,
      do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
      cluster_weights = cluster_weights, standardized = standardized,
      method = method, use_tilting = use_tilting, vls_scales = vls_scales),
      silent = TRUE)
    if(inherits(out, "try-error"))
      return(NA_real_)

    denom <- if(!is.null(indices))
      length(indices) else n_terms

    structure(c(out) / denom, std = attr(out, "std") / denom)
  }
  gr <- function(x, abs_eps, rel_eps, minvls, maxvls, indices){
    out <-
      -eval_pedigree_grad(ptr = ptr, par = get_par(x), maxvls = maxvls,
                          rel_eps = rel_eps, indices = indices, minvls = minvls,
                          abs_eps = abs_eps, do_reorder = do_reorder,
                          use_aprx = use_aprx, n_threads = n_threads,
                          cluster_weights = cluster_weights,
                          standardized = standardized, method = method,
                          use_tilting = use_tilting, vls_scales = vls_scales)
    if(any_fixed)
      out <- out[-fix]
    if(!is.null(indices))
      out / length(indices) else out / n_terms
  }

  if(hess_use_all)
    # we will need this later
    g_new <- gr(omegas[, 1], abs_eps = abs_eps_hess, rel_eps = rel_eps_hess,
                minvls = minvls_hess, maxvls = maxvls_hess,
                indices = indices)

  #####
  # perform the optimization
  k <- 0L
  stopifnot(n_it > 0, n_grad_steps > 0)
  w_vals <- matrix(NA_real_, n_pars, n_grad_steps)
  w_new <- w_old
  t <- 1L

  # values for the from the previous check
  fn_old <- fn(w_new, abs_eps = abs_eps, rel_eps = rel_eps,
               minvls = minvls, maxvls = maxvls, indices = NULL)
  old_par <- w_new
  old_k <- k
  old_H <- H
  t_old <- t
  min_step_size <- step_factor / 2^8

  .check_if_increased <- function(){
    if(k %% check_every == 0){
      fn_new <- fn(w_new, abs_eps = abs_eps, rel_eps = rel_eps,
                   minvls = minvls, maxvls = maxvls, indices = NULL)
      var_est <- attr(fn_new, "std")^2 + attr(fn_old, "std")^2

      if(fn_new < fn_old + 2.326 * sqrt(var_est)){
        # fine as we are minimizing the negative likelihood
        fn_old <<- fn_new
        old_k <<- k
        old_H <<- H
        old_par <<- w_new
        t_old <<- t

      } else {
        # we reset
        H <<- old_H
        w_new <<- old_par
        step_factor <<- step_factor / 2
        t <<- t_old
        if(step_factor > min_step_size)
          # otherwise we can go on forever
          k <<- old_k - 1L

        if(verbose)
          cat(sprintf(
            "\nThe log-likelihood had not increased. The difference is %.6f from %.3f.\nHalving the step size to %f\n",
            -(fn_new - fn_old), -fn_old * n_terms, step_factor))

        return(FALSE)

      }
    }

    TRUE
  }

  while(k < n_it){
    w_vals[] <- NA_real_
    i_indices <- if(k < 1) 1L else 1:n_grad_steps
    for(i in i_indices){
      if((k <- k + 1L) > n_it)
        break

      # check if we need to reset
      if(!(is_ok <- .check_if_increased()))
        break

      # perform the gradient step
      w_old <- w_new
      S <- sample(indices, n_grad)
      gr_val <- gr(w_old, abs_eps = abs_eps, rel_eps = rel_eps,
                   minvls = minvls, maxvls = maxvls, indices = S)
      w_new <- w_old - step_factor / k * drop(H %*% gr_val)
      w_vals[, i] <- w_new
    }

    if(!is_ok)
      next

    if(i < min(n_grad_steps, length(i_indices)))
      # no need for an update of the Hessian approximation
      break

    t <- t + 1L
    omegas[, t] <- rowMeans(w_vals[, i_indices, drop = FALSE], na.rm = TRUE)
    s <- omegas[, t] - omegas[, t - 1L]
    S <- if(hess_use_all) indices else sample(indices, n_hess)

    . <- function(x)
      gr(x, abs_eps = abs_eps_hess, rel_eps = rel_eps_hess,
         minvls = minvls_hess, maxvls = maxvls_hess, indices = S)
    if(hess_use_all){
      g_old <- g_new
      g_new <- .(omegas[, t     ])

    } else {
      g_old <- .(omegas[, t - 1L])
      g_new <- .(omegas[, t     ])
    }

    if(verbose){
      cat(sprintf(
        "\nHessian update %5d\nLog marignal likelihood at previous and new average parameter vector on the same sample is:\n  %14.3f\n  %14.3f\n",
        t, attr(g_old, "logLik"), attr(g_new, "logLik")))
      cat("New average parameter vector is\n")
      print(omegas[, t])
    }

    y <- g_new - g_old
    s_y <- sum(s * y)
    if(t <= 2L){
      # the first iteration
      rho <- s_y / sum(y * y)
      if(is.finite(rho) && rho > 0)
        H <- diag(rho, n_pars)
    }

    # damped BFGS update
    B_s <- solve(H, s)
    s_B_s <- drop(s %*% B_s)
    theta <- if(s_y >= .2 * s_B_s)
      1 else .8 * s_B_s / (s_B_s - s_y)
    r <- theta * y + (1 - theta) * B_s

    # TODO: can be done smarter
    r_s <- sum(r * s)
    D <- -outer(r, s) / r_s
    diag(D) <- diag(D) + 1

    H <- crossprod(D, H %*% D) + outer(s, s) / r_s
  }

  omegas <- omegas[, apply(!is.na(omegas), 2L, all), drop = FALSE]
  list(par = w_new, omegas = omegas, H = H)
}


#' Computes Profile Likelihood Based Confidence Intervals
#'
#' @description
#' Computes likelihood ratio based confidence intervals for one the parameters
#' in the model.
#'
#' @inheritParams pedmod_opt
#'
#' @param ptr object from \code{\link{pedigree_ll_terms}} or
#' \code{\link{pedigree_ll_terms_loadings}}.
#' @param par numeric vector with the maximum likelihood estimator e.g. from
#' \code{\link{pedmod_opt}}.
#' @param delta numeric scalar with an initial step to take. Subsequent steps
#' are taken by \code{2^(<iteration number> - 1) * delta}. Two times the
#' standard error is a good value or a guess thereof. Hessian approximations are
#' not implemented as of this writing and therefore the user needs to provide
#' some guess.
#' @param alpha numeric scalar with the confidence level required.
#' @param which_prof integer scalar with index of the parameter which the
#' profile likelihood curve should be computed for.
#' @param max_step integer scalar with the maximum number of steps to take in
#' either directions.
#' @param verbose logical for whether output should be printed to the console
#' during the estimation of the profile likelihood curve.
#' @param ... arguments passed on to \code{\link{pedmod_opt}}.
#' @param maxvls_start,minvls_start number of samples to use when finding the
#' initial values for the optimization.
#'
#' @seealso
#' \code{\link{pedmod_opt}}, \code{\link{pedmod_sqn}},
#' \code{\link{pedmod_profile_prop}}, and \code{\link{pedmod_profile_nleq}}
#'
#'
#' @return
#' A list with the following elements:
#'   \item{confs}{2D numeric vector with the profile likelihood based confidence
#'                interval.}
#'   \item{xs}{the points at which the profile likelihood is evaluated.}
#'   \item{p_log_Lik}{the log profile likelihood values at \code{xs}.}
#'   \item{data}{list with the returned objects from \code{\link{pedmod_opt}}.}
#'
#' @importFrom stats spline approx qchisq qnorm setNames splinefun
#'
#' @examples
#' \donttest{
#' # we simulate outcomes with an additive genetic effect. The kinship matrix is
#' # the same for all families and given by
#' K <- matrix(c(
#'   0.5  , 0    , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0    , 0.5  , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0.25 , 0.25 , 0.5  , 0   , 0.25 , 0   , 0.25  , 0.25  , 0.125 , 0.125 ,
#'   0    , 0    , 0    , 0.5 , 0    , 0   , 0.25  , 0.25  , 0     , 0     ,
#'   0.25 , 0.25 , 0.25 , 0   , 0.5  , 0   , 0.125 , 0.125 , 0.25  , 0.25  ,
#'   0    , 0    , 0    , 0   , 0    , 0.5 , 0     , 0     , 0.25  , 0.25  ,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.5   , 0.25  , 0.0625, 0.0625,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.25  , 0.5   , 0.0625, 0.0625,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.5   , 0.25  ,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.25  , 0.5
#' ), 10)
#'
#' # simulates a data set.
#' #
#' # Args:
#' #   n_fams: number of families.
#' #   beta: the fixed effect coefficients.
#' #   sig_sq: the scale parameter.
#' sim_dat <- function(n_fams, beta = c(-1, 1, 2), sig_sq = 3){
#'   # setup before the simulations
#'   Cmat <- 2 * K
#'   n_obs <- NROW(K)
#'   Sig <- diag(n_obs) + sig_sq * Cmat
#'   Sig_chol <- chol(Sig)
#'
#'   # simulate the data
#'   out <- replicate(
#'     n_fams, {
#'       # simulate covariates
#'       X <- cbind(`(Intercept)` = 1, Continuous = rnorm(n_obs),
#'                  Binary = runif(n_obs) > .5)
#'
#'       # assign the linear predictor + noise
#'       eta <- drop(X %*% beta) + drop(rnorm(n_obs) %*% Sig_chol)
#'
#'       # return the list in the format needed for the package
#'       list(y = as.numeric(eta > 0), X = X, scale_mats = list(Cmat))
#'     }, simplify = FALSE)
#'
#'   # add attributes with the true values and return
#'   attributes(out) <- list(beta = beta, sig_sq = sig_sq)
#'   out
#' }
#'
#' # simulate the data
#' set.seed(1)
#' dat <- sim_dat(100L)
#'
#' # fit the model
#' ptr <- pedigree_ll_terms(dat, max_threads = 1L)
#' start <- pedmod_start(ptr = ptr, data = dat, n_threads = 1L)
#' fit <- pedmod_opt(ptr = ptr, par = start$par, n_threads = 1L, use_aprx = TRUE,
#'                   maxvls = 5000L, minvls = 1000L, abs_eps = 0, rel_eps = 1e-3)
#' fit$par # the estimate
#'
#' # 90% likelihood ratio based confidence interval for the log of the scale
#' # parameter
#' prof_out <- pedmod_profile(ptr = ptr, fit$par, delta = .4, maxvls = 5000L,
#'                            minvls = 1000L, alpha = .1, which_prof = 4L,
#'                            abs_eps = 0, rel_eps = 1e-3, verbose = TRUE)
#' exp(prof_out$confs) # the confidence interval
#'
#' # plot the log profile likelihood
#' plot(exp(prof_out$xs), prof_out$p_log_Lik, pch = 16,
#'      xlab = expression(sigma), ylab = "log profile likelihood")
#' abline(v = exp(prof_out$confs), lty = 2)
#' }
#'
#' @export
pedmod_profile <- function(ptr, par, delta, maxvls, minvls = -1L,
                           alpha = .05, abs_eps,
                           rel_eps, which_prof, indices = NULL,
                           maxvls_start = max(100L,
                                              as.integer(ceiling(maxvls / 5))),
                           minvls_start = if(minvls < 0) minvls else minvls / 5,
                           do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L,
                           cluster_weights = NULL, method = 0L, seed = 1L,
                           verbose = FALSE, max_step = 15L,
                           standardized = FALSE, use_tilting = FALSE,
                           vls_scales = NULL, ...){
  # checks
  stopifnot(
    !missing(ptr), !missing(par), !missing(maxvls), !missing(abs_eps),
    !missing(rel_eps),
    is.numeric(par),
    length(which_prof) == 1L, is.integer(which_prof),
    which_prof %in% seq_along(par),
    is.numeric(delta), is.finite(delta), delta > 0,
    is.numeric(alpha), length(alpha) == 1, is.finite(alpha),
    alpha > 0, alpha < 1)

  is_wo_lodaings <- inherits(ptr, "pedigree_ll_terms_ptr")
  if(!is_wo_lodaings & !inherits(ptr, "pedigree_ll_terms_loadings_ptr"))
    .not_supported_for(ptr)

  # assign function to evaluate the log likelihood
  fn <- function(par, minv = minvls){
    set.seed(seed)
    eval_pedigree_ll(ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps,
                     rel_eps = rel_eps, indices = indices, minvls = minv,
                     do_reorder = do_reorder, use_aprx = use_aprx,
                     n_threads = n_threads, cluster_weights = cluster_weights,
                     standardized = standardized, method = method,
                     use_tilting = use_tilting, vls_scales = vls_scales)
  }
  optim_res <- list(par = par[-which_prof], value = -fn(par))

  if(verbose)
    message(sprintf("The estimate of the standard error of the log likelihood is %.8f. Preferably this should be below 0.001",
                    attr(optim_res$value, "std")))

  # assign function to do the model fitting
  n_scales <- get_n_scales(ptr)
  if(is_wo_lodaings){
    total_var <- 1 + sum(exp(tail(par, n_scales)))
    beta_0 <- head(par, -n_scales) / sqrt(total_var)
  }

  chi_val <- qchisq(1 - alpha, 1)
  crit_value <- -optim_res$value - chi_val / 2

  wrap_optim <- function(x, optim_obj, dir, ub = NULL, lb = NULL){
    if(verbose){
      new_val <- -optim_obj$value
      message(sprintf("LogLike: %.4f at %16f", new_val, x), appendLF = FALSE)
      if(!is.null(ub) && !is.null(lb)){
        if(new_val < crit_value)
          lb$value <- new_val
        else
          ub$value <- new_val
        message(sprintf(". Lb, target, ub: %.4f, %.4f, %.4f",
                        lb$value, crit_value, ub$value))

      } else
        message() # line feed
    }
    list(x = x, value = -optim_obj$value, optim = optim_obj,
         z_val = sign(dir) * sqrt((optim_obj$value - optim_res$value) * 2))
  }

  do_fit <- function(x, dir, lb = NULL, ub = NULL){
    # get the starting value
    par[which_prof] <- x
    if(is_wo_lodaings && which_prof > length(beta_0) && !standardized){
      total_var <- 1 + sum(exp(tail(par, n_scales)))
      par[seq_along(beta_0)] <- beta_0 * sqrt(total_var)
    }

    if(maxvls > maxvls_start){
      opt_quick <- pedmod_opt(
        ptr = ptr, par = par, maxvls = maxvls_start, abs_eps = abs_eps,
        rel_eps = rel_eps, seed = seed, indices = indices,
        minvls = minvls_start, do_reorder = do_reorder, use_aprx = use_aprx,
        n_threads = n_threads, fix = which_prof,
        cluster_weights = cluster_weights, standardized = standardized,
        method = method, ...)

      par[-which_prof] <- opt_quick$par
      opt_quick <- wrap_optim(x, opt_quick, dir, lb = lb, ub = ub)
    } else
      opt_quick <- NULL

    opt_out <- pedmod_opt(
      ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps, rel_eps = rel_eps,
      seed = seed, indices = indices, minvls = minvls, do_reorder = do_reorder,
      use_aprx = use_aprx, n_threads = n_threads, fix = which_prof,
      cluster_weights = cluster_weights, standardized = standardized,
      method = method, ...)

    structure(wrap_optim(x, opt_out, dir, lb = lb, ub = ub),
              opt_quick = opt_quick)
  }

  # find points on the profile likelihood curve in either direction
  get_points <- function(dir){
    dir <- sign(dir)
    if(verbose)
      message(sprintf(
        "\nFinding the %s limit of the profile likelihood curve",
        if(dir < 0) "lower" else "upper"))

    step <- 0L
    out <- vector("list", max_step)
    prev <- -optim_res$value
    did_fail <- FALSE

    while(prev > crit_value && (step <- step + 1L) <= max_step){
      out[[step]] <- do_fit(par[which_prof] + dir *  2^(step - 1) * delta,
                            dir = dir)
      if(out[[step]]$value > prev){
        warning("Log likelihood did not decrease. Either 'optim_res' is not an optimum or the precision needs to be increased")
        did_fail <- TRUE
        break
      }

      prev <- out[[step]]$value
    }

    .report_failed <- function()
      if(verbose && step > max_step)
        warning(sprintf(
          "Failed to find the appropiate point in %d steps", max_step))

    .report_failed()

    if(did_fail || step > max_step)
      return(out[sapply(out, length) > 0])

    ub <- if(step == 1L)
      wrap_optim(par[which_prof], optim_res, 1) else out[[step - 1L]]
    lb <- out[[step]]

    while(ub$value - lb$value > chi_val / 6 && (step <- step + 1L) <= max_step){
      # compute the next value
      xs <- c(unlist(sapply(out, `[[`, "x"))    , par[which_prof])
      ys <- c(unlist(sapply(out, `[[`, "value")), -optim_res$value)
      sp <- splinefun(ys, xs, method = "monoH.FC")
      y <- seq(lb$value, ub$value, length.out = 20)
      x <- sp(y)
      next_val <- approx(y, x, xout = crit_value)$y
      if(abs(next_val - ub$x) > abs(next_val - lb$x))
        next_val <- 8 / 9 * next_val + ub$x / 9
      else
        next_val <- 8 / 9 * next_val + lb$x / 9

      out[[step]] <- do_fit(next_val, dir = dir, lb = lb, ub = ub)

      if(out[[step]]$value > ub$value || out[[step]]$value < lb$value){
        warning("Log likelihood does not seem monotonic. Likely the precision needs to be increased")
        break
      }

      if(out[[step]]$value < crit_value)
        lb <- out[[step]]
      else
        ub <- out[[step]]
    }

    .report_failed()
    out <- out[sapply(out, length) > 0]
    out[order(sapply(out, `[[`, "x"))]
  }

  res_down <- get_points(-1)
  res_up   <- get_points( 1)
  out <- c(
    res_down, list(wrap_optim(par[which_prof], optim_res, dir = 0)), res_up)

  # compute the confidence interval
  xs  <- sapply(out, `[[`, "x")
  zs  <- sapply(out, `[[`, "z_val")
  pls <- sapply(out, `[[`, "value")

  confs <- comp_confs(xs, zs, alpha)

  list(confs = confs, xs = xs, p_log_Lik = pls, data = out)
}

#' @importFrom stats setNames approx spline
comp_confs <- function(xs, z_vals, alpha){
  keep <- qnorm(alpha, lower.tail = FALSE) - abs(z_vals) > -4
  xs <- xs[keep]
  z_vals <- z_vals[keep]

  sp <- spline(xs, z_vals, n = 1000)
  pvs <- c(alpha / 2, 1 - alpha/2)
  setNames(approx(sp$y, sp$x, xout = qnorm(pvs))$y,
           sprintf("%.2f pct.", 100 * pvs))
}

#' Computes Profile Likelihood Based Confidence Intervals for the Proportion
#' of Variance
#'
#' @description
#' Constructs a likelihood ratio based confidence intervals for the
#' proportion of variance for one of the effects.
#'
#' @inheritParams pedmod_profile
#'
#' @param ptr object from \code{\link{pedigree_ll_terms}}.
#' @param which_prof the index of the random effect which proportion of
#' variance should be profiled.
#' @param opt_func function to perform minimization with arguments like
#' \code{\link{optim}}. BFGS is used with \code{\link{optim}} if this argument
#' is \code{NULL}.
#' @param bound boundaries for the limits of the proportion. Has to be in
#' between \eqn{(0,1)}. This is useful particularly if the optimization fails to
#' work on the default values.
#' @param ... arguments passed to \code{opt_func}.
#'
#' @details
#' The function is only useful when there is more than one type of random
#' effect. If not, then \code{\link{pedmod_profile}} can be used because of
#' the scale invariance of the likelihood ratio.
#'
#' @return
#' A list like \code{\link{pedmod_profile}}.
#'
#' @seealso
#' \code{\link{pedmod_opt}}, \code{\link{pedmod_sqn}},
#' \code{\link{pedmod_profile}}, and \code{\link{pedmod_profile_nleq}}.
#'
#' @examples
#' \donttest{
#' # we simulate outcomes with an additive genetic effect and a childhood
#' # environment effect. The kinship matrix is the same for all families and
#' # given by
#' K <- matrix(c(
#'   0.5  , 0    , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0    , 0.5  , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0.25 , 0.25 , 0.5  , 0   , 0.25 , 0   , 0.25  , 0.25  , 0.125 , 0.125 ,
#'   0    , 0    , 0    , 0.5 , 0    , 0   , 0.25  , 0.25  , 0     , 0     ,
#'   0.25 , 0.25 , 0.25 , 0   , 0.5  , 0   , 0.125 , 0.125 , 0.25  , 0.25  ,
#'   0    , 0    , 0    , 0   , 0    , 0.5 , 0     , 0     , 0.25  , 0.25  ,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.5   , 0.25  , 0.0625, 0.0625,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.25  , 0.5   , 0.0625, 0.0625,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.5   , 0.25  ,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.25  , 0.5
#' ), 10)
#'
#' # the scale matrix for the childhood environment effect is also the same and
#' # given by
#' C <- matrix(c(
#'   1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'   0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 1
#' ), 10L)
#'
#' # simulates a data set.
#' #
#' # Args:
#' #   n_fams: number of families.
#' #   beta: the fixed effect coefficients.
#' #   sig_sq: the scale parameters.
#' sim_dat <- function(n_fams, beta = c(-1, 1, 2), sig_sq = c(3, 1)){
#'   # setup before the simulations
#'   Cmat <- 2 * K
#'   n_obs <- NROW(K)
#'   Sig <- diag(n_obs) + sig_sq[1] * Cmat + sig_sq[2] * C
#'   Sig_chol <- chol(Sig)
#'
#'   # simulate the data
#'   out <- replicate(
#'     n_fams, {
#'       # simulate covariates
#'       X <- cbind(`(Intercept)` = 1, Continuous = rnorm(n_obs),
#'                  Binary = runif(n_obs) > .5)
#'
#'       # assign the linear predictor + noise
#'       eta <- drop(X %*% beta) + drop(rnorm(n_obs) %*% Sig_chol)
#'
#'       # return the list in the format needed for the package
#'       list(y = as.numeric(eta > 0), X = X,
#'            scale_mats = list(genetic = Cmat, environment = C))
#'     }, simplify = FALSE)
#'
#'   # add attributes with the true values and return
#'   attributes(out) <- list(beta = beta, sig_sq = sig_sq)
#'   out
#' }
#'
#' # simulate the data
#' set.seed(1)
#' dat <- sim_dat(200L)
#'
#' # fit the model
#' ptr <- pedigree_ll_terms(dat, max_threads = 1L)
#' start <- pedmod_start(ptr = ptr, data = dat, n_threads = 1L)
#' fit <- pedmod_opt(ptr = ptr, par = start$par, n_threads = 1L, use_aprx = TRUE,
#'                   maxvls = 5000L, minvls = 1000L, abs_eps = 0, rel_eps = 1e-3)
#' fit$par # the estimate
#'
#' # 90% likelihood ratio based confidence interval for the proportion of variance
#' # of the genetic effect
#' prof_out <- pedmod_profile_prop(
#'   ptr = ptr, fit$par, maxvls = 5000L, minvls = 1000L, alpha = .1,
#'   which_prof = 1L, abs_eps = 0, rel_eps = 1e-3, verbose = TRUE)
#' prof_out$confs # the confidence interval for the proportion
#'
#' # plot the log profile likelihood
#' keep <- c(-1L, -length(prof_out$xs))
#' plot(prof_out$xs[keep], prof_out$p_log_Lik[keep], pch = 16,
#'      xlab = "proportion of variance", ylab = "log profile likelihood")
#' abline(v = prof_out$confs, lty = 2)
#' }
#'
#' @export
pedmod_profile_prop <- function(
  ptr, par, maxvls, minvls = -1L, alpha = .05, abs_eps,
  rel_eps, which_prof, indices = NULL,
  maxvls_start = max(100L, as.integer(ceiling(maxvls / 5))),
  minvls_start = if(minvls < 0) minvls else minvls / 5,
  do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L, cluster_weights = NULL,
  method = 0L, seed = 1L, verbose = FALSE, max_step = 15L,
  opt_func = NULL, use_tilting = FALSE, vls_scales = NULL,
  bound = c(.01, .99), ...){
  # checks
  standardized <- FALSE
  n_scales <- get_n_scales(ptr)
  stopifnot(
    !missing(par), !missing(maxvls), !missing(abs_eps),
    !missing(rel_eps),
    is.numeric(par),
    length(which_prof) == 1L, is.integer(which_prof),
    which_prof %in% 1:n_scales,
    is.numeric(alpha), length(alpha) == 1, is.finite(alpha),
    alpha > 0, alpha < 1,
    all(is.finite(bound)), diff(bound) > 0, 0 < bound[1],
    bound[2] < 1)
  if(n_scales < 2)
    stop("pedmod_profile when there is just one scale parameter")
  if(!inherits(ptr, "pedigree_ll_terms_ptr"))
    .not_supported_for(ptr)

  # assign function to evaluate the log likelihood
  fn <- function(par, minv = minvls){
    set.seed(seed)
    eval_pedigree_ll(ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps,
                     rel_eps = rel_eps, indices = indices, minvls = minv,
                     do_reorder = do_reorder, use_aprx = use_aprx,
                     n_threads = n_threads, cluster_weights = cluster_weights,
                     method = method, use_tilting = use_tilting,
                     vls_scales = vls_scales)
  }
  optim_res <- list(par = par, value = -fn(par))

  if(verbose)
    message(sprintf("The estimate of the standard error of the log likelihood is %.8f. Preferably this should be below 0.001",
                    attr(optim_res$value, "std")))

  # assign function to do the model fitting
  total_var <- 1 + sum(exp(tail(par, n_scales)))
  beta_0 <- head(par, -n_scales) / sqrt(total_var)

  chi_val <- qchisq(1 - alpha, 1)
  crit_value <- -optim_res$value - chi_val / 2

  # function to compute the scale parameter from the other scales and the
  # proportion of variance
  is_beta <- seq_len(length(par) - n_scales)
  is_scales <- (length(par) - n_scales + 1L):length(par)

  scales <- exp(par[is_scales])
  prop_var_max <- scales[which_prof] / (1 + sum(scales))

  get_par <- function(x, prop_var){
    scales <- c(0, x[-is_beta])
    scales_max <- max(scales)

    scale_prof <- log(prop_var / (1 - prop_var)) + scales_max +
      log(sum(exp(scales - scales_max)))
    c(x[is_beta], x[-is_beta], scale_prof)[
      c(is_beta, is_scales[-which_prof], is_scales[which_prof])]
  }

  wrap_optim <- function(x, optim_obj, dir, lb = NULL, ub = NULL){
    if(verbose){
      new_val <- -optim_obj$value
      message(sprintf("LogLike: %.4f at %16f", new_val, x), appendLF = FALSE)
      if(!is.null(ub) && !is.null(lb)){
        if(new_val < crit_value)
          lb$value <- new_val
        else
          ub$value <- new_val
        message(sprintf(". Lb, target, ub: %.4f, %.4f, %.4f",
                        lb$value, crit_value, ub$value))

      } else
        message() # line feed
    }
    list(x = x, value = -optim_obj$value, optim = optim_obj,
         z_val = sign(dir) * sqrt((optim_obj$value - optim_res$value) * 2))
  }

  do_fit <- function(x, dir, lb = NULL, ub = NULL){
    # get the starting value
    par <- par[-is_scales[which_prof]]

    # re-scale the starting values
    new_total_var <- (function(new_par){
      sum(exp(new_par[is_scales])) + 1
    })(get_par(par, x))
    par[is_beta] <- par[is_beta] * sqrt(new_total_var / total_var)

    # handle defaults
    if(is.null(opt_func)){
      opt_func <- optim
      formals(opt_func)$method <- "BFGS"
      formals(opt_func)$control <- list(fnscale = get_n_terms(ptr))
    }

    # checks
    stopifnot(is.function(opt_func))

    # define the function and gradient to minimize
    fn <- function(par, minvls, maxvls){
      if(!is.null(seed))
        set.seed(seed)

      out <- try(-eval_pedigree_ll(
        ptr = ptr, par = get_par(par, x), maxvls = maxvls, rel_eps = rel_eps,
        indices = indices, minvls = minvls, abs_eps = abs_eps,
        do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
        cluster_weights = cluster_weights, standardized = standardized,
        method = method, use_tilting = use_tilting, vls_scales = vls_scales),
        silent = TRUE)
      if(inherits(out, "try-error"))
        return(NA_real_)
      out
    }
    gr <- function(par, minvls, maxvls){
      if(!is.null(seed))
        set.seed(seed)
      par_vec <- get_par(par, x)
      out <- try(-eval_pedigree_grad(
        ptr = ptr, par = par_vec, maxvls = maxvls, rel_eps = rel_eps,
        indices = indices, minvls = minvls, abs_eps = abs_eps,
        do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
        cluster_weights = cluster_weights, standardized = standardized,
        method = method, use_tilting = use_tilting, vls_scales = vls_scales),
        silent = TRUE)
      if(inherits(out, "try-error"))
        return(rep(NA_real_, length(par_vec) - 1L))

      # have to add a gradient term
      attrit <- attributes(out)
      gr_which_prof <-
        out[is_scales[which_prof]] / exp(par_vec[is_scales[which_prof]])
      out <- out[-is_scales[which_prof]]
      out[-is_beta] <- out[-is_beta] + x / (1 - x) *
        exp(par_vec[is_scales[-which_prof]]) * gr_which_prof
      attributes(out) <- attrit
      out
    }

    if(maxvls > maxvls_start){
      opt_quick <- opt_func(par, fn, gr, ..., minvls = minvls_start,
                            maxvls = maxvls_start)

      par <- opt_quick$par
      tmp_opt <- opt_quick
      tmp_opt$par <- get_par(par, x)
      opt_quick <- wrap_optim(x, tmp_opt, dir, lb = lb, ub = ub)
    } else
      opt_quick <- NULL

    opt_out <- opt_func(
      par, fn, gr, ..., minvls = minvls, maxvls = maxvls)

    tmp_opt <- opt_out
    tmp_opt$par <- get_par(par, x)
    structure(wrap_optim(x, opt_out, dir, lb = lb, ub = ub),
              opt_quick = opt_quick)
  }

  # find points on the profile likelihood curve in either direction
  get_points <- function(dir){
    dir <- sign(dir)
    if(verbose)
      message(sprintf(
        "\nFinding the %s limit of the profile likelihood curve",
        if(dir < 0) "lower" else "upper"))

    step <- 0L
    out <- vector("list", max_step)
    prev <- -optim_res$value

    # check if the limit is ok
    step <- step + 1L
    if(dir < 0){
      lb <- bound[1]
      if(prop_var_max < lb){
        out[[step]] <- do_fit(prop_var_max / 2, dir = dir)
        return(out[sapply(out, length) > 0])
      }

      out[[step]] <- do_fit(lb, dir = dir)
      if(out[[step]]$value > crit_value)
        return(out[sapply(out, length) > 0])

    } else {
      ub <- bound[2]
      if(prop_var_max > ub){
        out[[step]] <- do_fit(1 - (1 - prop_var_max) / 2, dir = dir)
        return(out[sapply(out, length) > 0])
      }

      out[[step]] <- do_fit(ub, dir = dir)
      if(out[[step]]$value > crit_value)
        return(out[sapply(out, length) > 0])

    }

    # perform search
    .report_failed <- function()
      if(verbose && step > max_step)
        warning(sprintf(
          "Failed to find the appropiate point in %d steps", max_step))

    .report_failed()

    ub <- wrap_optim(prop_var_max, optim_res, 1)
    lb <- out[[step]]

    while(ub$value - lb$value > chi_val / 6 && (step <- step + 1L) <= max_step){
      # compute the next value
      xs <- c(unlist(sapply(out, `[[`, "x"))    , prop_var_max)
      ys <- c(unlist(sapply(out, `[[`, "value")), -optim_res$value)
      sp <- splinefun(ys, xs, method = "monoH.FC")
      y <- seq(lb$value, ub$value, length.out = 20)
      x <- sp(y)
      next_val <- approx(y, x, xout = crit_value)$y
      if(abs(next_val - ub$x) > abs(next_val - lb$x))
        next_val <- 8 / 9 * next_val + ub$x / 9
      else
        next_val <- 8 / 9 * next_val + lb$x / 9

      out[[step]] <- do_fit(next_val, dir = dir, lb = lb, ub = ub)

      if(out[[step]]$value > ub$value || out[[step]]$value < lb$value){
        warning("Log likelihood does not seem monotonic. Likely the precision needs to be increased")
        break
      }

      if(out[[step]]$value < crit_value)
        lb <- out[[step]]
      else
        ub <- out[[step]]
    }

    .report_failed()
    out <- out[sapply(out, length) > 0]
    out[order(sapply(out, `[[`, "x"))]
  }

  res_up   <- get_points( 1)
  res_down <- get_points(-1)
  out <- c(
    res_down, list(wrap_optim(prop_var_max, optim_res, dir = 0)), res_up)

  # compute the confidence interval
  xs  <- sapply(out, `[[`, "x")
  zs  <- sapply(out, `[[`, "z_val")
  pls <- sapply(out, `[[`, "value")

  confs <- comp_confs(xs, zs, alpha)

  list(confs = confs, xs = xs, p_log_Lik = pls, data = out)
}

#' Computes Profile Likelihood Based Confidence Intervals for a Nonlinear
#' Transformation of the Variables
#'
#' @inheritParams pedmod_profile
#' @param heq function that returns a one dimensional numerical vector which
#' should be profiled. It does not need to evaluate to zero at the maximum
#' likelihood estimator.
#' @param heq_bounds two dimensional numerical vector with bounds for
#' \code{heq}.
#' @param control.outer,control.optim,... arguments passed to
#' \code{\link[alabama]{auglag}}
#'
#' @seealso
#' \code{\link{pedmod_opt}}, \code{\link{pedmod_sqn}},
#' \code{\link{pedmod_profile}}, and \code{\link{pedmod_profile_prop}}.
#'
#' @examples
#' \donttest{
#' # similar examples to that in help("pedmod_profile_prop")
#' K <- matrix(c(
#'   0.5  , 0    , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0    , 0.5  , 0.25 , 0   , 0.25 , 0   , 0.125 , 0.125 , 0.125 , 0.125 ,
#'   0.25 , 0.25 , 0.5  , 0   , 0.25 , 0   , 0.25  , 0.25  , 0.125 , 0.125 ,
#'   0    , 0    , 0    , 0.5 , 0    , 0   , 0.25  , 0.25  , 0     , 0     ,
#'   0.25 , 0.25 , 0.25 , 0   , 0.5  , 0   , 0.125 , 0.125 , 0.25  , 0.25  ,
#'   0    , 0    , 0    , 0   , 0    , 0.5 , 0     , 0     , 0.25  , 0.25  ,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.5   , 0.25  , 0.0625, 0.0625,
#'   0.125, 0.125, 0.25 , 0.25, 0.125, 0   , 0.25  , 0.5   , 0.0625, 0.0625,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.5   , 0.25  ,
#'   0.125, 0.125, 0.125, 0   , 0.25 , 0.25, 0.0625, 0.0625, 0.25  , 0.5
#' ), 10)
#'
#' C <- matrix(c(
#'   1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'   0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 1
#' ), 10L)
#'
#' # simulates a data set.
#' #
#' # Args:
#' #   n_fams: number of families.
#' #   beta: the fixed effect coefficients.
#' #   sig_sq: the scale parameters.
#' sim_dat <- function(n_fams, beta = c(-1, 1, 2), sig_sq = c(3, 1)){
#'   # setup before the simulations
#'   Cmat <- 2 * K
#'   n_obs <- NROW(K)
#'   Sig <- diag(n_obs) + sig_sq[1] * Cmat + sig_sq[2] * C
#'   Sig_chol <- chol(Sig)
#'
#'   # simulate the data
#'   out <- replicate(
#'     n_fams, {
#'       # simulate covariates
#'       X <- cbind(`(Intercept)` = 1, Continuous = rnorm(n_obs),
#'                  Binary = runif(n_obs) > .5)
#'
#'       # assign the linear predictor + noise
#'       eta <- drop(X %*% beta) + drop(rnorm(n_obs) %*% Sig_chol)
#'
#'       # return the list in the format needed for the package
#'       list(y = as.numeric(eta > 0), X = X,
#'            scale_mats = list(genetic = Cmat, environment = C))
#'     }, simplify = FALSE)
#'
#'   # add attributes with the true values and return
#'   attributes(out) <- list(beta = beta, sig_sq = sig_sq)
#'   out
#' }
#'
#' # simulate the data
#' set.seed(1)
#' dat <- sim_dat(200L)
#'
#' # fit the model
#' ptr <- pedigree_ll_terms(dat, max_threads = 2L)
#' start <- pedmod_start(ptr = ptr, data = dat, n_threads = 2L)
#' fit <- pedmod_opt(ptr = ptr, par = start$par, use_aprx = TRUE, n_threads = 2L,
#'                   maxvls = 5000L, minvls = 1000L, abs_eps = 0, rel_eps = 1e-3)
#' fit$par # the estimate
#'
#' # 90% likelihood ratio based confidence interval for the proportion of variance
#' # of the genetic effect
#' heq <- function(par){
#'  vars <- exp(tail(par, 2))
#'  vars[1] / (1 + sum(vars))
#' }
#' heq(fit$par)
#' prof_out_nleq <- pedmod_profile_nleq(
#'   ptr = ptr, fit$par, maxvls = 2500L, minvls = 500L, alpha = .1,
#'   abs_eps = 0, rel_eps = 1e-3, verbose = TRUE, use_aprx = TRUE,
#'   heq = heq, heq_bounds = c(0, Inf), delta = .2, n_threads = 2L)
#' prof_out_nleq$confs # the confidence interval for the proportion
#'
#' # plot the log profile likelihood
#' plot(prof_out_nleq$xs, prof_out_nleq$p_log_Lik, pch = 16,
#'      xlab = "proportion of variance", ylab = "log profile likelihood")
#' abline(v = prof_out_nleq$confs, lty = 2)
#' }
#'
#' @importFrom alabama auglag
#' @export
pedmod_profile_nleq <- function(
  ptr, par, maxvls, minvls = -1L, alpha = .05, abs_eps,
  rel_eps, heq, heq_bounds = c(-Inf, Inf), delta, indices = NULL,
  maxvls_start = max(100L, as.integer(ceiling(maxvls / 5))),
  minvls_start = if(minvls < 0) minvls else minvls / 5,
  do_reorder = TRUE, use_aprx = FALSE, n_threads = 1L, cluster_weights = NULL,
  method = 0L, seed = 1L, verbose = FALSE, max_step = 15L,
  use_tilting = FALSE, vls_scales = NULL,
  control.outer = list(
    itmax = 100L, method = "BFGS", kkt2.check = FALSE, trace = FALSE),
  control.optim = list(fnscale = get_n_terms(ptr)),
  ...){
  # checks
  standardized <- FALSE
  stopifnot(
    !missing(par), !missing(maxvls), !missing(abs_eps),
    !missing(rel_eps),
    is.numeric(par),
    is.numeric(alpha), length(alpha) == 1, is.finite(alpha),
    alpha > 0, alpha < 1,
    is.function(heq), length(heq(par)) == 1, is.finite(heq(par)),
    length(heq_bounds) == 2, heq_bounds[1] < heq_bounds[2],
    heq_bounds[1] < heq(par), heq(par) < heq_bounds[2],
    length(delta) == 1, is.numeric(delta), is.finite(delta), delta > 0)
  if(!inherits(ptr, "pedigree_ll_terms_ptr") &
      !inherits(ptr, "pedigree_ll_terms_loadings_ptr"))
    .not_supported_for(ptr)

  # assign function to evaluate the log likelihood
  fn <- function(par, minv = minvls){
    set.seed(seed)
    eval_pedigree_ll(ptr = ptr, par = par, maxvls = maxvls, abs_eps = abs_eps,
                     rel_eps = rel_eps, indices = indices, minvls = minv,
                     do_reorder = do_reorder, use_aprx = use_aprx,
                     n_threads = n_threads, cluster_weights = cluster_weights,
                     method = method, use_tilting = use_tilting,
                     vls_scales = vls_scales)
  }
  optim_res <- list(par = par, value = -fn(par))

  if(verbose)
    message(sprintf("The estimate of the standard error of the log likelihood is %.8f. Preferably this should be below 0.001",
                    attr(optim_res$value, "std")))

  heq_shift <- heq(par)
  shift_heq_bounds <- heq_bounds - heq_shift
  heq_pass <- function(par)
    heq(par) - heq_shift

  # assign function to do the model fitting
  chi_val <- qchisq(1 - alpha, 1)
  crit_value <- -optim_res$value - chi_val / 2

  wrap_optim <- function(x, optim_obj, dir, lb = NULL, ub = NULL){
    if(verbose){
      new_val <- -optim_obj$value
      message(sprintf("LogLike: %.4f at %16f", new_val, x + heq_shift),
              appendLF = FALSE)
      if(!is.null(ub) && !is.null(lb)){
        if(new_val < crit_value)
          lb$value <- new_val
        else
          ub$value <- new_val
        message(sprintf(". Lb, target, ub: %.4f, %.4f, %.4f",
                        lb$value, crit_value, ub$value))

      } else
        message() # line feed
    }
    list(x = x, value = -optim_obj$value, optim = optim_obj,
         z_val = sign(dir) * sqrt((optim_obj$value - optim_res$value) * 2))
  }

  do_fit <- function(x, dir, par, lb = NULL, ub = NULL){
    # define the function and gradient to minimize
    fn <- function(par, minvls, maxvls){
      if(!is.null(seed))
        set.seed(seed)

      out <- try(-eval_pedigree_ll(
        ptr = ptr, par = par, maxvls = maxvls, rel_eps = rel_eps,
        indices = indices, minvls = minvls, abs_eps = abs_eps,
        do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
        cluster_weights = cluster_weights, standardized = standardized,
        method = method, use_tilting = use_tilting, vls_scales = vls_scales),
        silent = TRUE)
      if(inherits(out, "try-error"))
        return(NA_real_)
      out
    }
    gr <- function(par, minvls, maxvls){
      if(!is.null(seed))
        set.seed(seed)
      out <- try(-eval_pedigree_grad(
        ptr = ptr, par = par, maxvls = maxvls, rel_eps = rel_eps,
        indices = indices, minvls = minvls, abs_eps = abs_eps,
        do_reorder = do_reorder, use_aprx = use_aprx, n_threads = n_threads,
        cluster_weights = cluster_weights, standardized = standardized,
        method = method, use_tilting = use_tilting, vls_scales = vls_scales),
        silent = TRUE)
      if(inherits(out, "try-error"))
        return(rep(NA_real_, length(par) - 1L))

      out
    }

    opt_func <- function(par, minvls, maxvls){
      heq_pass_inner <- function(par, ...)
        heq_pass(par) - x
      auglag(
        par = par, fn = fn, gr = gr, heq = heq_pass_inner,
        control.outer = control.outer, minvls = minvls, maxvls = maxvls,
        control.optim = control.optim, ...)
    }

    if(maxvls > maxvls_start){
      opt_quick <- opt_func(par, minvls = minvls_start, maxvls = maxvls_start)

      par <- opt_quick$par
      opt_quick <- wrap_optim(x, opt_quick, dir, lb = lb, ub = ub)

    } else
      opt_quick <- NULL

    opt_out <- opt_func(par, minvls = minvls, maxvls = maxvls)

    structure(wrap_optim(x, opt_out, dir, lb = lb, ub = ub),
              opt_quick = opt_quick)
  }

  # find points on the profile likelihood curve in either direction
  get_points <- function(dir){
    dir <- sign(dir)
    if(verbose)
      message(sprintf(
        "\nFinding the %s limit of the profile likelihood curve",
        if(dir < 0) "lower" else "upper"))

    step <- 0L
    out <- vector("list", max_step)
    prev <- -optim_res$value
    did_fail <- FALSE

    # find the boundary
    old_step <- 0
    boundary_eps <- if(all(!is.finite(shift_heq_bounds)))
      0 else if(all(is.finite(shift_heq_bounds)))
        1e-4 * diff(shift_heq_bounds) else if(!is.finite(shift_heq_bounds[1]))
          shift_heq_bounds[2] - 1e-4 * abs(shift_heq_bounds[2]) else
            shift_heq_bounds[1] + 1e-4 * abs(shift_heq_bounds[1])
    prev_par <- par

    while(prev > crit_value && (step <- step + 1L) <= max_step){
      new_step <-
        max(shift_heq_bounds[1] + boundary_eps,
            min(shift_heq_bounds[2] - boundary_eps,
                dir *  2^(step - 1) * delta))
      did_fail <- new_step == old_step
      if(!did_fail)
        out[[step]] <- do_fit(new_step, dir = dir, par = prev_par)

      if(did_fail || out[[step]]$value > prev){
        warning("Log likelihood did not decrease or a boundary was hit. In the former case, 'optim_res' is not an optimum or the precision needs to be increased")
        did_fail <- TRUE
        break
      }

      old_step <- new_step
      prev <- out[[step]]$value
    }

    # perform search
    .report_failed <- function()
      if(verbose && step > max_step)
        warning(sprintf(
          "Failed to find the appropiate point in %d steps", max_step))

    .report_failed()

    if(did_fail || step > max_step)
      return(out[sapply(out, length) > 0])

    ub <- wrap_optim(0, optim_res, 1)
    lb <- out[[step]]

    while(ub$value - lb$value > chi_val / 6 && (step <- step + 1L) <= max_step){
      # compute the next value
      xs <- c(unlist(sapply(out, `[[`, "x"))    , 0)
      ys <- c(unlist(sapply(out, `[[`, "value")), -optim_res$value)
      sp <- splinefun(ys, xs, method = "monoH.FC")
      y <- seq(lb$value, ub$value, length.out = 20)
      x <- sp(y)
      next_val <- approx(y, x, xout = crit_value)$y
      if(abs(next_val - ub$x) > abs(next_val - lb$x))
        next_val <- 8 / 9 * next_val + ub$x / 9
      else
        next_val <- 8 / 9 * next_val + lb$x / 9

      par_use <- if(abs(lb$x - next_val) < abs(ub$x - next_val))
        lb$optim$par else ub$optim$par

      out[[step]] <- do_fit(
        next_val, dir = dir, par = par_use, lb = lb, ub = ub)

      if(out[[step]]$value > ub$value || out[[step]]$value < lb$value){
        warning("Log likelihood does not seem monotonic. Likely the precision needs to be increased")
        break
      }

      if(out[[step]]$value < crit_value)
        lb <- out[[step]]
      else
        ub <- out[[step]]
    }

    .report_failed()
    out <- out[sapply(out, length) > 0]
    out[order(sapply(out, `[[`, "x"))]
  }

  res_up   <- get_points( 1)
  res_down <- get_points(-1)
  out <- c(
    res_down, list(wrap_optim(0, optim_res, dir = 0)), res_up)

  # compute the confidence interval
  xs  <- sapply(out, `[[`, "x") + heq_shift
  zs  <- sapply(out, `[[`, "z_val")
  pls <- sapply(out, `[[`, "value")

  confs <- comp_confs(xs, zs, alpha)

  list(confs = confs, xs = xs, p_log_Lik = pls, data = out)
}
