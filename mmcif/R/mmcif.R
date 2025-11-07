.ghq_data_default_if_null <- function(ghq_data){
  if(is.null(ghq_data))
    ghq_data <- list(
      node = c(-2.02018287045609, -0.958572464613819, 2.73349801485409e-17, 0.958572464613819, 2.02018287045609),
      weight = c(0.0199532420590459,  0.393619323152241, 0.945308720482941, 0.393619323152241, 0.0199532420590459))
  ghq_data
}

.check_is_log_chol <- function(is_log_chol)
  stopifnot(is.logical(is_log_chol), length(is_log_chol) == 1,
            is_log_chol %in% c(FALSE, TRUE))

time_trans <- function(x, max_time){
  ifelse(x < max_time,
         suppressWarnings(
           atanh((x - max_time / 2) / (max_time / 2))),
         NA)
}

d_time_trans <- function(x, max_time) {
  ifelse(x < max_time,
         suppressWarnings({
           outer <- (x - max_time / 2) / (max_time / 2)
           1/((1 - outer^2) * (max_time / 2))
         }),
         NA)
}

time_expansion <- function(
    x, cause, max_time, splines, n_strata, which_strata = NULL){
  x <- time_trans(x, max_time)
  out <- splines[[cause]]$expansion(x, ders = 0L)

  if(!is.null(which_strata)){
    which_strata <- as.integer(which_strata)
    stopifnot(all(which_strata %in% 1:n_strata),
              length(which_strata) %in% c(1L, NROW(out)))
    out_expanded <- matrix(0., NROW(out), NCOL(out) * n_strata)
    for(i in 1:n_strata - 1L){
      strata_match <- which_strata == i + 1L
      out_expanded[strata_match, 1:NCOL(out) + i * NCOL(out)] <-
        out[strata_match, ]
    }
    out <- out_expanded
  }

  out
}

d_time_expansion <- function(
    x, cause, max_time, splines, n_strata, which_strata = NULL){
  z <- time_trans(x, max_time)
  d_x <- d_time_trans(x, max_time)
  out <- splines[[cause]]$expansion(z, ders = 1L) * d_x

  if(!is.null(which_strata)){
    which_strata <- as.integer(which_strata)
    stopifnot(all(which_strata %in% 1:n_strata),
              length(which_strata) %in% c(1L, NROW(out)))
    out_expanded <- matrix(0., NROW(out), NCOL(out) * n_strata)
    for(i in 1:n_strata - 1L){
      strata_match <- which_strata == i + 1L
      out_expanded[strata_match, 1:NCOL(out) + i * NCOL(out)] <-
        out[strata_match, ]
    }
    out <- out_expanded
  }

  out
}

eval_trajectory_covs <- function(x, FUN, covs, which_strata, n_causes){
  varying_covs <- lapply(1:n_causes, FUN, x = x, which_strata = which_strata)
  do.call(cbind, lapply(varying_covs, cbind, covs))
}

#' Sets up an Object to Compute the Log Composite Likelihood
#'
#' Sets up the R and C++ objects that are needed to evaluate the log composite
#' likelihood. This reduces to a log likelihood when only clusters of size one
#' or two are used.
#'
#' @param formula \code{formula} for covariates in the risk and trajectories.
#' @param data \code{data.frame} with the covariate and outcome information.
#' @param cause an integer vector with the cause of each outcome. If there are
#' \code{n_causes} of outcome, then the vector should have values in
#' \code{1:(n_causes + 1)} with \code{n_causes + 1} indicating censoring.
#' @param time a numeric vector with the observed times.
#' @param cluster_id an integer vector with the cluster id of each individual.
#' @param max_time the maximum time after which there are no observed events. It
#' is denoted by \eqn{\tau} in the original article (Cederkvist et al., 2019).
#' @param spline_df degrees of freedom to use for each spline in the
#' cumulative incidence functions.
#' @param left_trunc numeric vector with left-truncation times. \code{NULL}
#' implies that there are not any individuals with left-truncation.
#' @param ghq_data the default Gauss-Hermite quadrature nodes and weights to
#' use. It should be a list with two elements called \code{"node"}
#' and \code{"weight"}. A default is provided if \code{NULL} is passed.
#' @param strata an integer vector or a factor vector with the strata of each
#' individual. \code{NULL} implies that there are no strata.
#' @param knots A list of lists with knots for the splines. The inner lists
#'  needs to have elements called \code{"knots"} and
#' \code{"boundary_knots"} which are passed to a function like \code{\link{ns}}.
#' \code{NULL} yields defaults based on the quantiles of the observed event
#' times. Note that the knots needs to be on the
#' \code{atanh((time - max_time / 2) / (max_time / 2))} scale.
#' @param boundary_quantiles two dimensional numerical vector with boundary
#' quantile probabilities after which the natural cubic splines for the time
#' transformations are restricted to be linear. Only relevant if \code{knots} is
#' not \code{NULL}.
#'
#' @seealso
#' \code{\link{mmcif_fit}}, \code{\link{mmcif_start_values}} and
#' \code{\link{mmcif_sandwich}}.
#'
#' @references
#' Cederkvist, L., Holst, K. K., Andersen, K. K., &
#' Scheike, T. H. (2019).
#' \emph{Modeling the cumulative incidence function of multivariate competing
#' risks data allowing for within-cluster dependence of risk and timing}.
#' Biostatistics, Apr 1, 20(2), 199-217.
#'
#' @return
#' An object of class mmcif which is needed for the other functions in the
#' package.
#'
#' @examples
#' if(require(mets)){
#'   # prepare the data
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # randomly sub-sample
#'   set.seed(1)
#'   prt_use <- subset(
#'     prt_use, id %in% sample(unique(id), length(unique(id)) %/% 10L))
#'
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country)
#' }
#'
#' @importFrom stats model.frame model.matrix terms ave
#' @export
mmcif_data <- function(formula, data, cause, time, cluster_id, max_time,
                       spline_df = 3L, left_trunc = NULL, ghq_data = NULL,
                       strata = NULL, knots = NULL,
                       boundary_quantiles = c(.025, .975)){
  stopifnot(inherits(formula, "formula"),
            is.data.frame(data),
            length(spline_df) == 1, is.finite(spline_df), spline_df > 0)

  # setup the design matrix
  mf <- model.frame(formula, data)
  mf_terms <- terms(mf)
  covs_risk <- model.matrix(mf_terms, mf)

  cause <- eval(substitute(cause), data, parent.frame())
  time_observed <- eval(substitute(time), data, parent.frame())
  cluster_id <- eval(substitute(cluster_id), data, parent.frame())

  strata <- eval(substitute(strata), data, parent.frame())
  if(!is.null(strata)){
    if(is.integer(strata))
      strata <- factor(strata)
    n_strata <- length(unique(strata))

  } else
    n_strata <- 1L

  left_trunc <- eval(substitute(left_trunc), data, parent.frame())
  if(is.null(left_trunc))
    left_trunc <- numeric(length(time_observed))

  n_causes <- length(unique(cause)) - 1L

  ghq_data <- .ghq_data_default_if_null(ghq_data)

  stopifnot(
    length(cause) > 0L,
    NROW(covs_risk) == length(cause),
    length(time_observed) == length(cause),
    all(is.finite(time_observed) & time_observed > 0),
    length(cluster_id) == length(cluster_id),
    n_causes > 0L, all(1:(n_causes + 1L) %in% cause),
    is.numeric(max_time), length(max_time) == 1, is.finite(max_time),
    max_time > 0,
    max(time_observed[cause %in% 1:n_causes]) < max_time,
    length(left_trunc) == length(cause),
    all(is.finite(left_trunc) & left_trunc >= 0),
    all(time_observed > left_trunc),
    is.list(ghq_data), all(c("node", "weight") %in% names(ghq_data)),
    length(ghq_data$node) == length(ghq_data$weight),
    length(ghq_data$node) > 0,
    all(sapply(ghq_data, function(x) all(is.finite(x)))),
    (is.factor(strata) && length(strata) == length(cause) && n_strata > 1) ||
      is.null(strata))

  # check the knots
  if(!is.null(knots)){
    n_knots <- length(knots[[1]]$knots)

    stopifnot(
      is.list(knots),
      all(sapply(knots, function(x)
        is.numeric(x$boundary_knots) &&
          length(x$boundary_knots) == 2 &&
          all(is.finite(x$boundary_knots)) &&
          diff(x$boundary_knots) > 0)),
      all(sapply(knots, function(x)
        all(
          is.numeric(x$knots),
          all(is.finite(x$knots)),
          length(x$knots) == n_knots,
          all(x$boundary_knots[1] <= x$knots & x$knots <= x$boundary_knots[2])
          ))))

    spline_df <- n_knots + 1L

  } else
    knots <- replicate(n_causes, list(), simplify = FALSE)

  # add the time transformations
  time_observed_trans <- time_trans(time_observed, max_time)

  is_observed <- cause %in% 1:n_causes
  splines <- Map(
    function(time_observed_trans, knots)
      monotone_ns(
        time_observed_trans, df = spline_df,
        knots = knots$knots, boundary_knots = knots$boundary_knots,
        boundary_quantiles = boundary_quantiles),
    split(time_observed_trans[is_observed], cause[is_observed]),
    knots = knots)

  mmcif_time_expansion <- time_expansion
  time_expansion <- function(x, cause, which_strata = NULL){
    mmcif_time_expansion(x, cause, max_time, splines, n_strata, which_strata)
  }
  mmcif_d_time_expansion <- d_time_expansion
  d_time_expansion <- function(x, cause, which_strata = NULL){
    mmcif_d_time_expansion(
      x, cause, max_time, splines, n_strata, which_strata)
  }

  covs_trajectory <- eval_trajectory_covs(
    time_observed, time_expansion, covs_risk, strata, n_causes)

  d_covs_trajectory <- eval_trajectory_covs(
    time_observed, d_time_expansion,
    matrix(0, NROW(covs_risk), NCOL(covs_risk)), strata, n_causes)

  has_finite_trajectory_prob <- time_observed < max_time

  # compute the covariates for the left truncation
  covs_trajectory_delayed <-
    matrix(NaN, NROW(covs_trajectory), NCOL(covs_trajectory))
  has_delayed_entry <- which(left_trunc > 0)

  covs_trajectory_delayed[has_delayed_entry, ] <-
    eval_trajectory_covs(
      left_trunc[has_delayed_entry], time_expansion,
      covs_risk[has_delayed_entry, ],
      if(!is.null(strata)) strata[has_delayed_entry] else strata, n_causes)

  # find all permutation of indices in each cluster
  cluster_length <- ave(cluster_id, cluster_id, FUN = length)

  row_id <- seq_len(length(cluster_id))
  pair_indices_res <- create_pair_indices(
    cluster_id[cluster_length > 1], row_id[cluster_length > 1])
  pair_indices <- pair_indices_res$pair_indices
  pair_cluster_id <- pair_indices_res$pair_cluster_id

  singletons <- row_id[cluster_length < 2]

  # create the C++ object
  # TODO: this function should be exposed to advanced users
  comp_obj <- mmcif_data_holder(
    covs_risk = t(covs_risk), covs_trajectory = t(covs_trajectory),
    d_covs_trajectory = t(d_covs_trajectory),
    has_finite_trajectory_prob = has_finite_trajectory_prob,
    cause = cause - 1L, n_causes = n_causes, pair_indices = pair_indices - 1L,
    singletons = singletons - 1L,
    covs_trajectory_delayed = t(covs_trajectory_delayed),
    pair_cluster_id = pair_cluster_id)

  # create an object to do index the parameters
  n_coef_risk <- NCOL(covs_risk) * n_causes
  n_coef_trajectory <- (spline_df * n_strata + NCOL(covs_risk)) * n_causes

  coef_trajectory_time <- matrix(
    seq_len(n_coef_trajectory) + n_coef_risk,
    spline_df * n_strata + NCOL(covs_risk))
  coef_trajectory_time <- coef_trajectory_time[seq_len(spline_df * n_strata), ]

  idx_coef_trajectory <- seq_len(n_coef_trajectory) + n_coef_risk

  vcov_dim <- 2L * n_causes
  vcov_full <- max(idx_coef_trajectory) + seq_len(vcov_dim^2)
  vcov_upper <- max(idx_coef_trajectory) +
    seq_len((vcov_dim * (vcov_dim + 1L)) %/% 2L)

  indices <- list(
    coef_risk = seq_len(n_coef_risk),
    coef_trajectory = idx_coef_trajectory,
    coef_trajectory_time = c(coef_trajectory_time),
    vcov_full = vcov_full,
    vcov_upper = vcov_upper,
    n_causes = n_causes,
    n_par = max(vcov_full),
    n_par_upper_tri = max(vcov_upper),
    n_par_wo_vcov = max(idx_coef_trajectory),
    n_strata = n_strata)

  # create the constrain matrices
  n_constraints_per_spline <- NROW(splines[[1]]$constraints)
  constraints <- matrix(0., n_constraints_per_spline * n_causes * n_strata,
                        indices$n_par_wo_vcov)

  for(i in 1:n_causes){
    cols_idx <-
      matrix(matrix(indices$coef_trajectory_time, spline_df * n_strata)[, i],
             spline_df)
    for(j in 1:n_strata){
      rows <- 1:n_constraints_per_spline +
        (n_strata * (i - 1L) + j - 1L) * n_constraints_per_spline
      cols <-
      constraints[rows, cols_idx[, j]] <- splines[[i]]$constraints
    }
  }

  constraints <- list(
    wo_vcov = constraints,
    vcov_full =
      cbind(constraints, matrix(0., NROW(constraints), vcov_dim^2)),
    vcov_upper =
      cbind(constraints, matrix(0., NROW(constraints),
                                (vcov_dim * (vcov_dim + 1L)) %/% 2L)))

  # create the parameter names variable
  if(!is.null(strata)){
    n_varying <- NCOL(covs_trajectory) %/% n_causes - NCOL(covs_risk)
    n_varying <- n_varying %/% n_strata
    fixed_covs_names <- colnames(covs_risk)
    varying_covs_names <- sprintf("spline%d", 1:n_varying)
    varying_covs_names <-
      c(outer(varying_covs_names, sprintf("strata%s", levels(strata)),
              function(x, y) sprintf("%s:%s", y, x)))

  } else {
    n_varying <- NCOL(covs_trajectory) %/% n_causes - NCOL(covs_risk)
    fixed_covs_names <- colnames(covs_risk)
    varying_covs_names <- sprintf("spline%d", 1:n_varying)

  }

  trajectory_names <- outer(
    c(varying_covs_names, sprintf("traject:%s", fixed_covs_names)),
      1:n_causes, function(x, y) sprintf("cause%d:%s", y, x))
  colnames(covs_trajectory) <- colnames(covs_trajectory_delayed) <-
    colnames(d_covs_trajectory) <- c(trajectory_names)

  param_names_upper <- character(indices$n_par_upper_tri)
  param_names_upper[indices$coef_trajectory] <- c(trajectory_names)
  param_names_upper[indices$coef_risk] <-
    c(outer(fixed_covs_names, 1:n_causes,
            function(x, y) sprintf("cause%d:risk:%s", y, x)))

  vcov_full_names <-
    c(sprintf("risk%d", 1:n_causes), sprintf("traject%d", 1:n_causes))
  vcov_full_names <-
    outer(vcov_full_names, vcov_full_names, function(x, y) paste0(x, ":", y))
  vcov_full_names[] <- sprintf("vcov:%s", vcov_full_names)

  param_names_upper[indices$vcov_upper] <-
    vcov_full_names[upper.tri(vcov_full_names, TRUE)]

  param_names <- character(indices$n_par)
  param_names[-indices$vcov_full] <- param_names_upper[-indices$vcov_upper]
  param_names[indices$vcov_full] <- c(vcov_full_names)

  param_names <- list(full = param_names, upper = param_names_upper)

  colnames(constraints$wo_vcov) <-
    param_names$upper[1:indices$n_par_wo_vcov]
  colnames(constraints$vcov_upper) <- param_names$upper
  colnames(constraints$vcov_full) <- param_names$full

  # clean up
  rm(list = setdiff(
    ls(),
    c("comp_obj", "pair_indices", "singletons", "covs_risk",
      "covs_trajectory", "time_observed", "cause", "time_trans",
      "d_time_trans", "max_time", "indices", "splines", "d_covs_trajectory",
      "constraints", "covs_trajectory_delayed", "time_expansion",
      "d_time_expansion", "pair_cluster_id", "ghq_data",
      "param_names", "n_strata", "strata", "mf_terms",
      "mmcif_time_expansion", "mmcif_d_time_expansion")))

  structure(
    list(comp_obj = comp_obj, pair_indices = pair_indices,
         singletons = singletons, covs_risk = covs_risk,
         covs_trajectory = covs_trajectory, time_observed = time_observed,
         cause = cause, time_trans = function(x) time_trans(x, max_time),
         d_time_trans = function(x) d_time_trans(x, max_time),
         time_expansion = time_expansion, d_time_expansion = d_time_expansion,
         max_time = max_time, indices = indices, splines = splines,
         d_covs_trajectory = d_covs_trajectory, constraints = constraints,
         covs_trajectory_delayed = covs_trajectory_delayed,
         pair_cluster_id = pair_cluster_id, ghq_data = ghq_data,
         param_names = param_names, strata = strata, terms = mf_terms),
    class = "mmcif")
}

.check_n_threads <- function(n_threads){
  stopifnot(length(n_threads) == 1, is.finite(n_threads), n_threads > 0)
}

#' Evaluates the Log Composite Likelihood and its Gradient
#'
#' Evaluates the log composite likelihood and its gradient using adaptive
#' Gauss-Hermite quadrature.
#'
#' @param object an object from \code{\link{mmcif_data}}.
#' @param n_threads the number of threads to use.
#' @param par numeric vector with parameters. This is either using a log
#' Cholesky decomposition for the covariance matrix or the covariance matrix.
#' @param ghq_data the Gauss-Hermite quadrature nodes and weights to
#' use. It should be a list with two elements called \code{"node"}
#' and \code{"weight"}. A default is provided if \code{NULL} is passed.
#' @param is_log_chol logical for whether a log Cholesky decomposition is used
#' for the covariance matrix or the full covariance matrix.
#'
#' @return
#' A numeric vector with either the log composite likelihood or the gradient of
#' it.
#'
#' @examples
#' if(require(mets)){
#'   # prepare the data
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # randomly sub-sample
#'   set.seed(1)
#'   prt_use <- subset(
#'     prt_use, id %in% sample(unique(id), length(unique(id)) %/% 10L))
#'
#'   n_threads <- 2L
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country)
#'
#'   # get the staring values
#'   start_vals <- mmcif_start_values(mmcif_obj, n_threads = n_threads)
#'
#'   # compute the log composite likelihood and the gradient at the starting
#'   # values
#'   mmcif_logLik(
#'     mmcif_obj, start_vals$upper, is_log_chol = TRUE, n_threads = n_threads) |>
#'     print()
#'   mmcif_logLik_grad(
#'     mmcif_obj, start_vals$upper, is_log_chol = TRUE, n_threads = n_threads) |>
#'     print()
#' }
#'
#' @importFrom utils tail
#' @export
mmcif_logLik <- function(
  object, par, ghq_data = object$ghq_data, n_threads = 1L,
  is_log_chol = FALSE){
  stopifnot(inherits(object, "mmcif"))
  .check_n_threads(n_threads)
  .check_is_log_chol(is_log_chol)

  if(is_log_chol){
    n_causes <- object$indices$n_causes
    n_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
    par <- c(head(par, -n_vcov), log_chol_inv(tail(par, n_vcov)))
  }

  mmcif_logLik_cpp(
    object$comp_obj, par = par, ghq_data = ghq_data, n_threads = n_threads)
}

#' @rdname mmcif_logLik
#' @export
mmcif_logLik_grad <- function(
  object, par, ghq_data = object$ghq_data, n_threads = 1L,
  is_log_chol = FALSE){
  stopifnot(inherits(object, "mmcif"))
  .check_n_threads(n_threads)
  .check_is_log_chol(is_log_chol)


  if(is_log_chol){
    par_org <- par

    n_causes <- object$indices$n_causes
    n_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
    vcov <- log_chol_inv(tail(par, n_vcov))
    par <- c(head(par, -n_vcov), vcov)
  }

  gr <- mmcif_logLik_grad_cpp(
    object$comp_obj, par = par, ghq_data = ghq_data, n_threads = n_threads)
  if(!is_log_chol)
    return(gr)

  # back propagate the gradients w.r.t. the random effects
  d_vcov <- matrix(tail(gr, 4L * n_causes * n_causes), 2L * n_causes)
  C <- chol(vcov)
  d_vcov <- 2 * C %*% d_vcov
  diag(d_vcov) <- diag(d_vcov) * diag(C)

  c(head(gr, -4L * n_causes * n_causes), d_vcov[upper.tri(d_vcov, TRUE)])
}

#' Finds Staring Values
#'
#' Fast heuristic for finding starting values for the mixed cumulative incidence
#' functions model.
#'
#' @inheritParams mmcif_logLik
#' @param vcov_start starting value for the covariance matrix of the random
#' effects. \code{NULL} yields the identity matrix.
#'
#' @return
#' A list with
#' \itemize{
#'   \item an element called \code{"full"} with the starting value where the
#'   last components are the covariance matrix.
#'   \item an element called \code{"upper"} the staring values where the
#'   covariance matrix is stored as a log Cholesky decomposition. This is used
#'   e.g. for optimization with \code{\link{mmcif_fit}}.
#' }
#'
#' @examples
#' if(require(mets)){
#'   # prepare the data
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # randomly sub-sample
#'   set.seed(1)
#'   prt_use <- subset(
#'     prt_use, id %in% sample(unique(id), length(unique(id)) %/% 10L))
#'
#'   n_threads <- 2L
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country)
#'
#'   # get the staring values
#'   start_vals <- mmcif_start_values(mmcif_obj, n_threads = n_threads)
#'
#'   # the starting values
#'   print(start_vals)
#' }
#'
#' @importFrom stats lm.fit constrOptim na.omit sd
#' @export
mmcif_start_values <- function(object, n_threads = 1L, vcov_start = NULL){
  stopifnot(inherits(object, "mmcif"))
  .check_n_threads(n_threads)

  # find intercepts and slopes in a simple model
  n_causes <- object$indices$n_causes
  n_strata <- object$indices$n_strata
  cause <- object$cause
  time_observed <- object$time_observed

  is_observed <- cause %in% 1:n_causes

  slope_n_time <- tapply(
    time_observed[is_observed], cause[is_observed],
    function(time_observed){
      time_observed_trans <- object$time_trans(time_observed)
      time_sd <- sd(time_observed_trans)
      c(slope = -1 / time_sd, intercept = mean(time_observed_trans) / time_sd)
    })

  # find the linear combination which gives a line and a slope
  comb_slope <- sapply(object$splines, function(spline){
    boundary_knots <- spline$boundary_knots
    pts <- seq(boundary_knots[1], boundary_knots[2], length.out = 1000)
    lm.fit(cbind(1, spline$expansion(pts)), pts)$coef
  })
  if(n_strata > 1)
    comb_slope <- apply(
      comb_slope, 2, function(x) c(x[1], rep(x[-1], n_strata)))

  n_cov_traject <- NCOL(object$covs_trajectory) %/% n_causes
  traject_factor <- gl(n_causes, n_cov_traject)

  comb_line <-
    tapply(1:NCOL(object$covs_trajectory), traject_factor, function(indices){
      design_mat <- na.omit(object$covs_trajectory[, indices])
      lm.fit(design_mat, rep(1, NROW(design_mat)))$coef
    }, simplify = FALSE)
  comb_line <- do.call(cbind, comb_line)

  # start the optimization. Start only with the trajectories
  par <- numeric(object$indices$n_par_wo_vcov)
  coef_trajectory_time <- object$indices$coef_trajectory_time
  coef_trajectory <- object$indices$coef_trajectory

  par[coef_trajectory_time] <-
    comb_slope[-1, ] * rep(sapply(slope_n_time, `[[`, "slope"),
                           each = NROW(comb_slope) - 1)
  par[coef_trajectory] <- par[coef_trajectory] +
    c(comb_line *
        rep(sapply(slope_n_time, `[[`, "intercept") +
              comb_slope[1] * sapply(slope_n_time, `[[`, "slope"),
            each = NROW(comb_line)))

  fn <- function(x, with_risk){
    if(!with_risk)
      par[coef_trajectory] <- x
    else
      par <- x
    -mcif_logLik(data_ptr = object$comp_obj, par = par, n_threads = n_threads,
                 with_risk = with_risk)
  }
  gr <- function(x, with_risk){
    if(!with_risk)
      par[coef_trajectory] <- x
    else
      par <- x
    grs <- -mcif_logLik_grad(data_ptr = object$comp_obj, par = par,
                             n_threads = n_threads, with_risk = with_risk)
    if(with_risk) grs else grs[coef_trajectory]
  }

  constraints <- object$constraints$wo_vcov
  opt_trajectory <-
    constrOptim(
      par[coef_trajectory], fn, gr, method = "BFGS",
      ui = constraints[, coef_trajectory], ci = rep(1e-8, NROW(constraints)),
      control = list(maxit = 1000L), with_risk = FALSE)
  if(opt_trajectory$convergence != 0)
    warning(sprintf("convergence code is %d", opt_trajectory$convergence))

  par[coef_trajectory] <- opt_trajectory$par
  opt <- constrOptim(
    par, fn, gr, method = "BFGS", ui = constraints,
    ci = rep(1e-8, NROW(constraints)), control = list(maxit = 1000L),
    with_risk = TRUE)
  if(opt$convergence != 0)
    warning(sprintf("convergence code is %d", opt$convergence))

  if(is.null(vcov_start))
    vcov_start <- diag(2L * n_causes)
  stopifnot(is.matrix(vcov_start), all(dim(vcov_start) == 2L * n_causes),
            all(is.finite(vcov_start)))

  structure(
    list(full = setNames(
           c(opt$par, c(vcov_start)), object$param_names$full),
         upper = setNames(
           c(opt$par, log_chol(vcov_start)), object$param_names$upper)),
    logLik = -opt$value)
}


#' Fits a Mixed Competing Risk Model
#'
#' Fits mixed cumulative incidence functions model by maximizing the log
#' composite likelihood function.
#'
#' @inheritParams mmcif_logLik
#' @param par numeric vector with parameters. This is using a log
#' Cholesky decomposition for the covariance matrix.
#' @param ghq_data the Gauss-Hermite quadrature nodes and weights to use.
#' It should be a list with two elements called \code{"node"} and \code{"weight"}.
#' The argument can also be a list with lists with different sets of quadrature
#' nodes. In this case, fits are successively made using the previous fit as the
#' starting value. This may
#' reduce the computation time by starting with fewer quadrature nodes.
#' @param control.outer,control.optim,... arguments passed to
#' \code{\link[alabama]{auglag}}.
#'
#' @seealso
#' \code{\link{mmcif_data}}, \code{\link{mmcif_start_values}} and
#' \code{\link{mmcif_sandwich}}.
#'
#' @references
#' Cederkvist, L., Holst, K. K., Andersen, K. K., &
#' Scheike, T. H. (2019).
#' \emph{Modeling the cumulative incidence function of multivariate competing
#' risks data allowing for within-cluster dependence of risk and timing}.
#' Biostatistics, Apr 1, 20(2), 199-217.
#'
#' @return
#' The output from \code{\link{auglag}}.
#'
#' @examples
#' \donttest{if(require(mets)){
#'   # prepare the data
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # randomly sub-sample
#'   set.seed(1)
#'   prt_use <- subset(
#'     prt_use, id %in% sample(unique(id), length(unique(id)) %/% 10L))
#'
#'   n_threads <- 2L
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country)
#'
#'   # get the staring values
#'   start_vals <- mmcif_start_values(mmcif_obj, n_threads = n_threads)
#'
#'   # estimate the parameters
#'   ests <- mmcif_fit(start_vals$upper, mmcif_obj, n_threads = n_threads)
#'
#'   # show the estimated covariance matrix of the random effects
#'   tail(ests$par, 10L) |> log_chol_inv() |> print()
#'
#'   # gradient is ~ zero
#'   mmcif_logLik_grad(
#'     mmcif_obj, ests$par, is_log_chol = TRUE, n_threads = n_threads) |>
#'     print()
#' }}
#'
#' @importFrom alabama auglag
#' @importFrom stats setNames
#' @export
mmcif_fit <- function(
  par, object, n_threads = 1L,
  control.outer = list(itmax = 100L, method = "nlminb", kkt2.check = FALSE,
                       trace = FALSE),
  control.optim = list(eval.max = 10000L, iter.max = 10000L),
  ghq_data = object$ghq_data, ...){
  stopifnot(inherits(object, "mmcif"))

  if(is.null(ghq_data$node)){
    stopifnot(is.list(ghq_data), length(ghq_data) > 0)
    for(nodes in ghq_data)
      stopifnot(!is.null(nodes$node), !is.null(nodes$weight))

    n_ghq_data <- length(ghq_data)
    out <- vector("list", n_ghq_data)
    cl <- match.call()
    cl$ghq_data <- ghq_data[[1]]
    out[[1]] <- eval(cl, parent.frame())

    for(i in seq_len(n_ghq_data - 1L) + 1L){
      cl$par <- out[[i - 1L]]$par
      cl$ghq_data <- ghq_data[[i]]
      out[[i]] <- eval(cl, parent.frame())
    }

    return(setNames(out, names(n_ghq_data)))
  }

  constraints <- object$constraints$vcov_upper

  fn <- function(par){
    out <- try(mmcif_logLik(
      object, par, n_threads = n_threads, is_log_chol = TRUE,
      ghq_data = ghq_data), silent = TRUE)
    if(inherits(out, "try-error"))
      return(NA_real_)
    -out
  }
  gr <- function(par) {
    out <- try(mmcif_logLik_grad(
      object, par, n_threads = n_threads, is_log_chol = TRUE,
      ghq_data = ghq_data), silent = TRUE)
    if(inherits(out, "try-error"))
      return(rep(NA_real_, length(par)))
    -out
  }
  hin <- function(par) drop(constraints %*% par)
  hinjac <- function(...) constraints

  out <- auglag(
    par = par, fn = fn, gr = gr, hin = hin, hin.jac = hinjac,
    control.outer = control.outer, control.optim = control.optim, ...)

  out$par <- setNames(out$par, object$param_names$upper)
  out
}

#' Computes the Sandwich Estimator
#'
#' Computes the sandwich estimator of the covariance matrix. The parameter that
#' is passed is using the log Cholesky decomposition. The Hessian is computed
#' using numerical differentiation with Richardson extrapolation to refine the
#' estimate.
#'
#' @inheritParams mmcif_logLik
#' @param par numeric vector with the parameters to compute the sandwich
#' estimator at.
#' @param eps determines the step size in the numerical differentiation using
#'  \code{max(sqrt(.Machine$double.eps), |par[i]| * eps)}
#'  for each parameter \code{i}.
#' @param scale scaling factor in the Richardson extrapolation. Each step is
#' smaller by a factor \code{scale}.
#' @param tol relative convergence criteria in the extrapolation given
#' by \code{max(tol, |g[i]| * tol)} with \code{g} being the gradient and for
#' each parameter \code{i}.
#' @param order maximum number of iteration of the Richardson extrapolation.
#'
#' @seealso
#' \code{\link{mmcif_fit}} and \code{\link{mmcif_data}}.
#'
#' @return
#' The sandwich estimator along attributes called
#'
#' \itemize{
#'  \item \code{"meat"} for the "meat" of the sandwich estimator.
#'  \item \code{"hessian"} for the Hessian of the log composite likelihood.
#'  \item \code{"res vcov"} which is the sandwich estimator where the
#' last elements are the upper triangle of the covariance matrix of the random
#' effects rather than the log Cholesky decomposition of the matrix.
#' }
#'
#' @references
#' Cederkvist, L., Holst, K. K., Andersen, K. K., &
#' Scheike, T. H. (2019).
#' \emph{Modeling the cumulative incidence function of multivariate competing
#' risks data allowing for within-cluster dependence of risk and timing}.
#' Biostatistics, Apr 1, 20(2), 199-217.
#'
#' @examples
#' \donttest{if(require(mets)){
#'   # prepare the data
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # randomly sub-sample
#'   set.seed(1)
#'   prt_use <- subset(
#'     prt_use, id %in% sample(unique(id), length(unique(id)) %/% 10L))
#'
#'   n_threads <- 2L
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country)
#'
#'   # get the staring values
#'   start_vals <- mmcif_start_values(mmcif_obj, n_threads = n_threads)
#'
#'   # estimate the parameters
#'   ests <- mmcif_fit(start_vals$upper, mmcif_obj, n_threads = n_threads)
#'
#'   # get the sandwich estimator
#'   vcov_est <- mmcif_sandwich(
#'     mmcif_obj, ests$par, n_threads = n_threads, order = 2L)
#'
#'   # show the parameter estimates along with the standard errors
#'   rbind(Estimate = ests$par,
#'         SE = sqrt(diag(vcov_est))) |>
#'     print()
#'
#'   # show the upper triangle of the covariance matrix and the SEs
#'   rbind(`Estimate (vcov)` = tail(ests$par, 10) |> log_chol_inv() |>
#'           (\(x) x[upper.tri(x, TRUE)])() ,
#'         SE = attr(vcov_est, "res vcov") |> diag() |> sqrt() |> tail(10)) |>
#'     print()
#' }}
#'
#' @export
mmcif_sandwich <- function(
  object, par, ghq_data = object$ghq_data, n_threads = 1L, eps = .01,
  scale = 2., tol = .00000001, order = 3L){
  stopifnot(inherits(object, "mmcif"))
  .check_n_threads(n_threads)
  ghq_data <- .ghq_data_default_if_null(ghq_data)

  cpp_res <- mmcif_sandwich_cpp(
    data_ptr = object$comp_obj, par = par, ghq_data = ghq_data,
    n_threads = n_threads, eps = eps, scale = scale, tol = tol, order = order)

  hessian <- cpp_res$hessian
  hessian[lower.tri(hessian)] <- t(hessian)[lower.tri(hessian)]
  meat <- cpp_res$meat
  res <- solve(hessian, t(solve(hessian, meat)))

  # construct the covariance matrix estimate on the original covariance matrix
  # scale
  jac <- diag(length(par))
  jac[object$indices$vcov_upper, object$indices$vcov_upper] <-
    jac_log_chol_inv(par[object$indices$vcov_upper])
  res_org <- tcrossprod(jac %*% res, jac)

  rownames(hessian) <- colnames(hessian) <-
    rownames(meat) <- colnames(meat) <-
    rownames(res) <- colnames(res) <-
    rownames(res_org) <- colnames(res_org) <-
    object$param_names$upper

  structure(res, meat = meat, hessian = hessian, `res vcov` = res_org)
}
