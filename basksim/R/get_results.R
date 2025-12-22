#' Get Results for Simulation of Basket Trial Designs
#'
#' @template design
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' get_results(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   epsilon = 2, tau = 0, iter = 100)
get_results <- function(design, ...) {
  UseMethod("get_results", design)
}

#' Get Results for Simulation of a Basket Trial with the BMA Design
#'
#' @template design_bma
#' @template n
#' @template p1
#' @template lambda
#' @template pmp0
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_bma(k = 3, p0 = 0.2)
#' get_results(design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   pmp0 = 1, iter = 100)
get_results.bma <- function(design, n, p1 = NULL, lambda, pmp0, iter = 1000,
                            data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind',
                   .options.future = list(seed = TRUE)) %dofuture% {
    res_temp <- suppressWarnings(bmabasket::bma(pi0 = design$p0, y = data[i, ],
      n = rep(n, design$k), pmp0 = pmp0, ...))
    ifelse(as.vector(res_temp$bmaProbs) > lambda, 1, 0)
  }
}

#' Get Results for Simulation of a Basket Trial with the MML Design
#'
#' @template design_mml
#' @template n
#' @template p1
#' @template lambda
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_mml(k = 3, p0 = 0.2)
#' get_results(design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   iter = 100)
get_results.mml <- function(design, n, p1 = NULL, lambda, iter = 1000,
                            data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  weights <- get_weights_mml(design, n, ...)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind') %dofuture% {
    ana_pp(design = design, n = n, r = data[i, ], lambda = lambda,
      weights = weights)
  }
}

#' Get Results for Simulation of a Basket Trial with the Global MML Design
#'
#' @template design_mmlglobal
#' @template n
#' @template p1
#' @template lambda
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_mmlglobal(k = 3, p0 = 0.2)
#' get_results(design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   iter = 100)
get_results.mmlglobal <- function(design, n, p1 = NULL, lambda, iter = 1000,
                                  data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind',
                   .options.future = list(seed = TRUE)) %dofuture% {
    ana_mmlglobal(design = design, n = n, r = data[i, ], lambda = lambda)
  }
}

#' Get Results for Simulation of a Basket Trial with the BHM Design
#'
#' @template design_bhm
#' @template n
#' @template p1
#' @template lambda
#' @template tau_bhm
#' @template iter
#' @template n_mcmc
#' @template data_bhm
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
#' \donttest{get_results(design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tau_scale = 1, iter = 100)}
get_results.bhm <- function(design, n, p1 = NULL, lambda, tau_scale,
                            iter = 1000, n_mcmc = 10000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
    iter = iter)

  analyses <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = data,
    evidence_levels = lambda,
    method_names = "berry",
    target_rates = rep(design$p_target, design$k),
    prior_parameters_list = bhmbasket::setPriorParametersBerry(
      mu_mean = design$mu_mean,
      mu_sd = design$mu_sd,
      tau_scale = tau_scale
    ) ,
    n_mcmc_iterations = n_mcmc
  ))

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
    collapse = ", "), ")")
  res <- bhmbasket::getGoDecisions(
    analyses_list = analyses,
    cohort_names = paste("p", 1:design$k, sep = "_"),
    evidence_levels = rep(lambda, design$k),
    boundary_rules = str2lang(br),
    ...
  )
  res$scenario_1$decisions_list$berry[, -1]
}

#' Get Results for Simulation of a Basket Trial with the EXNEX Design
#'
#' @template design_exnex
#' @template n
#' @template p1
#' @template lambda
#' @template tau_exnex
#' @template w_exnex
#' @template iter
#' @template n_mcmc
#' @template data_bhm
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_exnex(k = 3, p0 = 0.2)
#' \donttest{get_results(design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tau_scale = 1, w = 0.5, iter = 100)}
get_results.exnex <- function(design, n, p1 = NULL, lambda, tau_scale, w,
                              iter = 1000, n_mcmc = 10000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
    iter = iter)

  analyses <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = data,
    evidence_levels = lambda,
    method_names = "exnex",
    prior_parameters_list = bhmbasket::setPriorParametersExNex(
      mu_mean = design$mu_mean,
      mu_sd = design$mu_sd,
      tau_scale = tau_scale,
      mu_j = rep(design$basket_mean, design$k),
      tau_j = rep(design$basket_sd, design$k),
      w_j = w
    ),
    n_mcmc_iterations = n_mcmc
  ))

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
    collapse = ", "), ")")
  res <- bhmbasket::getGoDecisions(
    analyses_list = analyses,
    cohort_names = paste("p", 1:design$k, sep = "_"),
    evidence_levels = rep(lambda, design$k),
    boundary_rules = str2lang(br)
  )

  res$scenario_1$decisions_list$exnex[, -1]
}

#' Get Results for Simulation of a Basket Trial with Fujikawa's Design
#'
#' @template design_fujikawa
#' @template n
#' @template p1
#' @template lambda
#' @template tuning_fujikawa
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' get_results(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   epsilon = 2, tau = 0, iter = 100)
get_results.fujikawa <- function(design, n, p1 = NULL, lambda, epsilon, tau,
                                 logbase = 2, iter = 1000, data = NULL,
                                 ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  weights <- get_weights_jsd(design = design, n = n, epsilon = epsilon,
    tau = tau, logbase = logbase)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind') %dofuture% {
    ana_fujikawa(design = design, n = n, r = data[i, ], lambda = lambda,
      weights = weights)
  }
}

#' Get Results for Simulation of a Basket Trial with the Power Prior Design
#' Based on Global JSD Weights
#'
#' @template design_jsdglobal
#' @template n
#' @template p1
#' @template lambda
#' @template tuning_jsdglobal
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_jsdglobal(k = 3, p0 = 0.2)
#' get_results(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   eps_pair = 2, eps_all = 2, iter = 100)
get_results.jsdglobal <- function(design, n, p1 = NULL, lambda, eps_pair,
                                  tau = 0, eps_all, logbase = 2, iter = 1000,
                                  data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  weights_pair <- get_weights_jsd(design = design, n = n, epsilon = eps_pair,
    tau = tau, logbase = logbase)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind') %dofuture% {
    ana_jsdglobal(design = design, n = n, r = data[i, ], eps_all = eps_all,
      lambda = lambda, weights_pair = weights_pair)
  }
}

#' Get Results for Simulation of a Basket Trial with a Calibrated Power Prior
#' Design
#'
#' @template design_cpp
#' @template n
#' @template p1
#' @template lambda
#' @template tuning_cpp
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_cpp(k = 3, p0 = 0.2)
#' get_results(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tune_a = 1, tune_b = 1, iter = 100)
get_results.cpp <- function(design, n, p1 = NULL, lambda, tune_a, tune_b,
                            iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  weights <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind') %dofuture% {
    ana_pp(design = design, n = n, r = data[i, ], lambda = lambda,
      weights = weights)
  }
}

#' Get Results for Simulation of a Basket Trial with a Global Calibrated
#' Power Prior Design
#'
#' @template design_cppglobal
#' @template n
#' @template p1
#' @template lambda
#' @template tuning_cppglobal
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A matrix of results with \code{iter} rows. A 0 means, that the
#' null hypothesis that the response probability exceeds \code{p0} was not
#' rejected, a 1 means, that the null hypothesis was rejected.
#' @export
#'
#' @examples
#' design <- setup_cppglobal(k = 3, p0 = 0.2)
#' get_results(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tune_a = 1, tune_b = 1, epsilon = 2, iter = 100)
get_results.cppglobal <- function(design, n, p1 = NULL, lambda, tune_a, tune_b,
                                  epsilon, iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  weights_pair <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  foreach::foreach(i = 1:nrow(data), .combine = 'rbind') %dofuture% {
    ana_cppglobal(design = design, n = n, r = data[i, ], lambda = lambda,
      weights_pair = weights_pair, epsilon = epsilon)
  }
}

